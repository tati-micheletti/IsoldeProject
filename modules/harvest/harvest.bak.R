defineModule(sim, list(
  name = "harvest",
  description = "Simulates block level forest harvest scheduling to reach AAC targets."),
  keywords = c("harvesting", "cut block", "adjacency constraints"),
  authors = c(person(c("Steven", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9006"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "harvest.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("minVolume", "numeric", 45, 0, NA, "Minimum volume in m^3/ha for eligibility to harvest"),
    defineParameter("minAge", "numeric", 50, 0, NA, "Minimum age to harvest"),
    defineParameter("startTime", "numeric", 0, 0, NA, "Start time"),
    defineParameter("returnInterval", "numeric", 1, 0, 0, "waddya think?"),
    defineParameter("minBlockSize", "numeric", 20, 1, NA, "minimum block size (ha)"),
    defineParameter("cutPersistanceTime", "numeric", 30, 0, 30, "For how many years do cut blocks affect adjacent areas?"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, 
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "forest Age structure" ),
    expectsInput(objectName = "yieldTables", objectClass = "list", desc = "what it is"),
    expectsInput(objectName = "strataMap", objectClass = "Rasterayer", desc = "map assigning cells to yield tables"),
    expectsInput(objectName = "landscapeAttr", objectClass = "list", 
                 desc = "info about landscape we are running on; e.g. cellSize"),
    expectsInput(objectName = "annualCut", objectClass = "list", desc = "m^3/yr per yield Class"),
    expectsInput(objectName = "ignitionLoci", objectClass = "numeric", desc = "indices ofburned cells")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "harvestStateMap", objectClass = "RasterLayer", desc = "areas locked up by spatial constraints"),
    createsOutput(objectName = "harvestState", objectClass = "data.frame" , desc = "record of harvest activity by strata")
  )
)

## event types
#   - type `init` is required for initialiazation

doEvent.harvest = function(sim, eventTime, eventType, debug = FALSE) {
  switch(evenType,
     init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$startTime,"harvest","harvest")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "harvest", "plot")
    },
    plot = {
      Plot(sim$harvestStateMap, legendRange=0:1)
      #Plot(sim$disturbanceMap, legendRange=0:3,zero.color="white")
      sim <- scheduleEvent(sim, time(sim)+P(sim)$.plotInterval, "harvest", "plot")
    },
    harvest = {
      sim <- Harvest(sim) 
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval,"harvest","harvest")
   }, 
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}



Init <- function(sim) {
  
  sim$harvestState<-list(idx=1,volCut=0)
  sim$harvestStats<-data.frame(list(area=numeric(0),volume=numeric(0),vPerHa=numeric(0)))
  
  sim$disturbanceMap<-raster(sim$strataMap) * 0 
  setColors(sim$disturbanceMap, n=3) <- c("white", "blue","red")
  
  sim$harvestStateMap<-raster(sim$strataMap) * 0
  setColors(sim$harvestStateMap, n=2) <- c("white","black")
  
  sim$volMap <- raster(sim$strataMap) * 0
  sim$merchCells <- list()    #of cells currently in the list for each stratum

  return(invisible(sim))
}


updateState <- function(sim){ #calcuate merchantable volume by strata
  
  sim$volMap[] <- sim$volMap[]*0
  ytList <- sim$yieldTables[[1]]
  
  for (i in 1:length(ytList)){
    idx <- which(sim$strataMap[] == i)
    isOK <- which(sim$harvestStateMap[idx] == 0)
    idx <- idx[isOK]
    ageVec <- sim$ageMap[idx]
    isOK <- which(ageVec >= P(sim)$minAge)
    ageVec <- ageVec[isOK]
    idx <- idx[isOKc]
    volVec <- ytList[[i]][ageVec,3]
    isOK <- which(volVec >= P(sim)$minVolume)
    volVec <- volVec[isOK]
    idx <- idx[isOK]
    sim$volMap[idx] <- volVec
    
    ages <- sim$ageMap[idx]
    x <- order(ages, decreasing=TRUE)
    idx <-idx[x]     #put in oldest first
    sim$merchCells[[i]] = length(idx)
  }
  sim$cutCellIndex <- rep(1,length(ytList))

  return(invisible(sim))  
}


updateHarvestStateMap<-function(sim){
  browser()
  sim$noCanGo[] <- 0
  minAge <- params(sim)$harvest$minAge
  if (!is.na(minAge) && minAge>0){
    bad <- which(sim$ageMap[] < minAge)
    sim$noCanGo[bad]<-1
  }
  minVolume <-  params(sim)$harvest$minVolume  #this next chunk would have to be yield curve specific 
  if (!is.na(minVolume) && minVolume>0){
    #at some point, need to make this object orientd wif methods
    v<-sim$yieldTables[[1]] #assume there is at least 1, and take the 1st for now
    n<-length(v)
    x<-sim$ageMap[]  #this solves the problem of 0 ages
    x<-ifelse(x==0,1,x)
    x<-ifelse(x<n,x,n)
    x<-v[x]
    bad <- which(x < minVolume)
    sim$noCanGo[bad]<-1
  }
  
  return(invisible(sim))
}


Harvest <- function(sim){
  
  sim <- updateVolMap(sim)
  
  sim$harvestState$volCut <- 0.0
  
  aac <- unlist(sim$annualCut) #level for region missing
  vol <- rep(0, length(aac))
  maxCells <- unlist(sim$merchCells)
  nStrata <- length(maxCells)
  while (any(vol < aac & sim$curCellIndex <= maxCells)){
    for (i in 1:nStrata){
      idx <- sim$curCellIndex[i]
      if (vol[i] < aac[i] && idx <= maxCells[i]){ #need more and there is potentially some
         while (sim$harvestStateMap[sim$merchCells[i][idx]] != 0){
           idx = idx + 1
           if (idx > maxCells[i])
             break
         }
         if (idx <= maxCells[i]){ #calculate the volume
           vc = sim$volMap[sim$merchCells[i][idx]] * sim$landscapeAttr$cellSize
           sim$harvestStateMap[sim$merchCells[i][idx]] <- 1
           sim$curCellIndex[i] <- idx+1
         }
      }
    }
  }
  #sim<-sequentialCut(sim)
  #sim<-sequenceOldest(sim)
  #sim<-blockOldest(sim)
  sim$harvestStats[nrow(sim$harvestStats+1),] <- vol #area, vol, vol/area; where to find these^
  
  return(invisible(sim))
  
}

blockOldest <- function(sim){
  x <- order(sim$ageMap[], decreasing=TRUE)
  x <- x[which(sim$noCanGo[x]==0)] 
  v<-sim$yieldTables[[1]] #assume there is at least 1, and take the 1st for now
  n<-length(v)
  idx <- 1
  while (sim$harvestState$volCut < sim$annualCut ){
    cell <- x[idx]
    if (sim$noCanGo[cell] == 0){
     # browser()
      # 9*6.25 = 56.25ha, an acceptable max block size for now (60ha is sometimes used in Alberta)
      block <- adjacent(sim$ageMap, cell, directions=8, pairs=FALSE, include=TRUE)
      operable <- sim$noCanGo[block]
      block <- block[!operable]   #length of block >= 1
      #other constraints on blocks: age range, type mix, adjacency.
      if (length(block)*sim$cellSize > params(sim)$harvest$minBlockSize){
        age <- sim$ageMap[block]
        age <- ifelse(age < n, age, n)
        cut <- sum(v[age]) * sim$cellSize
        sim$harvestState$volCut <- sim$harvestState$volCut + cut
        sim$ageMap[block] <- 0
        sim$disturbanceMap[block] <- 2
      }
    }
    idx <- idx + 1
    if (idx > length(x)){
      warning("ran out of cells")
      break
    }
  }
return(invisible(sim))  
}



sequenceOldest <- function(sim){
  x <- order(sim$ageMap[], decreasing=TRUE)
  x <- x[which(sim$noCanGo[x]==0)] 
  v<-sim$yieldTables[[1]] #assume there is at least 1, and take the 1st for now
  n<-length(v)
  idx <- 1
  while (sim$harvestState$volCut < sim$annualCut ){
    cell <- x[idx]
    age <- sim$ageMap[cell]
    age <- ifelse(age < n, age, n)
    cut <- v[age] * sim$cellSize
    sim$harvestState$volCut <- sim$harvestState$volCut + cut
    sim$ageMap[cell] <- 0
    sim$disturbanceMap[cell] <- 2
    idx <- idx + 1
    if (idx > length(x)){
      warning("ran out of cells")
      break
    }
  }
  return(invisible(sim))  
}

sequentialCut<-function(sim){
  
  N <- prod(dim(sim$noCanGo)[1:2])
  idx0 <- sim$harvestState$idx
  idx <-idx0
  v<-sim$yieldTables[[1]] #assume there is at least 1, and take the 1st for now
  n<-length(v)
  while (sim$harvestState$volCut < sim$annualCut ){
    
    if (sim$noCanGo[idx]==0){
      age<-sim$ageMap[idx]
      age<-min(age,n)
      sim$harvestState$volCut <- sim$harvestState$volCut + (v[age] * sim$cellSize)
      sim$disturbanceMap[idx] <- 2  #fix the magic numbers
      sim$ageMap[idx] <- 0
    }
    
    idx <- ifelse(idx==N,1,idx+1)
    if (idx == idx0){
      warning("ran out of harvestable cells")?
      break
    }
  }
  sim$harvestState$idx <- idx
  return(invisible(sim))
}

