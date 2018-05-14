defineModule(sim,  list(
  name = "harvest",
  description = "Simulates block level forest harvest scheduling to reach AAC targets.",
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
    defineParameter("minVolume", "numeric", 50, 0, NA, "Minimum volume in m^3/ha for eligibility to harvest"),
    defineParameter("minAge", "numeric", 40, 0, NA, "Minimum age to harvest"),
    defineParameter("startTime", "numeric", 0, 0, NA, "Start time"),
    defineParameter("returnInterval", "numeric", 1, 0, 0, "waddya think?"),
    defineParameter("minBlockSize", "numeric", 20, 1, NA, "minimum block size (ha)"),
    defineParameter("greenUpPeriod", "numeric", 30, 0, 30, "For how many years do cut blocks affect adjacent areas?"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, 
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "first time to save"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "first time to save")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "forest Age structure" ),
    expectsInput(objectName = "yieldTables", objectClass = "list", desc = "what it is"),
    expectsInput(objectName = "strataMap", objectClass = "Rasterayer", desc = "map assigning cells to yield tables"),
    expectsInput(objectName = "landscapeAttr", objectClass = "list", 
                 desc = "info about landscape we are running on; e.g. cellSize"),
    expectsInput(objectName = "annualCut", objectClass = "list", desc = "m^3/yr per yield Class")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "harvestStateMap", objectClass = "RasterLayer", desc = "areas locked up by spatial constraints"),
    createsOutput(objectName = "volMap", objectClass = "RasterLayer", desc = "volume denisty per cell"),
    createsOutput(objectName = "harvestStats", objectClass = "data.frame" , desc = "record of harvest activity by strata"),
    createsOutput(objectName = "cutCells", objectClass = "numeric", desc = "vector of cells harvested in current period")
  )
))



## event types
#   - type `init` is required for initialiazation

doEvent.harvest = function(sim, eventTime, eventType) {
  switch(eventType,
     init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$startTime,"harvest","harvest")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "harvest", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "harvest", "save", SpaDES.core::.lowest())
    },
    plot = {
      #browser()
      Plot(sim$harvestStateMap, legendRange=0:1)
      #Plot(sim$disturbanceMap, legendRange=0:3,zero.color="white")
      sim <- scheduleEvent(sim, time(sim)+P(sim)$.plotInterval, "harvest", "plot")
    },
    save = {
      Save(sim)
      #Plot(sim$disturbanceMap, legendRange=0:3,zero.color="white")
      sim <- scheduleEvent(sim, time(sim)+P(sim)$.saveInterval, "harvest", "save", SpaDES.core::.lowest())
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

  sim$harvestStats<-data.frame(rep(list(numeric(0)),length(sim$yieldTables[[1]]))) #again, use the 1st and only region
  
  #sim$disturbanceMap<-raster(sim$strataMap) * 0 
  #setColors(sim$disturbanceMap, n=3) <- c("white", "blue","red")
  
  
  sim$harvestStateMap<-raster::raster(sim$strataMap) 
  sim$harvestStateMap[] <- sim$strataMap[]*0
  setColors(sim$harvestStateMap, n=10) <- c("white","black")
  
  sim$volMap <- raster::raster(sim$strataMap) 
  sim$volMap[] <- sim$strataMap[]*0
  sim$merchCells <- list()    #of cells currently in the list for each stratum
  sim$cutCells <- numeric()

  return(invisible(sim))
}


updateState <- function(sim){ #calcuate merchantable volume by strata
  
 # browser()
  sim$cutCells <- numeric(0)
  sim$volMap[] <- sim$volMap[]*0
  ytList <- sim$yieldTables[[1]]
  #age the previous cuts
  idx <- which(sim$harvestStateMap[]>0)
  ages <- sim$harvestStateMap[idx] - 1
  sim$harvestStateMap[idx] <- ages
  
  for (i in 1:length(ytList)){
    idx <- which(sim$strataMap[] == i)
    isOK <- which(sim$harvestStateMap[idx] == 0)
    idx <- idx[isOK]
    ageVec <- sim$ageMap[idx]
    isOK <- which(ageVec >= P(sim)$minAge)
    ageVec <- ageVec[isOK]
    idx <- idx[isOK]
    #right truncate ageVec to yieldTableMaxAge
    maxAge <- dim(ytList[[i]])[1]
    ageVec <- ifelse(ageVec > maxAge, maxAge, ageVec)
    volVec <- ytList[[i]][ageVec,3]
    isOK <- which(volVec >= P(sim)$minVolume)
    volVec <- volVec[isOK]
    idx <- idx[isOK]
    sim$volMap[idx] <- volVec
    
    ages <- sim$ageMap[idx]
    if (length(ages) > 0)
      x <- order(ages, decreasing=TRUE)
    else
      x <- integer(0)
    idx <-idx[x]     #put in oldest first
    sim$merchCells[[i]] <- idx
  }
  sim$cutCellIndex <- rep(1,length(ytList))
  return(invisible(sim))  
}

testBlock <-function(cells, strata){
  
  
  
}

Harvest <- function(sim){
  sim <- updateState(sim)
  #browser()
  aac <- unlist(sim$annualCut) #level for region missing
  vol <- rep(0, length(aac))
  maxCells <- unlist(lapply(sim$merchCells,length))
  nStrata <- length(maxCells)
  while (any(vol < aac & sim$cutCellIndex <= maxCells)){
    for (i in 1:nStrata){
      idx <- sim$cutCellIndex[i]
      if (vol[i] < aac[i] && idx <= maxCells[i]){ #need more and there is potentially some
         while (sim$harvestStateMap[sim$merchCells[[i]][idx]] != 0){
           idx = idx + 1
           if (idx > maxCells[i])
             break
         }
         if (idx <= maxCells[i]){ #could we find a cell to cut? 
           #browser()
           cellX <- sim$merchCells[[i]][idx]
           #for a simple block simulator, get adjacent cells
           adjX  <- adjacent(sim$volMap, cellX, directions=8, pairs=FALSE, include=TRUE)
           #select those in same strata, that are merch and not blocked. 
           isOK <- which(sim$strataMap[adjX] == i & sim$volMap[adjX] > 0 & sim$harvestStateMap[adjX] == 0)
           adjX <- adjX[isOK]
           #if the block is big enough, cut them all and mark them
           if (length(adjX) * sim$landscapeAttr$cellSize >= P(sim)$minBlockSize){
             vol[i] <- vol[i] +  sum(sim$volMap[adjX]) * sim$landscapeAttr$cellSize #then get its Volume/ha
             sim$cutCells <- c(sim$cutCells, adjX)
             #should mark all cells adjacent also. 
             adjX2 <- adjacent(sim$volMap, adjX, directions=4, pairs=FALSE, include=TRUE)
             sim$harvestStateMap[adjX2] <- P(sim)$greenUpPeriod #mark it cut
           }
         }
        sim$cutCellIndex[i] <- idx + 1 # this now does not ensure that something is cut every step.
      }
    }
  }
  #sim<-sequentialCut(sim)
  #sim<-sequenceOldest(sim)
  #sim<-blockOldest(sim)
  #browser()
  sim$harvestStats[nrow(sim$harvestStats)+1,] <- vol #area, vol, vol/area; where to find these^
  
  return(invisible(sim))
  
}


Save <- function(sim){
  fname <- file.path(outputPath(sim), "harvestStats.csv")
  write.table(sim$harvestStats,fname, dec=".", sep=",", 
              col.names=FALSE, row.names=FALSE)       #the vol vector in Harvest() does not inherit the 
                                                      #the strata names; it should.
  return(invisible(sim))
}

