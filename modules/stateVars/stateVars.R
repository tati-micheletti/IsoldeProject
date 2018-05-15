defineModule(sim, list(
  name = "stateVars",
  description = "keep track of stat transitions affecting multiple moduls; also classification",
  keywords = c("insert key words here"),
  authors = c(person(c("First", "Middle"), "Last", email="email@example.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9006"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit =  "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "stateVars.Rmd"),
  reqdPkgs = list("raster", "data.table", "RColorBrewer"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("startTime", "numeric", 0, 0, NA, "Start time"),
    defineParameter("returnInterval", "numeric", 1, 0, NA, "waddya think?"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter("persistTimes", "numeric", c(40,30,30), c(0,0,0), c(100,100,100), desc=  "For how many years do disturbances effect indicators?")
    
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "disturbanceMap", objectClass = "RasterLayer", desc = "state of burned or harvestd cells"),
    expectsInput(objectName = "spreadState", objectClass = "data.table", desc = "table of active initial spread cells"),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "time since last disturbance"),
    expectsInput(objectName = "cutCells", objectClass = "numeric", desc = "vector of recently cut cells")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "heightMap", objectClass = "RasterLayer", desc = "crude height from age model"),
    createsOutput(objectName = "disturbanceMap", objectClass = "RasterLayer", desc = "update for caribou adjacency"),
    createsOutput(objectName = "dtMap", objectClass = "RasterLayer", desc = "timer for disturbances")
  )
))


doEvent.stateVars = function(sim, eventTime, eventType) {
  switch (eventType,
  init = {
    sim <- Init(sim)
    sim <- scheduleEvent(sim, P(sim)$startTime,"stateVars","update")
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,"stateVars","plot")
  },
  update = {
    Update(sim)
    sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "stateVars", "update")
  },
  plot = {
    Plot(sim$disturbanceMap)
    sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "stateVars", "plot")
  },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

heightFromAge <-function(sim){
  x <- sim$ageMap[]/ (sim$ageMap[] + 100)
  sim$heightMap[] <- 80*x
  return(invisible(sim))
}

Init <- function(sim) {
  #use disturbanceMap as template to copy.
  #browser()
  sim$disturbanceMap <- raster::raster(sim$ageMap) 
  sim$disturbanceMap[] <- sim$ageMap[] * 0
  sim$dtMap <-  raster::raster(sim$ageMap)
  sim$dtMap[] <-  sim$ageMap[] * 0
  
  #raster::setValues(sim$harvestStateMap, values=0) The reason CS people fucking HATE R 
  #is because of random non-orthogonality like this, and the documentation that tries to be 
  #like UNIX, but fails. We shoulda gone with Python.
  setColors(sim$disturbanceMap,n=4) <- c("grey80", "red", "blue", "yellow") 
  #0 = none
  #1 = burn
  #2 = cut
  #3 = adjacent to cut
  
  sim$heightMap <- raster::raster(sim$disturbanceMap)
  sim$heightMap[] <- sim$disturbanceMap[] * 0
  setColors(sim$heightMap, n=10) <- colorRampPalette(c("white","green4"))(10)
  sim<-heightFromAge(sim)
  
  return(invisible(sim))
}


Update <- function(sim){
  #browser()
  #idx <- which(sim$harvestStateMap[] > 0)
  #vals <- sim$harvestStateMap[idx]
  #sim$harvestStateMap[idx] <- vals - 1 #let't not go all negative
  #x <- which(sim$disturbanceMap[] == 2) # 2 codes for harvest, 1 for fire; 3 could code for "adjacent to harvest"
  #Let's add this here
  #sim$harvestStateMap[x] <- P(sim)$cutPersistanceTime
  
  #browser()
  dtidx <- which(sim$dtMap[] > 0)
  dtval <- sim$dtMap[dtidx]
  dtval <- dtval - 1
  sim$dtMap[dtidx] <- dtval
  unMark <- dtidx[which(dtval == 0)]
  sim$disturbanceMap[unMark] <- 0
  
  
  if (is.data.table(sim$spreadState) && nrow(sim$spreadState) > 0){   #existant and non-empty?
    idx <- sim$spreadState[,indices]                                  #then scfmSpread will define "indices"     
    raster::values(sim$disturbanceMap)[idx] <- 1
    raster::values(sim$dtMap)[idx] <- P(sim)$persistTimes[1]
  }      
   
  if (is.numeric(sim$cutCells) && length(sim$cutCells) > 0){
     adjx <- raster::adjacent(sim$disturbanceMap,sim$cutCells,pairs=FALSE)
     raster::values(sim$disturbanceMap)[adjx] <- 3
     raster::values(sim$dtMap)[adjx] <- P(sim)$persistTimes[3]
     #do these after, because overlap when cutting blocks of cells.
     raster::values(sim$disturbanceMap)[sim$cutCells] <- 2
     raster::values(sim$dtMap)[sim$cutCells] <- P(sim)$persistTimes[2]
     #update ageMap
     raster::values(sim$ageMap)[sim$cutCells] <- 0
  }
  
  sim <- heightFromAge(sim)
  
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above

