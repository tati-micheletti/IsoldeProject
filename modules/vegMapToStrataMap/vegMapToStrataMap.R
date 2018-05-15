defineModule(sim, list(
  name = "vegMapToStrataMap",
  description = "assign yield tables (strata) to vegetation classes", 
  keywords = NA, # c("insert key words here"),
  authors = person("Isolde", "Lane Shaw", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9007", vegMapToStrataMap = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "vegMapToStrataMap.Rmd"),
  reqdPkgs = list("data.table", "raster",  "RandomFields", "magrittr", "spatial.tools", "rgdal", "ggplot2"),
  
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, desc = "duh"),
    defineParameter(".plotInterval", "numeric", 10, NA, NA, desc = "duh"),
    defineParameter("returnInterval", "numeric", 1, NA, NA, desc = "duh"),
    defineParameter("startTime", "numeric", 0, NA, NA, desc = "duh"),
    defineParameter("reclassificationTable", "character","reclassificationTableAlberta.csv", NA, NA, "transition matrix"),
    defineParameter(".statsInitialTime", "numeric", 0, NA_real_, NA_real_, "This describes the simulation time at which the first event should occur"),
    defineParameter(".statsInterval", "numeric", 1, NA_real_, NA_real_, "This describes the simulation time interval between events"),
    defineParameter("startTime", "numeric", 0, NA_real_, NA_real_, "if not NA, main event fire occurs at this time"),
    defineParameter("returnInterval", "numeric", 1, NA_real_, NA_real_, "time to next main event")
    ),
  
  inputObjects = bind_rows(
    expectsInput(objectName = "vegMap", objectClass = "RasterLayer", desc = "vegetation class map")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "strataMap", objectClass = "RasterLayer", desc = "yield curve indexes")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.vegMapToStrataMap = function(sim, eventTime, eventType, debug = TRUE) {
  switch(
    eventType,
    init = {
      sim <- init(sim)
      sim <- scheduleEvent(sim, P(sim)$startTime, "vegMapToStrataMap", "stratify")
      sim <- scheduleEvent(sim, P(sim)$.statsInitialTime, "vegMapToStrataMap", "statsInit")
      #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "vegMapToStrataMap", "plot")
    },
    
    stratify = {
      sim <- stratify(sim)
      #sim <- scheduleEvent(sim, P(sim)$startTime, "vegMapToStrataMap", "plot")
    },
    
    statsInit = {
      sim <- statsInit(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.statsInitialTime, "vegMapToStrataMap", "stats")
    },
    
    stats = {
      sim <- stats(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.statsInterval, "vegMapToStrataMap", "stats")
    },
    
    plot = {
       Plot(sim$strataMap)
       scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "vegMapToStrataMap", "plot")
    },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


init <- function(sim) {
 
  sim$strataMap <- raster::raster(sim$vegMap) 
  sim$strataMap[] <- sim$vegMap[] * 0
  
  return(invisible(sim))
}

stratify <- function(sim){

  fp <- file.path(modulePath(sim), "vegMapToStrataMap","data", P(sim)$reclassificationTable)
  sTab <- read.csv(fp, header = TRUE, colClasses = "numeric")
  sim$strataMap <-reclassify(sim$vegMap, sTab)
  
  return(invisible(sim))
}

statsInit <- function(sim){
  
  sim$strataMapStats  <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(sim$strataMapStats) <- as.factor(1:8)
    return(invisible(sim))
}

stats <- function(sim){
  
  tmpVec <- sim$annualCut
  sim$strataMapStats[nrow(sim$strataMapStats) + 1, ] <- tmpVec
  return(invisible(sim))
}