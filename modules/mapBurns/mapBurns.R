defineModule(sim, list(
  name = "mapBurns",
  description = "link results of scfmSpread and friends with ageModule", 
  keywords = c("burn", "interface"),
  authors = person("Steven", "Cumming", email = "stevec@sbf.ulaval.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9005", mapBurns = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "mapBurns.Rmd"),
  reqdPkgs = list("data.table", "raster"),
  parameters = rbind(
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc="Time interval between aging aevents"),
    defineParameter("startTime", "numeric", 0, NA, NA, desc="Simulation time at which to initiate aging")
  ),
  inputObjects = bind_rows(
    #only the first input is required in the intended application
    expectsInput(objectName = "spreadState", objectClass = "data.table", desc = "state of fire spread from scfmSpread"), 
    expectsInput(objectName = "spreadStateE", objectClass = "data.table", desc = "state of fire spread from scfmEscape"),
    expectsInput(objectName = "ignitionLoci", objectClass = "numeric", desc = "indices of ignited cells"),
    expectsInput(objectName = "ageMap", objectClass = "numeric", desc = "indices of ignited cells")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "ageMap", objectClass = "RasterLayer", desc = "updates ageMap")
  )
))


doEvent.mapBurns = function(sim, eventTime, eventType, debug=FALSE) {
  switch(
    eventType,
    init = {
      #note that no Init() is needed because this module maintains no state
      sim <- scheduleEvent(sim, P(sim)$startTime, "mapBurns", "mark")
    },
    mark  = {
     sim <- Mark(sim)
     sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "mapBurns", "mark")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Mark <- function(sim){
  #this logic is not minimal, but should mark all burned cells
  #no matter which combination of the three scfmIgnite, scfmEscpae, and scfnSpread modules are active. 
  #in the inteneded application, this would work fine. 
  #values(sim$ageMap)[sim$spreadState[,indices]] <- 0
  #browser()
  if ("spreadState" %in% names(objs(sim)) && nrow(sim$spreadState) > 0)
    raster::values(sim$ageMap)[sim$spreadState[,indices]] <- 0
  else if ("spreadStateE" %in% names(objs(sim)) && nrow(sim$spreadStateE) > 0)
    raster::values(sim$ageMap)[sim$spreadStateE[,indices]] <- 0
  
  if ("ignitionLoci" %in% names(objs(sim)) && length(sim$ignitionLoci) > 0)
    raster::values(sim$ageMap)[sim$ignitionLoci] <- 0
  return(invisible(sim))
}

