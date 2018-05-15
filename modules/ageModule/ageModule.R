defineModule(sim, list(
  name = "ageModule",
  description = "Creates and maintains a raster called ageMap",
  keywords = c("forest age", "modelling course", "Lab 5"),
  authors = c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.9.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "ageModule.Rmd"),
  reqdPkgs = list("raster","RColorBrewer"),
  parameters = rbind(
    defineParameter("initialAge", "numeric", 99.0, 0, 1e4, desc="initial age"),
    defineParameter("maxAge","numeric", 250, 0, 2**16-1, desc="maximum age for plotting"),
    defineParameter("returnInterval", "numeric", 1, NA, NA, desc="Time interval between aging aevents"),
    defineParameter("startTime", "numeric", 0, NA, NA, desc="Simulation time at which to initiate aging"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc="This describes the simulation time at which the first plot event should occur")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName="flammableMap", objectClass = "RasterLayer", desc="Template map"),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "ageMap to update")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "ageMap",  objectClass = "RasterLayer", desc="Duh")
  )
))

doEvent.ageModule = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$startTime, "ageModule", "age")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "ageModule", "plot")
    },
    age = {
      sim <- Age(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "ageModule", "age")
    },
    plot = {
      Plot(sim$ageMap, legendRange=c(0,P(sim)$maxAge))
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,"ageModule", "plot")
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  if (!("ageMap" %in% names(objs(sim)))){
    sim$ageMap <- sim$flammableMap    #this, on the other hand, had better exist
    sim$ageMap[] <- P(sim)$initialAge 
  }
  # we will use our colour choices, not whatever may have come with the loaded map.
  sim$ageMap <- setColors(sim$ageMap,n=10,colorRampPalette(c("LightGreen", "DarkGreen"))(10))
  return(invisible(sim))
}

Age <- function(sim){
  oldest = if (is.na(P(sim)$maxAge)) 2^31-1 else P(sim)$maxAge
  sim$ageMap <- setValues(sim$ageMap, pmin(oldest, 
                                      getValues(sim$ageMap)+P(sim)$returnInterval))
  return(invisible(sim))
}


.inputObjects <- function(sim){
  
  
  return(invisible(sim))
}
