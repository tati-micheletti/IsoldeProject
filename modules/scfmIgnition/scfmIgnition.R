# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "scfmIgnition",
  description = "start fires on landscape according to a given probability (map)",
  keywords = c("fire ignition van_Wagner"),
  authors = c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.0.9002"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "scfmIgnition.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    defineParameter("pIgnition", "numeric", 0.001, 0, 1, desc="per cell and time ignition probability"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc="interval between main events"),
    defineParameter("startTime", "numeric", 0, NA, NA, desc="Simulation time at which to initiate ignitions"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "scfmPars", objectClass = "list", desc = "unified paramater list", sourceURL = NA),
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "map of flammable cells", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "ignitionLoci",  objectClass = "numeric", desc = "indices of ignited cells", sourceURL = NA)
  )
))

doEvent.scfmIgnition = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$startTime, "scfmIgnition", "ignite")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "scfmIgnition", "plot")
    },
    plot = {
      sim <- scheduleEvent(sim, time(sim) + params(sim)$scfmIgnition$.plotInterval, "scfmIgnition", "plot")
    },
    ignite = {
      sim = Ignite(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "scfmIgnition", "ignite")
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


Init <- function(sim) {
  if (!("flammableMap" %in% names(objs(sim)))){
      stop("need to give me something!")
  }
  sim$ignitionLoci <- numeric(0)
  return(invisible(sim))
 }

Ignite <- function(sim) {
  #browser()
  pIg <- if ("scfmPars" %in% names(objs(sim)))  #if either of these is a map, it needs to have NAs in the right place
                                                #and be conformant with flammableMap
           sim$scfmPars$pIgnition
         else
           P(sim)$pIgnition
  
  sim$ignitionLoci <- numeric(0)  #ensure always in a determinate state
  tmp <- which(runif(raster::ncell(sim$flammableMap)) < pIg)
  if (length(tmp) > 0){
    tmp <- tmp[which(sim$flammableMap[tmp]==0)] #restrict to flammable cells (necessary if pIg constant)
    if (length(tmp) > 0)
      sim$ignitionLoci <- tmp 
  }
  return(invisible(sim))
}


