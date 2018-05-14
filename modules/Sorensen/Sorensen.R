# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "Sorensen",
  description ="This module replicates the Sorensen paper ",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1", Sorensen = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Sorensen.Rmd"),
  reqdPkgs = list("igraph", "magrittr", "raster", "rgdal", "spatial.tools"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("N0", "numeric", 100, 0, 10000, "Initial Caribou population size"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", 0, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    #expectsInput(objectName="flammableMap", objectClass = "RasterLayer", desc="Template map"),
    #
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "ageMap to update"),
    expectsInput(objectName = "disturbanceMap", objectClass = "RasterLayer", desc = "state of burned or harvestd cells"),
    #expectsInput(objectName = "landscapeAttr", objectClass="list", desc="cellsize and total area of study area"),
    expectsInput(objectName = "N0", objectClass="numeric", desc="Initial Caribou Population Size")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "SorensenStats", objectClass ="data.frame", desc = "Lambda and herd size")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Sorensen = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, start(sim), "Sorensen", "SorensenE")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Sorensen", "save", SpaDES.core::.lowest())
      },
  
    SorensenE  = {
      sim <- SorensenE (sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "Sorensen", "SorensenE")
      },
    
    save = {
      save(sim)
      #Plot(sim$disturbanceMap, legendRange=0:3,zero.color="white")
      sim <- scheduleEvent(sim, time(sim)+P(sim)$.saveInterval, "Sorensen", "save", SpaDES.core::.lowest())
      },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


#Template for initizialitation
Init <- function(sim, initType, debug=FALSE){
  sim$Nt = P(sim)$N0
  sim$SorensenStats <-data.frame(list(Lambda=double(),Nt=double()))
  return(invisible(sim))
  }

SorensenE  <- function(sim){
  
  burn=which(sim$disturbanceMap[]==1)
  cut=which(sim$disturbanceMap[]==2)
  adjcut=which(sim$disturbanceMap[]==3)

  pBurn=(length(burn)/sum(table(sim$disturbanceMap[])))*100
  pcut=(length(cut)/sum(table(sim$disturbanceMap[])))*100
  padj=(length(adjcut)/sum(table(sim$disturbanceMap[])))*100
  
  pIND=padj+pcut
  
  lambda<-1.19 - (0.0032* pIND)- (0.0030 *pBurn)
  sim$Nt <- sim$Nt * lambda
  sim$SorensenStats[nrow(sim$SorensenStats)+1,]=c(lambda,sim$Nt)
  
  
  return(invisible(sim))
}
  
  save <- function(sim){
    fname <- file.path(outputPath(sim), "SorensenStats.csv")
    write.table(sim$SorensenStats,fname, dec=".", sep=",", 
                col.names=FALSE, row.names=FALSE)       #the vol vector in Harvest() does not inherit the 
    #the strata names; it should.
}

.inputObjects <- function(sim) {
  if (!('N0' %in% sim$.userSuppliedObjNames))
     sim$N0 <- P(sim)$N0
  
  return(invisible(sim))
  }
