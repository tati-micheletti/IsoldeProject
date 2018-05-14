
# Everything in this file gets sourced during siminit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "Sorensen",
  description = "re-impliments the Sorensen (2007) model of caribou population size in response to a fire regime",
  keywords = c("caribou", "population", "fire regime", "habitat"), 
  authors = person("Isolde", "Lane Shaw", email = "r.i.lane.shaw@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1", Sorensen = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib", "Sorensen et al. (2007)"),
  documentation = list("README.txt", "Sorensen.Rmd"),
  reqdPkgs = list("raster", "SpaDES"), 
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("N0", "numeric", 187, 0, NA, "Number of Caribou at time 0"),
    defineParameter("minHabitatAge", "numeric", 50, 0, 300, "The number of years following fire disturbance before the area becomes suitable caribou habitat")
  ),
  
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "a raster map of the ages of the vegetation in each cell/the number of years since an area burned", sourceURL = NA),
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", desc = "which pixels can burn: coded 0", sourceURL = NA),
    expectsInput(objectName = "landscapeAttr", objectClass = "list", desc = "landscape parameters from scfmLandCoverinit via scfm{Regime|Driver}", sourceURL = NA)  ),
  
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput( objectName = "Nt", objectClass = "numeric", desc = "Number of caribou at timestep t"),
    createsOutput( objectName = "herdTable", objectClass = "dataFrame", desc = "a table showing herd size over time"))
  ))

## event types
#   - type `init` is required for initialization

doEvent.Sorensen = function(sim, eventTime, eventType, debug = TRUE) {
  switch(
    eventType,
    init = { 
      sim <- init(sim)
      sim <- scheduleEvent(sim, start(sim), "Sorensen", "SorensenE")
    },
    
    SorensenE = {
     
      # do stuff for this event
      sim <- SorensenE(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "Sorensen", "SorensenE")
            },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  
  return(invisible(sim))
  
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameinit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
init = function(sim) {
  
  #create the herdTable with two columns, named Lt and Nt
  sim$herdTable <- data.frame(matrix(ncol = 2, nrow = 0))
  columntitlesCaribou <- c("Lt", "Nt")
  colnames(sim$herdTable) <- columntitlesCaribou
  
  sim$Nt = P(sim)$N0
  #sim$SorensenStats <-data.frame(list(Lt=double(),Nt=double()))
  #Then add row showing the initial caribou numbers
  addCaribouRow <- c( 0, sim$Nt)
  sim$herdTable[nrow(sim$herdTable) +1, ] <- addCaribouRow
  
  return(invisible(sim))
}

### SorensenE event
SorensenE <- function(sim) {

  #first, we want to calculate %BURN
  burn <- which(sim$disturbanceMap[]==1)
  BURN <- (length(burn)/sum(table(sim$disturbanceMap[])))*100
  
  #Then we calculate %IND
  cut <- which(sim$disturbanceMap[]==2)
  adjcut <- which(sim$disturbanceMap[]==3)
  pcut <- (length(cut)/sum(table(sim$disturbanceMap[])))*100
  padj <- (length(adjcut)/sum(table(sim$disturbanceMap[])))*100
  
  IND <- padj+pcut
  
  #then we want to work out Lt
  sim$Lt <- 1.19 - (0.0032*IND) - (0.0030*BURN)
  
  #then work out new Nt from Lt
  sim$Nt <- sim$Nt * sim$Lt
  
  #then we need to input Lt and Nt into the table
  addCaribouRow <- c( sim$Lt, sim$Nt)
  sim$herdTable[nrow(sim$herdTable) +1, ] <- addCaribouRow
  return(invisible(sim))
}

#.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can use 'sim$.userSuppliedObjNames' in their function below to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call. e.g.,
  # if (!('defaultColor' %in% sim$.userSuppliedObjNames)) {
  #  sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  
  #we want to create Nt (N0) as a named object inside the sim environment
  #  sim$Nt <- P(sim)$N0
  
  # ! ----- STOP EDITING ----- ! #
  
#  return(invisible(sim))
#}

