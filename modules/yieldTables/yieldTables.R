
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "yieldTables",
  description = "load and pre-processes stand Volume Age Tables.",
  keywords = c("insert key words here"),
  authors = c(person(c("Steve"), "Cumming", email="stevec@sbf.laval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9001"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "yieldTables.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("yieldTableMaxAge","numeric", 300, NA, NA, "dimensions of tables"),
    defineParameter("yieldTableDir", "character", "", NA, NA, "directory in which table families are found"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = "yieldTables",
    objectClass = "list",
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.yieldTables = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$yieldTablesInit(sim)
        
    
    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$yieldTables$.plotInitialTime, "yieldTables", "plot")
    sim <- scheduleEvent(sim, params(sim)$yieldTables$.saveInitialTime, "yieldTables", "save")
  } 
  else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
yieldTablesInit <- function(sim) {
 
  #create a list to hold the output
  #browser()
  sim$yieldTables<-list()
  ytDir <- params(sim)$yieldTables$yieldTableDir
  
  if (ytDir == ""){
    warning("no Yield Table Directory given")
  }
  ytDir <- file.path(inputPath(sim),ytDir)
  ytFs <- dir(ytDir)  #this will be a vector of names of visible files
  for(ytF in ytFs){     #and yes, various sanity chex would be appropriate
    #I'd wanted to use the event handler for this, but it's too much trouble.
    #sim <- scheduleEventleEvent(sim, time(sim), "yieldTables", i, .highest())
    if (ytF == "AB" ){
      res <- yieldTablesAB(sim, file.path(ytDir, ytF))
      sim$yieldTables[[ytF]] <- res
    }
    else {
      warning(paste("Undefined yield family: '", ytF,
                    "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
    }
  }
  return(invisible(sim))
}

#these source-specific processing functions need to be hand-tooled, so
#I don't see much point in trying to paramaterise them. 

yieldTablesAB <- function(sim,dataDir) {
  
  #browser()
  dataFile <- file.path(dataDir,"AlPac\ AME\ Mixedwood\ VolTabs.vol")
  
  dt <- read.table(dataFile)
  n <- dim(dt)[2]
  #plot(as.numeric(dt[2,]),col="red")
  #points(as.numeric(dt[1,]),col="green")
  vol <- c(0,dt[1,] + dt[2,])
  ages<- c(1, 1:n*10)
  xout <- 1:params(sim)$yieldTables$yieldTableMaxAge
  res <- approx(ages,vol,xout,rule=2)
  
  #lines(res,col="blue",lwd=3)
  return(res$y)
}

### template for save events
yieldTablesSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
yieldTablesPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}





