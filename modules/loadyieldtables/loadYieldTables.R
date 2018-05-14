# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "loadYieldTables",
  description = "load and pre-processes stand Volume Age Tables.",
  keywords = c("insert key words here"),
  authors = c(person(c("Steve"), "Cumming", email="stevec@sbf.laval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9001"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "loadYieldTables.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("yieldTableMaxAge","numeric", 240, NA_integer_, NA_integer_, "dimensions of tables"),
    defineParameter("yieldTableDir", "character", NA_character_, NA_character_, NA_character_, "directory in which table families are found")
  ),
  inputObjects = data.table(objectName=character(0),objectClass=character(0),desc=character(0)),
  outputObjects = bind_rows(
    createsOutput(objectName = "yieldTables", objectClass = "list", desc = "list of regional yield table objects, indexed by subfolder name")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.loadYieldTables = function(sim, eventTime, eventType, debug = FALSE) {
  switch (
    eventType, 
    init = {
      sim <- Init(sim)
     }, 
     warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  #browser()
  sim$yieldTables<-list()    #is a list, at least one element
  tmp <- P(sim)$yieldTableDir
  if (is.na(tmp))
    ytDir <- file.path(dataPath(sim))
  else
    ytDir <- file.path(inputPath(sim),tmp)
  ytFs <- dir(ytDir) 
  
  for(ytF in ytFs){    
    fp <- file.path(ytDir, ytF)
    if (!file_test("-d",fp))
      next
    switch (
      ytF, 
      AB = {
        res <- yieldTablesAB(sim, fp)
      },
      warning(paste("Undefined yield family: '", ytF,
                    "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
    )
    sim$yieldTables[[ytF]] <- res
  }
  return(invisible(sim))
}

#these source-specific processing functions need to be hand-tooled, so
#I don't see much point in trying to paramaterise them. 
yieldTablesAB <- function(sim,dataDir) {
  #browser()
  res <- list()
  dataFile <- file.path(dataDir,"AlPac\ AME\ Mixedwood\ VolTabs.vol")
  dt <- read.table(dataFile)
  n <- dim(dt)[2] 
  
  ageMax <- P(sim)$yieldTableMaxAge
  
  if (n*10 > ageMax){
    idx <- as.integer(ageMax/10)
    dt <- dt[,1:idx]
    n <- dim(dt)[2]
    message("truncating yieldTables to yieldTableMaxAge in yieldTabelsAB")
  }
      
  xout <- 1:ageMax
  volTab <- matrix(nrow=length(xout),ncol=3,dimnames=list(NULL,c("Con","Dec","Total")))
  ytNames<-c("Aw","Aw/Sw","AwSw","SwAw","Sw","Sb","Pj","AwPj")  #found These names from old FeenixCode
  for (i in 1:8){
    ages<- c(1, 1:n*10)  #c(1,10,20,...210)
    vol <- c(0,dt[i*2-1,])
    volTab[,1] <- approx(ages,vol,xout,rule=2)$y
    vol <- c(0,dt[i*2,])
    volTab[,2] <- approx(ages,vol,xout,rule=2)$y
    volTab[,3] <- volTab[,1] + volTab[,2]
    res[[ytNames[i]]]<- volTab
  }
  return(res)   
}







