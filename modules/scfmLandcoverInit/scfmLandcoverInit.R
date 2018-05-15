stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="scfmLandcoverInit",
  description="Takes the LCC05 classification of 39 land cover classes, and reclassifies it to flammable and inflammable [1,0]",
  keywords=c("fire", "LCC05", "land cover classification 2005", "BEACONs"),
  childModules=character(),
  authors=c(person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c("2005-01-01", NA)),
  documentation = list("README.txt", "scfmLandcoverInit.Rmd"),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster"),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, desc="Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, desc="Interval between plotting"), #usually, once is enough
    defineParameter("nonFlammClasses", "numeric", c(36, 37, 38, 39), NA, NA, desc="which classes don't burn"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, desc="Should Cache the processed vegMap?")
    ),
  inputObjects=bind_rows(
    expectsInput(objectName="nNbrs", objectClass = "numeric", desc="raster cell neighborhood defaults to 8"),
    expectsInput(objectName="vegMap", objectClass = "RasterLayer", desc="vegetation template LCC05"),
    expectsInput(objectName="Mask", objectClass = "RasterLayer", desc="NA mask of study area")
  ),
  outputObjects=bind_rows(
    createsOutput(objectName = "flammableMap",  objectClass = "RasterLayer", desc="which pixels can burn: coded 0"),
    createsOutput(objectName = "landscapeAttr",  objectClass = "list", desc="landscape parameters from scfm{Regime|Driver}")
  )
))

doEvent.scfmLandcoverInit = function(sim, eventTime, eventType, debug=FALSE) {
  switch(
    eventType, 
    init =  {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                         "scfmLandcoverInit", "plot")
    },
    plot = {
      Plot(sim$vegMap, new=TRUE)
      Plot(sim$flammableMap, legend=FALSE) # this is failing probably due to a bug in Plot
                                           # EJM is working on it 20160224
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "scfmLandcoverInit", "plot")
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                    "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  )
  return(invisible(sim))
}

genFireMapAttr <- function(sim){
  #browser()
  #we assume the raster is a regular grid, not in lat-long
  #cellSize <- mean(raster::values(area(sim$flammableMap,na.rm=TRUE)),
  #                 na.rm=TRUE)*100 #area returns km^2!
  cellSize <- prod(res(sim$flammableMap))/1e4  #copied below from template sim$vegMap
  
  if (is.na(cellSize))
    stop("scfmLandcoverInit: cellSize is NA")
  
  nFlammable<-table(values(sim$flammableMap), useNA="no")["0"] 
  if (sim$nNbrs==8)
    w<-matrix(c(1,1,1,1,0,1,1,1,1),nrow=3,ncol=3)
  else if (sim$nNbrs==4)
    w<-matrix(c(0,1,0,1,0,1,0,1,0),nrow=3,ncol=3)
  else 
    stop("illegal global neighbours spec")
  tmp <- focal(sim$flammableMap, w, na.rm=TRUE) #default function is sum(...,na.rm)
  x <- values(tmp)
  x <- x[raster::values(sim$flammableMap)==0] #only count neighbours for flammable cells!
  x <- sim$nNbrs - x  #need to invert, because we are counting the nonflamy 1's
  nv <- table(x,useNA="no")
  nNbrs <- rep(0,9) #guard against the terrible chance that 
  #not all nNbrs values are realised on the landscape. 
  nNbrs[as.integer(names(nv))+1] <- nv
  names(nNbrs) <- 0:8
  sim$landscapeAttr<-list(cellSize=cellSize,nFlammable=nFlammable,
                        burnyArea=cellSize*nFlammable, nNbrs=nNbrs)
  return(invisible(sim))
}

### template initilization
Init = function(sim) {
  # these classes are LCC05 specific
  #browser()
  
  #nonFlammClasses <- c(36, 37, 38, 39)  #should be a parameter.
  oldClass <- 0:39
  newClass <- ifelse(oldClass %in% P(sim)$nonFlammClasses,1,0)   #1 codes for non flammable 
                                                                 #see mask argument for SpaDES::spread()
  flammableTable <- cbind(oldClass, newClass)
  sim$flammableMap <-raster::ratify(raster::reclassify(sim$vegMap, flammableTable),count=TRUE)
  if ("Mask" %in% names(objs(sim))){
    if (class(sim$Mask) == "RasterLayer") #should also check they conform.
      sim$flammableMap <- sim$flammableMap * sim$Mask # NAs in Mask should 
    else
      stop("Invalid class for Mask: require RasterLayer")
  }
  #the count options should cause that "a column with frequencies is added. 
  #setColors(sim$flammableMap, n=2) <- c("blue","red")
  setColors(sim$flammableMap,n=2) <- c("skyblue", "red") 
  sim <- genFireMapAttr(sim)
  return(invisible(sim))
}


.inputObjects <- function(sim){
  
  if (!("nNbrs" %in% names(objs(sim)))){
    warning("nNbrs set to 8 in scfmLandcoverInit..inputObjects")
    sim$nNbrs <- 8
  }
  
  return(invisible(sim))
}
