stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="scfmRegime",
  description="estimates fire regime parameters for BEACONs a la Steve's method",
  keywords=c("fire regime", "BEACONs"),
  authors=c(person(c("Steven", "G."), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules=character(),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list(),
  documentation = list("README.txt", "scfmRegime.Rmd"),
  reqdPkgs=list("sp", "rgdal"),
  parameters=rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "cache or not"),
    defineParameter("fireCause", "character", c("L"), NA_character_, NA_character_,  "subset of c(H,H-PB,L,Re,U)"),
    defineParameter("fireEpoch", "numeric", c(1961,1990), NA_integer_, NA_integer_, "start and end of normal period")
  ),
  inputObjects =  bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygons", desc="shapefile of study area"),
    expectsInput(objectName = "landscapeAttr", objectClass="list", desc="cellsize and total area of study area"),
    expectsInput(objectName = "firePointsDB", objectClass="SpatialPointsDataFrame", desc="fire point database")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName="firePoints", objectClass="SpatialPointsDataFrame", desc="fires cropped to studyArea"),
    createsOutput(objectName="scfmRegimePars", objectClass="list", desc="list of fire regime paramaters to simulate")
  )
))

doEvent.scfmRegime = function(sim, eventTime, eventType, debug=FALSE) {
  switch(
    eventType, 
    init = {
      sim <- Init(sim)
    },
      warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                    "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  )
  return(invisible(sim))
}


Init <- function(sim) {
  
  if ( !("landscapeAttr" %in% names(objs(sim))))
    stop("No landscape attribute list provided")
  
  #spatial subset the fires within our study arae
  simProjection  <- crs(sim$studyArea)  
  fireProjection <- crs(sim$firePointsDB)
  
  studyAreaTmp   <- sp::spTransform(sim$studyArea, CRSobj = fireProjection)
  sim$firePoints <- sim$firePointsDB[studyAreaTmp,]  
  sim$firePoints <- sp::spTransform(sim$firePoints,CRSobj = simProjection)
  #browser()
  #subset fires by cause and epoch.
  
  tmp <- as.data.frame(sim$firePoints)
  #extract and validate fireCause spec
  fc <- P(sim)$fireCause
  if (any(!(fc %in% base::levels(tmp$CAUSE))))
      stop("illegal fireCause: ", fc)
  tmp <- subset(tmp, CAUSE %in% fc)
  #extract and validate fireEpoch
  epoch <- P(sim)$fireEpoch
  if (length(epoch) != 2 || !is.numeric(epoch) || any(!is.finite(epoch)) || epoch[1] > epoch[2])
      stop("illegal fireEpoch: ",epoch)
  tmp <- subset(tmp, YEAR >= epoch[1] & YEAR <= epoch[2])

  
  #Poisson!!!

  nFires <- dim(tmp)[1]
  epochLength <- as.numeric(epoch[2]-epoch[1]) + 1
  rate <- nFires/(epochLength * sim$landscapeAttr$burnyArea)   # fires per ha per yr

  #calculate escaped fires
  #careful to subtract cellSize where appropriate
  xVec <- tmp$SIZE_HA[tmp$SIZE_HA > sim$landscapeAttr$cellSize]
  pEscape <- length(xVec)/nFires
  xBar <- mean(xVec)
  #lxBar<-mean(log(xVec))
  xMax <- max(xVec)
  zVec <- log(xVec/sim$landscapeAttr$cellSize)
  if (length(zVec) < 100)
    warning("Less than 100 \"large\" fires. That estimates may be unstable.\nConsider using a larger area and/or longer epoch.")
  #later, this would sim$HannonDayiha
  
  hdList <- HannonDayiha(zVec) #defined in source file data/TEutilsNew.R
  if (hdList$That > 0)
     maxFireSize <- exp(hdList$That)
  else {
    maxFireSize <- xMax 
    warning("scfmRegime: truncation estimator failed to terminate. using xMax")
  }
  maxFireSize <- as.integer(maxFireSize * sim$landscapeAttr$cellSize)
  
  #need to addd a name or code for basic verification by Driver module, and time field
  
  sim$scfmRegimePars <- list(ignitionRate=rate,
                       pEscape=pEscape,
                       xBar=xBar,       # mean fire size ## SHOULD USE Expected Value?
                       emfs=maxFireSize # Estimated Maximum Fire Size in ha
  )
  return(invisible(sim))
}


.inputObjects <- function(sim){
  if ( !("studyArea" %in% sim$.userSuppliedObjNames))
    stop("No studyArea provided")
  
  if (!("firePointsDB" %in% sim$.userSuppliedObjNames)){
     dsnPath <- file.path(dataPath(sim),"NFDB_point")
     dsnPath <- normalizePath(dsnPath)
     sim$firePointsDB <- Cache(readOGR, dsn=dsnPath, layer="NFDB_point_20171106", cacheRepo=cachePath(sim))
  }
  
  return(invisible(sim))
}

