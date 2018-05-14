stopifnot(packageVersion("SpaDES") >= "0.99.0")
defineModule(sim, list(
  name="scfmCrop",
  description="A translator module. Crops, reprojects and mask all necessary layers to\ 
      specifed extent. Selects or subsets all spatialised data e.g. fires.\ 
      Use cropped RasterLayer, defined by a spatial polygon that defines the area of interest",
  keywords=c("translator", "lcc05", "Land Cover Classification", "vegetation","Canadian National Fire Database"),
  childModules=character(),
  authors=c(person("Steve", "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut")),
            person(c("Eliot", "J","B"), "McIntire", email="emcintir@nrcan.gc.ca", role=c("aut")),
            person("Pierre", "Vernier", email="pierre.vernier@gmail.com")
            ),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster","rgeos","sp"),
  documentation = list("README.txt", "scfmCrop.Rmd"),
  parameters=rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA, desc="Use cache or not."),
    defineParameter("ecoDis", "numeric", 609, NA_real_, NA_real_, "ecodistrict")
        ),
  inputObjects=bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygons", desc = "study area"),
    expectsInput(objectName = "vegMapInit", objectClass = "RasterLayer", desc = "national landcover map LCC05"),
    expectsInput(objectName = "ageMapInit", objectClass ="RasterLayer", desc = "national 1km2 ageMap")
  ),
  outputObjects=bind_rows(
    createsOutput(objectName = "vegMap", objectClass = "RasterLayer", desc = ""),
    createsOutput(objectName = "ageMap", objectClass = "RasterLayer", desc = ""),
    createsOutput(objectName = "Mask", objectClass = "RasterLayer", desc = "")
  )
))

doEvent.scfmCrop = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    sim <- Init(sim)
  } else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                  "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}


Init <- function(sim) {

  #browser()
  
  dsn <- file.path(inputDir, "Ecodistricts")
  shape <- readOGR(dsn = dsn, layer = "ecodistricts")
  sim$studyArea<-shape[shape$ECODISTRIC==609,]
  
  vegProjection <- crs(sim$vegMapInit)
  
  if (is.na(crs(sim$studyArea)))            #in case it was sampled from the vegmap.
    crs(sim$studyArea) <- vegProjection
  
  simProjection <- crs(sim$studyArea)         #this would probably be set to be the same as the veg map at an earlier stage.
  
  #if(ncell(sim$vegMap)>5e5) beginCluster(min(parallel::detectCores(),6))
  
  #Project the study area into each input raster, then crop and mask; 
  #Then project result back into the sim projection.
  #browser()
  studyAreaTmp <- spTransform(sim$studyArea, CRSobj =vegProjection)
  sim$vegMap <-  crop(sim$vegMapInit, studyAreaTmp)
  crs(sim$vegMap) <- vegProjection
  sim$vegMap <- mask(sim$vegMap, studyAreaTmp) #É
  #sim$vegMap <- projectRaster(sim$vegMap, crs=simProjection, method="ngb", to=sim$vegMapInit)
  sim$Mask <- sim$vegMap
  sim$Mask[] <- ifelse(is.na(sim$vegMap[]), NA, 1)
  
  tmp <- getColors(sim$vegMapInit)[[1]]        # mask removes colors!
  setColors(sim$vegMap, n=length(tmp)) <- tmp  # so put them back.
  
  ageProjection <- crs(sim$ageMapInit)
  studyAreaTmp <- spTransform(sim$studyArea, CRSobj =ageProjection)
  sim$ageMap <-  crop(sim$ageMapInit, studyAreaTmp)
 # crs(sim$ageMap) <- ageProjection
  sim$ageMap <- mask(sim$ageMap,studyAreaTmp)
  sim$ageMap <- projectRaster(sim$ageMap,to=sim$vegMap,method="ngb")
  
  #endCluster()
    
  if(sum(!is.na(getValues(sim$ageMap)))==0)
      stop("There are no age data provided with input age map")
  if(sum(!is.na(getValues(sim$vegMap)))==0) 
    stop("There are no vegatation data provided with input vegatation map")
  
  return(invisible(sim))
}

.inputObjects <- function(sim){
  
  if (!("ageMapInit" %in% sim$.userSuppliedObjNames)){
    fp <- file.path(dataPath(sim),"ageMapInit","age.tif")
    sim$ageMapInit <- raster(fp)
  }
  
  if (!("vegMapInit" %in% sim$.userSuppliedObjNames)){
    fp <- file.path(dataPath(sim),"vegMapInit","LCC2005_V1_4a.tif")
    sim$vegMapInit <- raster(fp)
  }
  
  return(invisible(sim))
}

