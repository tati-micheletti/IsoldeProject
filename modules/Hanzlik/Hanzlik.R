defineModule(sim, list(
  name = "Hanzlik",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("First", "Middle"), "Last", email="email@example.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9001"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit =  "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Hanzlik.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("rationPeriodMultiplier", "numeric", 1, NA, NA, "Vm/R*x"),
    defineParameter("replanInterval", "numeric", 10, NA, NA, "How frequently do we replan?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "yieldTables", objectClass = "list", desc = "a list of named yield table objects"),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "map from whence comes theforest age-class structure"),
    expectsInput(objectName = "strataMap", objectClass = "Rasterayer", desc = "map assigning cells to yield tables"),
    expectsInput(objectName = "landscapeAttr", objectClass = "list", desc = "info about landscape we are running on; e.g. cellSize")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "hanzlikPars", objectClass = "list", desc = "hanzlik structures per yield class"),
    createsOutput(objectName = "annualCut", objectClass = "list", desc = "m^3/yr per yield Class")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Hanzlik = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, time(sim), "Hanzlik", "plan")
      },
    plan = {
      sim <- Plan(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$replanInterval, "Hanzlik", "plan")
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}



calcHanzlik <- function(ytList,rpm){
  cHelper <- function(yt,rpm){
    #browser()
    if (!is.matrix(yt))
      stop("invalid yield table")
    nr <- dim(yt)[1]
    nc <- dim(yt)[2] #we will assume last collumn has the total volume, and manage on that for now
    vt<- yt[,nc]/1:nr
    #plot(vt)
    R <- order(vt,decreasing=TRUE)[1] #trouve le age de la culmination
    vt <- yt[,nc]
    inc <- c(0, diff(vt))
    #tmp <- yt[R:nr]/(P(sim)$rationPeriodMultiplier*R)  
    tmp <- vt[R:nr]/(rpm*R)    #contribution of each age class to Vm/R in m^3/ha
    tmp <- c(inc[1:R-1], tmp)
    return(list(R=R,I=inc,hVec=tmp))
  }
  # assumes yt is just a vector of volumes in 1 yr age classes starting at 1
  # Vm/R + I
  res <- lapply(ytList, cHelper,rpm=rpm)
  return(res)
}

### template initialization
Init <- function(sim) {
  rpm <- P(sim)$rationPeriodMultiplier
  sim$hanzlikPars <- lapply(sim$yieldTables, calcHanzlik,rpm=rpm)
  return(invisible(sim))
}


Plan <- function(sim) {
  #browser()
  region <- sim$hanzlikPars[[1]]   # we are going to use the first region, if we can
  
  for (i in 1:length(region)){
    
    aac <- 0
    idx <- which(sim$strataMap[] == i)
  
    if (length(idx)>0) {
      hVec <-region[[i]]$hVec  #in future versions there will be list of these,
                             #one for each collumn in the YT
      nh <- length(hVec)
      aVec<-rep(0,nh)
   
      x <- tabulate(sim$ageMap[idx])
    
      nx<-length(x)
      if (nx <= nh){
        aVec[1:nx] <- x
      }
      else {
        aVec  <- x[1:nh]
        x[nh] <- x[nh] + sum(x[nh+1:nx]) #accumulate any "missing ones"
      }
      aac<-sum(aVec*hVec) * sim$landscapeAttr$cellSize #this needs to be changed.
    }
    sim$annualCut[[i]] <- aac    # 
  }
  return(invisible(sim))
}

oldPlan <- function(sim) {
  browser()
  region <- sim$hanzlikPars[[1]]   # we are going to use the first region, if we can
  
  hVec<-sim$hanzlikPars[[1]][["SwAw"]]$hVec       #1 == "AB": for now, assume hanzlikPars has at least one object, and take the 1st
  #Comment peut-on le generaliser pour plusieurs courbes de rendemment?
  nh <- length(hVec)
  res<-rep(0,nh)
  x<-tabulate(sim$ageMap[])         # yieldTableMazAge = nh
  # tabulate fills gaps with 0s
  nx<-length(x)
  if (nx <= nh){
    res[1:nx] <- x
  }
  else {
    res <- x[1:nh]
    x[nh] <- x[nh] + sum(x[nh+1:nx]) #accumulate any "missing ones"
  }
  res<-sum(res*hVec) * sim$cellSize #this needs to be changed.
  print(sprintf("%d AAC = %5.1f\n",time(sim),res/1e3))
  sim$annualCut <- res  
  return(invisible(sim))
}

