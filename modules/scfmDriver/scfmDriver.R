
defineModule(sim, list(
  name="scfmDriver",
  description="Generate parameters for the scfm 3-stage landscape fire model",
  keywords=c("fire"),
  authors=c(person(c("Steven", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules=character(),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year", 
  citation=list(),
  documentation = list("README.txt", "scfmDriver.Rmd"),
  reqdPkgs=list("stats"),
  parameters=rbind(
    defineParameter("returnInterval", "numeric", 1, NA, NA, desc="Years for scaling rates")
  ),
  #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
  inputObjects=bind_rows(
    expectsInput(objectName="scfmRegimePars", objectClass="list", desc="canonical regime description"),
    expectsInput(objectName="landscapeAttr", objectClass="list", desc="details of the current landscape structure"),
    expectsInput(objectName="cTable2", objectClass="data.frame", desc="lame-ass calibration data")
  ),
  outputObjects=bind_rows(
    createsOutput(objectName = "scfmPars", objectClass = "list", desc = "parameters for threeStageFireModel")
  )
))

doEvent.scfmDriver = function(sim, eventTime, eventType, debug=FALSE) {
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


# 1 - (1-p0)**N = pEscape
# 1 - pEscape = (1-p0)**Ns
# (1 - pEscape)**1/N = 1 - p0
# p0 = 1 - (1 - pEscape)**1/N

hatP0<-function(pE,n=8){
  1 - (1-pE)**(1/n)
}

#a real clever boots might minimise the abs log odds ratio. 
#be my guest.

escapeProbDelta<-function(p0,w,hatPE){
  abs(sum(w*(1-(1-p0)**(0:8)))-hatPE)  
}

Init = function(sim) {
  #this table contains calibration data for several landscape sizes
  #and several min fire sizes (1 or 2 cells), organised by collumn.
  #The data were made by Steve Cumming in June 2013 for a whole other purpose.
  #I chose the one that seems most appropriate to me
  #browser()
  #we know this table was produced with MinFireSize=2cells.
  #browser()
  y <- sim$cTable2$y
  x <- sim$cTable2$p
  m.lw <- lowess(y~x,iter=2)
  if (any(diff(m.lw$y)<0))
    warning("scfmDriver: lowess curve non-monotone. Proceed with caution")
  targetSize <- sim$scfmRegimePars$xBar/sim$landscapeAttr$cellSize - 1 
  pJmp <- approx(m.lw$y,m.lw$x,targetSize,rule=2)$y
  w<-sim$landscapeAttr$nNbrs
  w<-w/sum(w)
  hatPE<-sim$scfmRegimePars$pEscape
  #browser()
  #This won't actually work unless sim$nNbrs is 8
  minP0 <- hatP0(hatPE,sim$nNbrs)
  maxP0 <- hatP0(hatPE,floor(sum(w*0:8)))
  if (minP0 < maxP0){ 
    foo<-stats::optimise(escapeProbDelta,
                interval=c(minP0, maxP0),
                tol=1e-6,
                w=w,
                hatPE=hatPE)
    adjP0 <- foo$minimum
  }
  else
    adjP0 <- minP0
  
  #it is almost obvious that the true minimum must occur within the interval specified in the 
  #call to optimise, but I have not proved it, nor am I certain that the function being minimised is 
  #monotone.

  rate<-sim$scfmRegimePars$ignitionRate * sim$landscapeAttr$cellSize * P(sim)$returnInterval 
  pIgnition <- rate #approximate Poisson arrivals as a Bernoulli process at cell level.
                    #for Poisson rate << 1, the expected values are the same, partially accounting
                    #for multiple arrivals within years. Formerly, I used a poorer approximation
                    #where 1-p = P[x==0 | lambda=rate] (Armstrong and Cumming 2003).
                    #See the .Rmd file for details.
  
  if (pIgnition >= 1 || pIgnition < 0)
    stop("illegal estimate of igntition probability")
  if (pIgnition > 0.01)
    warning("Poisson variance approximation will be poor")
  
  sim$scfmPars<-list(pSpread=pJmp,
                     p0=adjP0,
                     naiveP0=minP0, 
                     pIgnition=pIgnition,
                     maxBurnCells=as.integer(round(sim$scfmRegimePars$emfs/sim$landscapeAttr$cellSize)))

  return(invisible(sim))
}
 
.inputObjects <- function(sim){
  if (!("cTable2" %in% sim$.userSuppliedObjNames)){
    fn <- file.path(dataPath(sim), "FiresN1000MinFiresSize2NoLakes.csv")
    sim$cTable2 <- read.csv(fn)
  }
  return(invisible(sim))
}
