# linear model of early vs. late
# 7/4/2018

library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)

# map layers
states <- getData('GADM', country='United States', level=1)

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')

# load data
load("./fixed/bothBaseT/detrendedBaseT10.RData")

s <- stack(gdd50_x4Resid, gdd450_x4Resid)
rm(gdd250_x4Resid, gdd450_x4Resid, gdd50_x4Resid)
funSlope=function(x) { if (is.na(x[1])){ NA } else { lm(x[70:138]~x[1:69])$coefficients[2] }}
funR2=  function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[70:138]~x[1:69]);summary(m)$r.squared }}
funpVal=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[70:138]~x[1:69]); summary(m)[["coefficients"]][2,4]}}

pval<-calc(s, fun=funpVal)

beginCluster(7)
  coeff <- clusterR(s, calc, args=list(fun=funSlope))
  #r2 <- clusterR(s, calc, args=list(fun=funR2))
  #pval <- clusterR(s, calc, args=list(fun=funpVal))
endCluster()

# alternative code to get raster regression stats
# https://stackoverflow.com/questions/20262999/how-to-output-regression-summarye-g-p-value-and-coeff-into-a-rasterbrick
funReg <- function(x) {
  if (all(is.na(x))) {
    return(cbind(NA,NA,NA))
  }
  m = lm(x~time)
  s  <- summary(m)
  r2 <- s$r.squared
  resid.s.e <- s$sigma
  pf<- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],lower.tail = FALSE) 
  cbind(r2, resid.s.e, pf)
}

rastReg <- calc(s, funReg)

# map layers
states <- getData('GADM', country='United States', level=1)

my.at <- seq(0, 1, 0.1)
levelplot(r2, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 R2 (TopoWx 48-16)")+ 
  layer(sp.polygons(states))

my.at <- seq(0, 3, 0.1)
levelplot(coeff, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Coeff (TopoWx 48-16)")+ 
  layer(sp.polygons(states))

