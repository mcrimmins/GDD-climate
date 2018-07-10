# old vs corrected grid comparisons
# MAC 03/25/18

library(raster)
library(rasterVis)

# get state boundaries
states <- getData('GADM', country='United States', level=1)

# new grids
gdd50_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd250_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh250_1981-2010.grd")
gdd450_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh450_1981-2010.grd")
# old grids
gdd50_x4old<-stack("./X4_anomDOY_baseT10_thresh50_1981-2010.grd")

# set names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")
# old
names(gdd50_x4old)<-paste0(seq(1948, 2016, by=1),".GDD50")

# mask out   
maskNA<-raster("./fixed/maskNAalt.grd") 
gdd50_x4 <-  mask(gdd50_x4, maskNA)  
gdd250_x4 <- mask(gdd250_x4, maskNA)
gdd450_x4 <- mask(gdd450_x4, maskNA)

# standardize grids with z-scores
zscore<-function(x){scale(x, center = TRUE, scale = TRUE)}
ptm <- proc.time()
beginCluster(6)
  gdd450_x4 <- clusterR(gdd450_x4, calc, args=list(fun=zscore))
  gdd250_x4 <- clusterR(gdd250_x4, calc, args=list(fun=zscore))
  gdd50_x4 <- clusterR(gdd50_x4, calc, args=list(fun=zscore))
  #gdd50_x4old <- clusterR(gdd50_x4old, calc, args=list(fun=zscore))
endCluster()
proc.time() - ptm  

# set names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")

# check standardized anom plots
diff450_50<-gdd450_x4-gdd50_x4
my.at <- seq(-4, 4, 0.5)
levelplot(diff450_50[[47:69]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450-50 Z-Diff - zscore")

  layer(sp.polygons(states))


my.at <- seq(-4, 4, 0.1)
levelplot(gdd450_x4[[34:69]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450 Anomalies - zscore")+ 
  layer(sp.polygons(states))

my.at <- seq(-4, 4, 0.1)
levelplot(gdd50_x4old[[58:69]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50 Anomalies - zscore (OLD)")+ 
  layer(sp.polygons(states))

gdd450raw_x4 <- mask(gdd450raw_x4, maskNA)
my.at <- seq(1, 365, 5)
levelplot(gdd450raw_x4[[46]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450 Anomalies")+ 
  layer(sp.polygons(states))

meanDOY450 <- mask(meanDOY450, maskNA)
my.at <- seq(0, 1, 1)
levelplot(gdd450raw[[46]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450")+ 
  layer(sp.polygons(states))

# new mask
# mask out   
maskNA<-raster("./fixed/maskNAalt.grd") 
gdd450_x4 <- mask(gdd450_x4, maskNA)
my.at <- seq(-50, 50, 5)
levelplot(gdd450_x4[[46]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450 Anomalies - zscore")+ 
  layer(sp.polygons(states))


# standard dev - parallel?
gddAnomSD <- overlay(gdd450_x4, fun=sd)

beginCluster(6)
gddAnomSD <- clusterR(gdd450_x4, overlay, args=list(fun=sd))
endCluster()

my.at <- seq(0, 15, 3)
levelplot(gddAnomSD, at=my.at, margin=FALSE, main="Std Dev of GDD450 Anom 1948-2016 (TopoWx 81-00)") + 
  layer(sp.polygons(states))

