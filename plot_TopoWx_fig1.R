# Potential fig 1 plot
# TopoWx 01/19/18

#library(cowplot)
library(raster)
library(rasterVis)
library(sp)
library(maptools)
library(gridExtra)
#library(RStoolbox)
#library(tidyr)
#library(dplyr)

# set rasteroptions
rasterOptions(progress = 'text')
tmpDir(create=TRUE)

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)

# # # or anoms
gdd50_x4<-stack("X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd200_x4<-stack("X4_anomDOY_baseT10_thresh200_1981-2010.grd")
gdd500_x4<-stack("X4_anomDOY_baseT10_thresh500_1981-2010.grd")

# mask out   
maskNA<-raster("maskNA.grd") 
gdd50_x4 <-  mask(gdd50_x4, maskNA)  
gdd200_x4 <- mask(gdd200_x4, maskNA)
gdd500_x4 <- mask(gdd500_x4, maskNA)

# ---- Plot Mean DOYs
meanDOY50_x4<-raster("X4_meanDOY_baseT10_thresh50_1981-2010.grd")
  meanDOY50_x4 <- mask(meanDOY50_x4, maskNA)
meanDOY200_x4<-raster("X4_meanDOY_baseT10_thresh200_1981-2010.grd")
  meanDOY200_x4 <- mask(meanDOY200_x4, maskNA)
meanDOY500_x4<-raster("X4_meanDOY_baseT10_thresh500_1981-2010.grd")
  meanDOY500_x4 <- mask(meanDOY500_x4, maskNA)
# set names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd200_x4)<-paste0(seq(1948, 2016, by=1),".GDD200")
names(gdd500_x4)<-paste0(seq(1948, 2016, by=1),".GDD500")

# plot mean DOY
meanDOY<-stack(meanDOY50_x4,meanDOY200_x4,meanDOY500_x4)
names(meanDOY)<-c("GDD50","GDD200","GDD500")
my.at <- seq(0, 240, 20)
meanFig<-levelplot(meanDOY, layout=c(1,3),at=my.at, par.settings = magmaTheme,
          margin=FALSE, main="Mean GDD - DOY", colorkey=list(space="bottom")) #+ 
#  layer(sp.polygons(states))

# plot anomaly years
# GDD2012<-stack(gdd50_x4[[58]],gdd200_x4[[58]],gdd500_x4[[58]],gdd50_x4[[63]],gdd200_x4[[63]],gdd500_x4[[63]],
#                gdd50_x4[[65]],gdd200_x4[[65]],gdd500_x4[[65]])
# names(meanDOY)<-c("2005 GDD50","2005 GDD200","2005 GDD500","2010 GDD50","2010 GDD200","2010 GDD500","2012 GDD50","2012 GDD200","2012 GDD500")
# my.at <- seq(-50, 50, 1)
# anomFig<-levelplot(GDD2012, layout=c(3,3), par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD Anomalies")+ 
#      layer(sp.polygons(states))
 GDD2005<-stack(gdd50_x4[[58]],gdd200_x4[[58]],gdd500_x4[[58]])
 names(GDD2005)<-c("2005 GDD50","2005 GDD200","2005 GDD500")
 GDD2010<-stack(gdd50_x4[[63]],gdd200_x4[[63]],gdd500_x4[[63]])
 names(GDD2010)<-c("2010 GDD50","2010 GDD200","2010 GDD500")
 GDD2012<-stack(gdd50_x4[[65]],gdd200_x4[[65]],gdd500_x4[[65]])
 names(GDD2012)<-c("2012 GDD50","2012 GDD200","2012 GDD500")
 
my.at <- seq(-50, 50, 1)
  GDD2005Fig<-levelplot(GDD2005, layout=c(1,3), par.settings = RdBuTheme, at=my.at, margin=FALSE, main="2005",  colorkey=list(space="bottom"))  
       #layer(sp.polygons(states))
  GDD2010Fig<-levelplot(GDD2010, layout=c(1,3), par.settings = RdBuTheme, at=my.at,colorkey=list(space="bottom"), margin=FALSE, main="2010") 
    #layer(sp.polygons(states))
  GDD2012Fig<-levelplot(GDD2012, layout=c(1,3), par.settings = RdBuTheme, at=my.at, margin=FALSE, main="2012",  colorkey=list(space="bottom")) 
    #layer(sp.polygons(states))
  
#grid.arrange(meanFig, anomFig, ncol=2)
print(meanFig, split=c(1,1,4,1), more=TRUE)
print(GDD2005Fig, split=c(2,1,4,1), more=TRUE)
print(GDD2010Fig, split=c(3,1,4,1), more=TRUE)
print(GDD2012Fig, split=c(4,1,4,1))