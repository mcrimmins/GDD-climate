# Potential fig 1 plot
# TopoWx 01/19/18
# adjusted for new, corrected GDD grids


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
gdd50_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd250_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh250_1981-2010.grd")
gdd450_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh450_1981-2010.grd")

# mask out   
maskNA<-raster("./fixed/maskNAalt.grd") 
gdd50_x4 <-  mask(gdd50_x4, maskNA)  
gdd250_x4 <- mask(gdd250_x4, maskNA)
gdd450_x4 <- mask(gdd450_x4, maskNA)

# ---- Plot Mean DOYs
meanDOY50_x4<-raster("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
  meanDOY50_x4 <- mask(meanDOY50_x4, maskNA)
meanDOY250_x4<-raster("./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd")
  meanDOY250_x4 <- mask(meanDOY250_x4, maskNA)
meanDOY450_x4<-raster("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
  meanDOY450_x4 <- mask(meanDOY450_x4, maskNA)
# set names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")

# plot mean DOY
meanDOY<-stack(meanDOY50_x4,meanDOY250_x4,meanDOY450_x4)
names(meanDOY)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 240, 20)
meanFig<-levelplot(meanDOY, layout=c(1,3),at=my.at, par.settings = magmaTheme,
          margin=FALSE, main="Mean GDD - DOY", colorkey=list(space="bottom")) #+ 
#  layer(sp.polygons(states))

# plot mean DOY diffs
meanDOYdiffs<-stack(meanDOY450_x4-meanDOY50_x4,meanDOY250_x4-meanDOY50_x4,meanDOY450_x4-meanDOY250_x4)
names(meanDOYdiffs)<-c("GDD450-50","GDD250-50","GDD450-250")
my.at <- seq(0, 80, 5)
meanDiffFig<-levelplot(meanDOYdiffs, layout=c(1,3),at=my.at, par.settings = viridisTheme,
                   margin=FALSE, main="Diffs in Mean GDD DOY", colorkey=list(space="bottom"))

# plot anomaly years
# GDD2012<-stack(gdd50_x4[[58]],gdd250_x4[[58]],gdd450_x4[[58]],gdd50_x4[[63]],gdd250_x4[[63]],gdd450_x4[[63]],
#                gdd50_x4[[65]],gdd250_x4[[65]],gdd450_x4[[65]])
# names(meanDOY)<-c("2005 GDD50","2005 GDD250","2005 GDD450","2010 GDD50","2010 GDD250","2010 GDD450","2012 GDD50","2012 GDD250","2012 GDD450")
# my.at <- seq(-50, 50, 1)
# anomFig<-levelplot(GDD2012, layout=c(3,3), par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD Anomalies")+ 
#      layer(sp.polygons(states))
 GDD2005<-stack(gdd50_x4[[58]],gdd250_x4[[58]],gdd450_x4[[58]])
 names(GDD2005)<-c("2005 GDD50","2005 GDD250","2005 GDD450")
 GDD2010<-stack(gdd50_x4[[63]],gdd250_x4[[63]],gdd450_x4[[63]])
 names(GDD2010)<-c("2010 GDD50","2010 GDD250","2010 GDD450")
 GDD2012<-stack(gdd50_x4[[65]],gdd250_x4[[65]],gdd450_x4[[65]])
 names(GDD2012)<-c("2012 GDD50","2012 GDD250","2012 GDD450")
 
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