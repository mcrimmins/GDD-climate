# FIGURE S3?
# Mean DOY and SDEV maps
# MAC 08/15/18
# from sdevMapsPlots.R

library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)
library(reshape2)
library(tidyr)

# map layers
states <- getData('GADM', country='United States', level=1)

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')

# linear model  BASE 10----
# load mask
maskNA<-raster("./fixed/maskNAalt.grd") 
# load datasets
# load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")
meanDayGDD50<-raster("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
meanDayGDD250<-raster("./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd")
meanDayGDD450<-raster("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
# load mask
maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd") 
# load datasets
meanDayGDD50B0<-raster("./fixed/X4_meanDOY_baseT0_thresh50_1981-2010.grd")
meanDayGDD250B0<-raster("./fixed/X4_meanDOY_baseT0_thresh250_1981-2010.grd")
meanDayGDD450B0<-raster("./fixed/X4_meanDOY_baseT0_thresh450_1981-2010.grd")


# DOY colorramp
mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))

# plot mean DOY - BASET10
meanDOY<-stack(mask(meanDayGDD50, maskNA),mask(meanDayGDD250, maskNA),mask(meanDayGDD450,maskNA))
names(meanDOY)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 240, 10)
meanFig10<-levelplot(meanDOY, layout=c(1,3),at=my.at, par.settings = mapTheme,
                     margin=FALSE, main="Mean GDD-BaseT10", colorkey=list(space="bottom"),
                     xlab='', ylab='') + 
            layer(sp.polygons(states))
# plot sDevs
sDev<-stack(mask(sdGDD50Res, maskNA),mask(sdGDD250Res, maskNA),mask(sdGDD450Res,maskNA))
names(sDev)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 30, 1)
sdevFig10<-levelplot(sDev, layout=c(1,3),at=my.at, par.settings = YlOrRdTheme,
                     margin=FALSE, main="GDD Sdevs-BaseT10", colorkey=list(space="bottom"),
                     xlab='', ylab='') + 
layer(sp.polygons(states))
# plot mean DOY - BASET0
meanDOY<-stack(mask(meanDayGDD50B0, maskNA_0),mask(meanDayGDD250B0, maskNA_0),mask(meanDayGDD450B0,maskNA_0))
names(meanDOY)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 240, 10)
meanFig0<-levelplot(meanDOY, layout=c(1,3),at=my.at, par.settings = mapTheme,
                    margin=FALSE, main="Mean GDD-BaseT0", colorkey=list(space="bottom"),
                    xlab='', ylab='') + 
layer(sp.polygons(states))
# plot sDevs
sDev<-stack(mask(sdGDD50_0Res, maskNA_0),mask(sdGDD250_0Res, maskNA_0),mask(sdGDD450_0Res,maskNA_0))
names(sDev)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 30, 1)
sdevFig0<-levelplot(sDev, layout=c(1,3),at=my.at, par.settings = YlOrRdTheme,
                    margin=FALSE, main="GDD Sdevs-BaseT0", colorkey=list(space="bottom", title="SD (days)"),
                    xlab='', ylab='') + 
layer(sp.polygons(states))

#grid.arrange(meanFig, anomFig, ncol=2)
png("/home/crimmins/RProjects/TopoWx/figs/S3maps.png", width = 7, height = 5, units = "in", res = 300L)
grid.newpage()
print(meanFig0, split=c(1,1,4,1), more=TRUE)
print(sdevFig0, split=c(2,1,4,1), more=TRUE)
print(meanFig10, split=c(3,1,4,1), more=TRUE)
print(sdevFig10, split=c(4,1,4,1))

dev.off()
