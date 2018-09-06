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
meanDOY<-stack(mask(meanDayGDD50B0, maskNA_0),mask(meanDayGDD250B0, maskNA_0),mask(meanDayGDD450B0,maskNA_0),
               mask(meanDayGDD50, maskNA),mask(meanDayGDD250, maskNA),mask(meanDayGDD450,maskNA))
mean.titles<-c("GDD50 - 0°C base temp",
                  "GDD250 - 0°C base temp",
                  "GDD450 - 0°C base temp",
                  "GDD50 - 10°C base temp",
                  "GDD250 - 10°C base temp",
                  "GDD450 - 10°C base temp"
                  )
my.at <- seq(0, 240, 10)
meanFig<-levelplot(meanDOY, layout=c(3,2),at=my.at, par.settings = mapTheme,
                   margin=FALSE, main="Mean GDD Day of Year", colorkey=list(space="bottom"),
                   names.attr=mean.titles, par.strip.text=list(cex=0.8),
                   sub=list(label="             Day of Year",cex=0.75,font = 1),
                   xlab='', ylab='') + 
            layer(sp.polygons(states))

# plot sDevs
sDev<-stack(mask(sdGDD50_0Res, maskNA_0),mask(sdGDD250_0Res, maskNA_0),mask(sdGDD450_0Res,maskNA_0),
            mask(sdGDD50Res, maskNA),mask(sdGDD250Res, maskNA),mask(sdGDD450Res,maskNA))
sd.titles<-c("GDD50 - 0°C base temp",
               "GDD250 - 0°C base temp",
               "GDD450 - 0°C base temp",
               "GDD50 - 10°C base temp",
               "GDD250 - 10°C base temp",
               "GDD450 - 10°C base temp")
my.at <- seq(0, 30, 1)
sdevFig<-levelplot(sDev, layout=c(3,2),at=my.at, par.settings = YlOrRdTheme,
                   margin=FALSE, main="GDD Standard Deviation", colorkey=list(space="bottom"),
                   names.attr=sd.titles, par.strip.text=list(cex=0.8),
                   sub=list(label="             SD (days)",cex=0.75,font = 1),
                   xlab='', ylab='') + 
  layer(sp.polygons(states))


#grid.arrange(meanFig, anomFig, ncol=2)
png("/home/crimmins/RProjects/TopoWx/figs/S1a_DOY.png", width = 7, height = 5, units = "in", res = 300L)
#grid.newpage()
#print(meanFig, split=c(1,1,1,2), more=TRUE)
#print(sdevFig, split=c(1,2,1,2))
print(meanFig, newpage = FALSE)
dev.off()

png("/home/crimmins/RProjects/TopoWx/figs/S1b_SD.png", width = 7, height = 5, units = "in", res = 300L)
#grid.newpage()
#print(meanFig, split=c(1,1,1,2), more=TRUE)
#print(sdevFig, split=c(1,2,1,2))
print(sdevFig, newpage = FALSE)

dev.off()
