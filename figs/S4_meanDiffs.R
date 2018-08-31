# FIGURE S4?
# Mean DOY Diff
# MAC 08/15/18

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
meanDayGDD50<-raster("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
meanDayGDD250<-raster("./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd")
meanDayGDD450<-raster("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
# load mask
maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd") 
# load datasets
meanDayGDD50B0<-raster("./fixed/X4_meanDOY_baseT0_thresh50_1981-2010.grd")
meanDayGDD250B0<-raster("./fixed/X4_meanDOY_baseT0_thresh250_1981-2010.grd")
meanDayGDD450B0<-raster("./fixed/X4_meanDOY_baseT0_thresh450_1981-2010.grd")

allMaps<-stack(mask(meanDayGDD450-meanDayGDD50, maskNA),
               mask(meanDayGDD450B0-meanDayGDD50B0, maskNA_0),
               mask(meanDayGDD250-meanDayGDD50, maskNA),
               mask(meanDayGDD250B0-meanDayGDD50B0, maskNA_0),
               mask(meanDayGDD450-meanDayGDD250, maskNA),
               mask(meanDayGDD450B0-meanDayGDD250B0, maskNA_0))

names(allMaps)<-c("Diff450.50BaseT10","Diff450.50BaseT0",
                  "Diff250.50BaseT10","Diff250.50BaseT0",
                  "Diff450.250BaseT10","Diff450.250BaseT0")

# DOY colorramp
mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))
my.at <- seq(10, 150, 5)

p1<-levelplot(allMaps, layout=c(2,3),at=my.at, par.settings = mapTheme,
              margin=FALSE, main="Mean Diffs", colorkey=list(space="bottom"),
              scales=list(alternating=3)) + 
  layer(sp.polygons(states))

