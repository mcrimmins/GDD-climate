# SDEV plots
# 07/10/18 MAC

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


# plot mean DOY - BASET10
meanDOY<-stack(mask(meanDayGDD50, maskNA),mask(meanDayGDD250, maskNA),mask(meanDayGDD450,maskNA))
names(meanDOY)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 240, 10)
meanFig10<-levelplot(meanDOY, layout=c(1,3),at=my.at, par.settings = viridisTheme,
                   margin=FALSE, main="Mean GDD-BaseT10", colorkey=list(space="bottom"),
                   xlab='', ylab='') #+ 
  #layer(sp.polygons(states))
# plot sDevs
sDev<-stack(mask(sdGDD50Res, maskNA),mask(sdGDD250Res, maskNA),mask(sdGDD450Res,maskNA))
names(sDev)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 30, 1)
sdevFig10<-levelplot(sDev, layout=c(1,3),at=my.at, par.settings = YlOrRdTheme,
                   margin=FALSE, main="GDD Sdevs-BaseT10", colorkey=list(space="bottom"),
                   xlab='', ylab='') #+ 
  #layer(sp.polygons(states))
# plot mean DOY - BASET0
meanDOY<-stack(mask(meanDayGDD50B0, maskNA_0),mask(meanDayGDD250B0, maskNA_0),mask(meanDayGDD450B0,maskNA_0))
names(meanDOY)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 240, 10)
meanFig0<-levelplot(meanDOY, layout=c(1,3),at=my.at, par.settings = viridisTheme,
                     margin=FALSE, main="Mean GDD-BaseT0", colorkey=list(space="bottom"),
                    xlab='', ylab='') #+ 
#layer(sp.polygons(states))
# plot sDevs
sDev<-stack(mask(sdGDD50_0Res, maskNA_0),mask(sdGDD250_0Res, maskNA_0),mask(sdGDD450_0Res,maskNA_0))
names(sDev)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 30, 1)
sdevFig0<-levelplot(sDev, layout=c(1,3),at=my.at, par.settings = YlOrRdTheme,
                     margin=FALSE, main="GDD Sdevs-BaseT0", colorkey=list(space="bottom"),
                    xlab='', ylab='') #+ 
#layer(sp.polygons(states))

#grid.arrange(meanFig, anomFig, ncol=2)
print(meanFig0, split=c(1,1,4,1), more=TRUE)
print(sdevFig0, split=c(2,1,4,1), more=TRUE)
print(meanFig10, split=c(3,1,4,1), more=TRUE)
print(sdevFig10, split=c(4,1,4,1))

# faceted scatterplot of DOY vs stdev
meanDOYB10<-stack(mask(meanDayGDD50, maskNA),mask(meanDayGDD250, maskNA),mask(meanDayGDD450,maskNA))
sDevB10<-stack(mask(sdGDD50Res, maskNA),mask(sdGDD250Res, maskNA),mask(sdGDD450Res,maskNA))
meanDOYB0<-stack(mask(meanDayGDD50B0, maskNA_0),mask(meanDayGDD250B0, maskNA_0),mask(meanDayGDD450B0,maskNA_0))
sDevB0<-stack(mask(sdGDD50_0Res, maskNA_0),mask(sdGDD250_0Res, maskNA_0),mask(sdGDD450_0Res,maskNA_0))
latGrid <- (init(meanDayGDD50, 'y'))
# allGrids<-stack(meanDOYB0, meanDOYB10, sDevB0, sDevB10, latGrid)
# names(allGrids)<-c("GDD50b0","GDD250b0","GDD450b0","GDD50b10","GDD250b10","GDD450b10",
#                   "GDD50b0SD","GDD250b0SD","GDD450b0SD","GDD50b10SD","GDD250b10SD","GDD450b10SD",
#                   "latitude")
# allGridsDF <- data.frame((values(allGrids)))
# allGridsDF<-melt(allGridsDF,measure.vars = c("GDD50b0","GDD250b0","GDD450b0","GDD50b10","GDD250b10","GDD450b10",
#                                              "GDD50b0SD","GDD250b0SD","GDD450b0SD","GDD50b10SD","GDD250b10SD","GDD450b10SD"))


meanGrids<-stack(meanDOYB0, meanDOYB10, latGrid)
names(meanGrids)<-c("GDD50b0","GDD250b0","GDD450b0","GDD50b10","GDD250b10","GDD450b10",
                   "latitude")
meanGridsDF <- data.frame((values(meanGrids)))
meanGridsDF<-melt(meanGridsDF,measure.vars = c("GDD50b0","GDD250b0","GDD450b0","GDD50b10","GDD250b10","GDD450b10"))

sdGrids<-stack(sDevB0, sDevB10, latGrid)
names(sdGrids)<-c("GDD50b0SD","GDD250b0SD","GDD450b0SD","GDD50b10SD","GDD250b10SD","GDD450b10SD",
                    "latitude")
sdGridsDF <- data.frame((values(sdGrids)))
sdGridsDF<-melt(sdGridsDF,measure.vars = c("GDD50b0SD","GDD250b0SD","GDD450b0SD","GDD50b10SD","GDD250b10SD","GDD450b10SD"))

bothGridsDF<-cbind(meanGridsDF,sdGridsDF)
  bothGridsDF <- bothGridsDF[,-1]
    colnames(bothGridsDF)[2]<-"DOY"
    colnames(bothGridsDF)[5]<-"sDev"
  bothGridsDF<-na.omit(bothGridsDF) # thin out data frame
  bothGridsDF<-separate(bothGridsDF,variable,c("gdd","base"), sep ="b")
  bothGridsDF$gdd<-factor(bothGridsDF$gdd, levels=c("GDD50","GDD250","GDD450"))
  bothGridsDF$base<-factor(bothGridsDF$base, levels=c("0","10"))
  #colnames(bothGridsDF)<-c()
  library("ggplot2")
  ggplot(bothGridsDF, aes(x = DOY, y = sDev)) +
    geom_hex() +
    #geom_smooth(method = "lm", se = FALSE)+
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 4), se = FALSE)+
    facet_grid(gdd ~ base)+
    scale_fill_gradient(low='grey',high='red')+
    theme_bw()
  
  

# scatter plot
plot( mask(meanDayGDD50, maskNA), mask(sdGDD50Res, maskNA), maxpixels=500000, col='red')
plot( mask(meanDayGDD450, maskNA), mask(sdGDD450Res, maskNA), maxpixels=500000, col='green', add=TRUE)



my.at <-  seq(0.0, 20, 0.5)
levelplot(sdGDD50Res, par.settings = YlOrRdTheme, at=my.at, margin=TRUE, main="Sdev GDD50-BaseT10 DOY, TopoWx 1948-2016")+
  layer(sp.polygons(states))

vars<-stack(mask(meanDayGDD50, maskNA), mask(sdGDD50Res, maskNA))
names(vars)<-c("meanDOY","sdGDD")
gdd50<-hexbinplot(sdGDD~meanDOY, data=vars, alpha=0.5, type=c("r","g"),
           aspect=0.5, colorkey=FALSE, xlim=c(0, 250), ylim=c(0,35))
# xyplot(sdGDD~meanDOY, data = vars, pch=20, type=c("p","g","r"),
#        auto.key = list(space = 'right'),
#        alpha = 0.2)
vars<-stack(mask(meanDayGDD450, maskNA), mask(sdGDD450Res, maskNA))
names(vars)<-c("meanDOY","sdGDD")
gdd450<-hexbinplot(sdGDD~meanDOY, data=vars, alpha=0.5, type=c("r","g"),
           aspect=0.5, colorkey=FALSE, xlim=c(0, 250), ylim=c(0,35))


# transect plots 29.903740, -82.833766, 48.350484, -95.478082
# from https://cmerow.github.io/RDataScience/05_Raster.html
library(dplyr)
transect = SpatialLinesDataFrame(
  SpatialLines(list(Lines(list(Line(
    rbind(c(-82.833766, 29.903740),c(-95.478082, 48.350484)))), ID = "S-N"))),
  data.frame(Z = c("transect"), row.names = c("S-N")))
gplot(sdGDD50Res)+geom_tile(aes(fill=value))+
  geom_line(aes(x=long,y=lat),data=fortify(transect),col="red")

trans=raster::extract(x=stack(sdGDD50Res,meanDayGDD50),
                      y=transect,
                      along=T,
                      cellnumbers=T) %>% data.frame()
trans[,c("lon","lat")]=coordinates(sdGDD50Res)[trans$cell,]
trans$order=as.integer(rownames(trans))
transl=group_by(trans,lon,lat)%>%
  gather(variable, value, -lon, -lat, -cell, -order)
ggplot(transl,aes(x=lat,y=value,
                  colour=variable,
                  group=variable,
                  order=order))+
  geom_line()
