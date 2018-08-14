# point/click explorer for Raw Grids
# FOR DETRENDED GRIDS
# MAC 6/11/18
# COVARIANCE and CORRELATION Calcs

library(tidyr) # masks extract...
library(raster)
library(rasterVis)
library(sp)
library(maptools)
library(ggmap)
library(cowplot)
library(reshape2)
library(ggExtra)
library(grid)
library(scales)
#library(gridExtra)

# set rasteroptions
rasterOptions(progress = 'text')

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)

# load data
# load("./fixed/bothBaseT/detrendedBaseT0.RData")
# load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")
# meanDOY50<-raster("./fixed/X4_meanDOY_baseT0_thresh50_1981-2010.grd")
# meanDOY450<-raster("./fixed/X4_meanDOY_baseT0_thresh450_1981-2010.grd")
load("./fixed/bothBaseT/detrendedBaseT10.RData")
load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
meanDOY50<-raster("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
meanDOY450<-raster("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
corRaster<-corRasterDet_BaseT10

# load mask
maskNA<-raster("./fixed/maskNAalt.grd")
gdd50_x4 <- mask(gdd50_x4Resid, maskNA)
gdd450_x4 <- mask(gdd450_x4Resid, maskNA)
meanDOY50 <- mask(meanDOY50, maskNA)
meanDOY450 <- mask(meanDOY450, maskNA)

# base T0 mask
# maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd")
# gdd50_x4<- mask(gdd50_x4Resid_0, maskNA_0)
# gdd450_x4<- mask(gdd450_x4Resid_0, maskNA_0)
# meanDOY50 <- mask(meanDOY50, maskNA_0)
# meanDOY450 <- mask(meanDOY450, maskNA_0)

# point/click time series maps; add raw, anomalies and zscores...
plot(corRaster[[1]], 
     zlim=c(-1,1),
     breaks= seq(-1, 1, by = 0.1), 
     col=colorRampPalette(c("blue", "white", "red"))(length(seq(-1, 1, by = 0.1))-1),
     main="GDD50x4/GDD450x4 Detrended Pearson-Corr",
     legend.args=list(text='r', side=4, font=2, line=2.3))
plot(states, add=TRUE)

point<-click(corRaster[[1]], n=1, xy=TRUE, show=FALSE, type="p")

# get time series
gdd50ts<-t(raster::extract(gdd50_x4, cellFromXY(gdd50_x4, c(point$x,point$y))))
gdd450ts<-t(raster::extract(gdd450_x4, cellFromXY(gdd450_x4, c(point$x,point$y))))
# get means
mean50<-t(raster::extract(meanDOY50, cellFromXY(meanDOY50, c(point$x,point$y))))
mean450<-t(raster::extract(meanDOY450, cellFromXY(meanDOY450, c(point$x,point$y))))

gddTS<-as.data.frame(cbind(seq(1948, 2016, by=1),gdd50ts,gdd450ts))
colnames(gddTS)<-c("years","gdd50","gdd450")
gddTS$zProd<-scale(gddTS$gdd50, center=TRUE, scale=TRUE)*scale(gddTS$gdd450, center=TRUE, scale=TRUE)
gddTS$gdd50<-mean50+gddTS$gdd50
gddTS$gdd450<-mean450+gddTS$gdd450
gddTS$diff<-gddTS$gdd450-gddTS$gdd50


# variance, covariance and correlation
cov(gddTS$gdd50,gddTS$gdd450)
sd(gddTS$gdd50)
sd(gddTS$gdd450)
cor(gddTS$gdd50,gddTS$gdd450)

var(gddTS$diff)
var(gddTS$gdd50)
var(gddTS$gdd450)
# var(x-y)=var(x)+var(y)-2Cov(x,y)
(var(gddTS$gdd50)+var(gddTS$gdd450)-2*cov(gddTS$gdd50,gddTS$gdd450))/var(gddTS$gdd50)
var(gddTS$diff)/var(gddTS$gdd50)

# index related to corr
a<-cor(gddTS$gdd50,gddTS$gdd450)/(sd(gddTS$diff)/sd(gddTS$gdd50))
a<-0.3615178
a*(sd(gddTS$diff)/sd(gddTS$gdd50))
cor(gddTS$gdd50,gddTS$gdd450)

# (var(x-y)-var(x)-var(y))/2=cov(x,y)
(var(gddTS$diff)-var(gddTS$gdd50)-var(gddTS$gdd450))/2
cov(gddTS$gdd50,gddTS$gdd450)/(sd(gddTS$gdd50)*sd(gddTS$gdd450))

# sd version
(sd(gddTS$diff)-sd(gddTS$gdd50)-sd(gddTS$gdd450))/2
cor(gddTS$gdd50,gddTS$gdd450)
