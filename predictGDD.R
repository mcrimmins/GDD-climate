# point/click explorer for Raw Grids
# FOR DETRENDED GRIDS
# MAC 6/11/18

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
#library(clipr)

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
meanDOY250<-raster("./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd")
meanDOY450<-raster("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
corRaster<-corRasterDet_BaseT10

# load mask
maskNA<-raster("./fixed/maskNAalt.grd")
gdd50_x4 <- mask(gdd50_x4Resid, maskNA)
gdd250_x4 <- mask(gdd250_x4Resid, maskNA)
gdd450_x4 <- mask(gdd450_x4Resid, maskNA)
meanDOY50 <- mask(meanDOY50, maskNA)
meanDOY250 <- mask(meanDOY250, maskNA)
meanDOY450 <- mask(meanDOY450, maskNA)

# base T0 mask
# maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd")
# gdd50_x4<- mask(gdd50_x4Resid_0, maskNA_0)
# gdd450_x4<- mask(gdd450_x4Resid_0, maskNA_0)
# meanDOY50 <- mask(meanDOY50, maskNA_0)
# meanDOY450 <- mask(meanDOY450, maskNA_0)

# # point/click time series maps; add raw, anomalies and zscores...
# plot(corRaster[[1]], 
#      zlim=c(-1,1),
#      breaks= seq(-1, 1, by = 0.1), 
#      col=colorRampPalette(c("blue", "white", "red"))(length(seq(-1, 1, by = 0.1))-1),
#      main="GDD50x4/GDD450x4 Detrended Pearson-Corr",
#      legend.args=list(text='r', side=4, font=2, line=2.3))
# plot(states, add=TRUE)
# 
# point<-click(corRaster[[1]], n=1, xy=TRUE, show=FALSE, type="p")

# hard code in points
#point<-data.frame(36.176936,-86.776413) # Nashville
# point<-data.frame(41.676432,-93.544581) # Des Moines
# colnames(point)<-c("y","x")
# 
# # purple loosestrife locations
# point<-data.frame(45.4856,-122.855) # location 1
# #point<-data.frame(40.79759,-76.0407) # location 2
# #point<-data.frame(39.55005,-105.782) # location 3
# point<-data.frame(40.783138,-81.917193) # Herm's data 40.783138, -81.917193
# colnames(point)<-c("y","x")

# points from Theresa
point<-data.frame(44.92456,-92.7919) # Herm's data 
point<-data.frame(46.99674,-94.6966)
point<-data.frame(40.37309,-74.1211)
point<-data.frame(42.32657,-70.9265)
point<-data.frame(42.26768,-70.9213)
point<-data.frame(44.84614,-93.2321)
#
colnames(point)<-c("y","x")


# make map
# ggmap key
source('~/RProjects/RainlogAPI/APIkey.R')
map<-get_map(location = c(lon = point$x, lat = point$y), 
             zoom = 3, source = "google", maptype = "terrain") %>% ggmap() +
  geom_point(data = point, aes(x = x, y = y), color = 'red', size = 3)

# get time series
gdd50ts<-t(raster::extract(gdd50_x4, cellFromXY(gdd50_x4, c(point$x,point$y))))
gdd250ts<-t(raster::extract(gdd250_x4, cellFromXY(gdd250_x4, c(point$x,point$y))))
gdd450ts<-t(raster::extract(gdd450_x4, cellFromXY(gdd450_x4, c(point$x,point$y))))
# get means
mean50<-t(raster::extract(meanDOY50, cellFromXY(meanDOY50, c(point$x,point$y))))
mean250<-t(raster::extract(meanDOY250, cellFromXY(meanDOY250, c(point$x,point$y))))
mean450<-t(raster::extract(meanDOY450, cellFromXY(meanDOY450, c(point$x,point$y))))

gddTS<-as.data.frame(cbind(seq(1948, 2016, by=1),gdd50ts,gdd250ts,gdd450ts))
colnames(gddTS)<-c("years","gdd50","gdd250","gdd450")
  gddTS$zProd<-scale(gddTS$gdd50, center=TRUE, scale=TRUE)*scale(gddTS$gdd450, center=TRUE, scale=TRUE)
  gddTS$gdd50<-mean50+gddTS$gdd50
  gddTS$gdd250<-mean250+gddTS$gdd250
  gddTS$gdd450<-mean450+gddTS$gdd450
gddDiffTS<-as.data.frame(cbind(gddTS$years, gddTS$gdd450-gddTS$gdd50)) 
  colnames(gddDiffTS)<-c("years","gdd450.50Diff")


# models and predictions 50 to 450
  library(jtools)
  gdd.model = lm(gdd450 ~ gdd50, data=gddTS) # y~x
  summ(gdd.model)
    min(gddTS$gdd50)
    max(gddTS$gdd50)
  #effect_plot(gdd.model, pred = gdd50, interval = TRUE, plot.points = TRUE)
    new.gdd = data.frame(gdd50=47)
    #new.gdd=data.frame(gdd50=floor(runif(10, min=min(gddTS$gdd50), max=max(gddTS$gdd50))))
  pred.gdd<-predict(gdd.model, new.gdd, interval="predict") 
  pred.range<-mean(pred.gdd[,3]-pred.gdd[,2])
  
# from http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/ 
  # 1. Add predictions 
  pred.int <- predict(gdd.model, interval = "prediction")
  mydata <- cbind(gddTS, pred.int)
  # 2. Regression line + confidence intervals
  library("ggplot2")
  p <- ggplot(mydata, aes(gdd50, gdd450)) +
    geom_point() +
    stat_smooth(method = lm)
  # 3. Add prediction intervals
  p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y = upr), color = "red", linetype = "dashed")+
    ggtitle("GDD50vGDD450 with Conf/Prediction Intervals")
  
  combined<-cbind(gddTS,pred.int)
  #write_clip(combined, return_new = TRUE)
  
  # Nash-Sutcliffe
  #library(hydroGOF)
  #NSE(sim=mydata$fit, obs=mydata$gdd450)
  
  # models and predictions 250 to 450
  library(jtools)
  gdd.model = lm(gdd450 ~ gdd250, data=gddTS) # y~x
  summ(gdd.model)
  min(gddTS$gdd250)
  max(gddTS$gdd250)
  #effect_plot(gdd.model, pred = gdd50, interval = TRUE, plot.points = TRUE)
  new.gdd = data.frame(gdd250=94)
  #new.gdd=data.frame(gdd250=floor(runif(10, min=min(gddTS$gdd250), max=max(gddTS$gdd250))))
  pred.gdd<-predict(gdd.model, new.gdd, interval="predict") 
  pred.range<-mean(pred.gdd[,3]-pred.gdd[,2])
  
  # from http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/ 
  # 1. Add predictions 
  pred.int <- predict(gdd.model, interval = "prediction")
  mydata <- cbind(gddTS, pred.int)
  # 2. Regression line + confidence intervals
  library("ggplot2")
  p <- ggplot(mydata, aes(gdd250, gdd450)) +
    geom_point() +
    stat_smooth(method = lm)
  # 3. Add prediction intervals
  p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y = upr), color = "red", linetype = "dashed")+
    ggtitle("GDD250vGDD450 with Conf/Prediction Intervals")
  
  combined2<-cbind(gddTS,pred.int)
  
  
  # get PRISM GDD for lat/lon
  library(RCurl)
  library(jsonlite)
  lon<- -86.776413
  lat<- 36.176936
  base<-50
  sdate<-"2018-01-01"
  edate<-"2018-12-31"  
  
  jsonQuery=paste0('{"loc":"',lon,',',lat,'","grid":"21","elems":"gdd',base,',pcpn","sdate":"',sdate,'","edate":"',edate,'"}')
  out<-postForm("http://data.rcc-acis.org/GridData", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  temp<-as.data.frame(out$data)
  temp$cumGDD<-cumsum(as.numeric(as.character(temp$V2)))
  # fibd days
  which.min(temp$cumGDD<=50)
  which.min(temp$cumGDD<=250)
  which.min(temp$cumGDD<=450)
  
  
  