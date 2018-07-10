# CORRELATION UNCERTAINTY
# Cluster time series from zonal stats
# code taken from plot_TopoWx_GDD_classifications
# further adapted from clusterTimeSeriesPlots.R
# MAC 04/12/2018

library(cowplot)
library(raster)
library(rasterVis)
library(sp)
library(maptools)
library(RStoolbox)
library(tidyr)
library(dplyr)

# set rasteroptions
#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')
#rasterOptions(todisk = TRUE)
tmpDir(create=TRUE)

# rsToolbox options
rsOpts(verbose=TRUE)

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)

# save classMap
load("./fixed/cluster12classMap.RData")
clusterN<-12
# load mask
maskNA<-raster("./fixed/maskNA.grd") 

# try zonal stats by cluster map
gdd50raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh50.grd")
gdd50raw_x4 <- mask(gdd50raw_x4, maskNA)
gdd250raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh250.grd")
gdd250raw_x4 <- mask(gdd250raw_x4, maskNA)
gdd450raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh450.grd")
gdd450raw_x4 <- mask(gdd450raw_x4, maskNA)
# set names
names(gdd50raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")
allGddraw<-stack(gdd50raw_x4,gdd250raw_x4,gdd450raw_x4)
rm(gdd50raw_x4,gdd250raw_x4,gdd450raw_x4)

# --- plot with sd intervals
# get zonal mean of each cluster
zStats<-as.data.frame(t(zonal(allGddraw, classMap, 'mean'))) # mean or median
zStats<-zStats[-1,]
zStats$yearLayer<-rownames(zStats)
zStats<-zStats %>% gather(yearLayer, 1:clusterN)
colnames(zStats)<-c("code","cluster","GDDValue")
zStats<-separate(zStats,code,c("X","code"), sep ="X")
zStats<-separate(zStats,code,c("year","threshold"))
zStats$year<-as.numeric(zStats$year)

clusterNames<-c("S Rockies","N Rockies","Northeast","Pacific NW","S Plains",
                "C Plains","N Plains","Southeast","Ohio Valley","Upper Midwest",
                "Gulf Coast","Southwest")
nameLUP<-as.data.frame(cbind(paste0("V",as.character(seq(1, clusterN, by=1))),c("S Rockies","N Rockies","Northeast","Pacific NW","S Plains",
                                                                                "C Plains","N Plains","Southeast","Ohio Valley","Upper Midwest",
                                                                                "Gulf Coast","Southwest"))) 
# lookup table to add in names
new<-zStats
new[] <- nameLUP$V2[match(unlist(zStats), nameLUP$V1)]
zStats$cluster<-new$cluster
# factor order 
zStats$cluster<-factor(zStats$cluster, levels=c("Southwest","S Rockies","Pacific NW","N Rockies","S Plains","C Plains",
                                                "N Plains","Upper Midwest","Gulf Coast","Southeast","Ohio Valley","Northeast"))

# get zonal sdev
for (i in seq(from=1, to=207, by=3)){
  j=i+2
  temp<-zonal(allGddraw[[i:j]], classMap, 'sd')
  if (i==1){
    zStdev <- temp
  }else{
    zStdev <- cbind(zStdev, temp[,2:4]) # bind
  }
}

zStdev<-as.data.frame(t(zStdev))
zStdev<-zStdev[-1,]
zStdev$yearLayer<-rownames(zStdev)
zStdev<-zStdev %>% gather(yearLayer, 1:clusterN)
colnames(zStdev)<-c("code","cluster","GDDValue")
zStdev<-separate(zStdev,code,c("X","code"), sep ="X")
zStdev<-separate(zStdev,code,c("year","threshold"))
zStdev$year<-as.numeric(zStdev$year)

# get sdev intervals
zStats$sdev<-zStdev$GDDValue

# ggplot correlation heatmaps ----
# detrend before correlating
library(pracma)
DTzStats <- zStats %>%
  group_by(threshold,cluster) %>% # add in threshold
  #summarise(corrs = cor(GDDValue, GDDValue))
  do(data.frame(GDDValue=detrend(.$GDDValue, 'linear')))
DTzStats$year<- zStats$year
meanGDDs<-spread(DTzStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,5,3,4)] #c(1,2,4,3,5)

