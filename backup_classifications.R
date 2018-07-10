# Cluster GDDs, plot
# TopoWx 12/20/17

library(cowplot)
library(raster)
library(rasterVis)
library(sp)
library(maptools)
library(RStoolbox)
library(tidyr)
library(dplyr)


# set rasteroptions
rasterOptions(progress = 'text')

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)

# # classify with 50/200/500 combo
# gdd50raw_x4<-stack("X4_rawDOY_baseT10_thresh50.grd")
# gdd200raw_x4<-stack("X4_rawDOY_baseT10_thresh200.grd")
# gdd500raw_x4<-stack("X4_rawDOY_baseT10_thresh500.grd")
# # set names
# names(gdd50raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
# names(gdd200raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD200")
# names(gdd500raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD500")


# # # or anoms
maskNA<-raster("maskNA.grd")
gdd50_x4<-stack("X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd50_x4 <- mask(gdd50_x4, maskNA)
gdd200_x4<-stack("X4_anomDOY_baseT10_thresh200_1981-2010.grd")
gdd200_x4 <- mask(gdd200_x4, maskNA)
gdd500_x4<-stack("X4_anomDOY_baseT10_thresh500_1981-2010.grd")
gdd500_x4 <- mask(gdd500_x4, maskNA)
# add in 
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

# # kendall corr
# allGdd<-stack(gdd50raw_x4,gdd200raw_x4,gdd500raw_x4) # raw
# allGdd<-stack(gdd50_x4,gdd200_x4,gdd500_x4) # anom
# corRaster<-corLocal(gdd200_x4,gdd500_x4, test=TRUE, method="kendall")
# my.at <- seq(0, 1, 0.1)
# levelplot(corRaster[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD200x4-Anom/GDD500x4-Anom Kendall-Corr")+ 
#   layer(sp.polygons(states))
# 
# boxplot(corRaster[[1]],classMap)

# lat lon grids
# lonGrid <- init(gdd50_x4, 'x')
# latGrid <- init(gdd50_x4, 'y')

# try anoms vs raw....
allGdd<-stack(gdd50_x4,gdd200_x4,gdd500_x4, meanDOY50_x4,meanDOY200_x4,meanDOY500_x4)
#allGdd<-stack(gdd50_x4,gdd200_x4,gdd500_x4, lonGrid, latGrid)
# allGdd<-stack(gdd50raw_x4,gdd200raw_x4,gdd500raw_x4)

set.seed(1234)
clusterN<-8
unC <- unsuperClass(allGdd, nSamples=1000, nClasses=clusterN, nStarts = 5)

## Plots

# # 5 colors
# colors <- c("#A7A7A7",
#             "dodgerblue",
#             "firebrick",
#             "forestgreen",
#             "gold")

darkcols <- brewer.pal(clusterN, "Set1")

classMap<-as.factor(unC$map)
rat <- levels(classMap)[[1]]
rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 
levelplot(classMap, col.regions=darkcols, margin=FALSE, main="50/200/500 GDD Anom clustering")+
  layer(sp.polygons(states))
#  layer(sp.polygons(test))

## plot with cluster boundaries
my.at <- seq(-50, 50, 5)
#levelplot(gdd50, par.settings = RdBuTheme, at=my.at, main="GDD50-BaseT10")
levelplot(gdd50_x4[[67]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 Anoms (BaseT10-TopoWx,81-00 normal)")+
  layer(sp.polygons(states))+
  layer(sp.polygons(test))


## 


# time series plots
centers<-as.data.frame(t(unC$model$centers))
centers$yearLayer<-rownames(centers)
centers<-centers %>% gather(yearLayer, 1:clusterN)
colnames(centers)<-c("code","cluster","GDDValue")
centers<-separate(centers,code,c("X","code"), sep ="X")
centers<-separate(centers,code,c("year","threshold"))
centers$year<-as.numeric(centers$year)

# correlate centers with each pixel in each cluster? 

# get summary stats for plots
df.sd.GDD <- centers %>%
  group_by(cluster,threshold) %>% # add in threshold
  summarise(sdGDD = round(sd(GDDValue), 2))
# add positions
posLabel<-as.data.frame(do.call(rbind, replicate(clusterN, cbind(c(1960,1980,2000),c(200,200,200)), simplify=FALSE)))
df.sd.GDD<-cbind(as.data.frame(df.sd.GDD),posLabel)

#library(ggrepel)
ggplot(centers, aes(x=year,y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line() +
  geom_smooth(method = "lm")+
  ylim(c(0,200))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY for GDD Thresholds"),
       x ="Year", y = "Day of Year")+
  geom_text(x = 2000, y = 200, 
            aes(label = paste0("SD: ", sdGDD)), 
            data = df.sd.GDD, check_overlap = TRUE)


# # find optimal cluster number
clusterN=10
wss<-allGdd[[1]]@nrows*allGdd[[1]]@ncols
minwss<-allGdd[[1]]@nrows*allGdd[[1]]@ncols
totss<-allGdd[[1]]@nrows*allGdd[[1]]@ncols
btwss<-allGdd[[1]]@nrows*allGdd[[1]]@ncols
for (i in 5:clusterN){
  set.seed(1234)
  unC <- unsuperClass(allGdd, nSamples = 50000, nClasses = i, nStarts = 5)
  wss[i]<-unC$model$tot.withinss
  #minwss[i]<-min(unC$model$withinss)
  #totss[i]<-unC$model$totss
  #btwss[i]<-unC$model$betweenss
  print(i)
}
# plot(2:clusterN, wss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms w DOY added",
#        ylab="Within groups sum of squares", ylim=c(min(wss[2:clusterN]),max(wss[2:clusterN])))
# plot(2:clusterN, diff(wss), type="b", xlab="Number of Clusters - GDD Anoms w DOY added",
#      ylab="Diff Within groups sum of squares", ylim=c(min(diff(wss)),max(diff(wss))))


# # polygon
cluster8poly<-rasterToPolygons(classMap, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
writePolyShape(cluster8poly, "./mapFiles/cluster8poly")
test <- readShapePoly("./mapFiles/cluster8poly")
# 
# levelplot(classMap, par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Kendall-Corr")+ 
#   layer(sp.polygons(test))

# try zonal stats by cluster map
gdd50raw_x4<-stack("X4_rawDOY_baseT10_thresh50.grd")
gdd50raw_x4 <- mask(gdd50raw_x4, maskNA)
gdd200raw_x4<-stack("X4_rawDOY_baseT10_thresh200.grd")
gdd200raw_x4 <- mask(gdd200raw_x4, maskNA)
gdd500raw_x4<-stack("X4_rawDOY_baseT10_thresh500.grd")
gdd500raw_x4 <- mask(gdd500raw_x4, maskNA)
# set names
names(gdd50raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd200raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD200")
names(gdd500raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD500")
allGddraw<-stack(gdd50raw_x4,gdd200raw_x4,gdd500raw_x4)

# get zonal mean of each cluster
zStats<-as.data.frame(t(zonal(allGddraw, classMap, 'mean')))
zStats<-zStats[-1,]
zStats$yearLayer<-rownames(zStats)
zStats<-zStats %>% gather(yearLayer, 1:clusterN)
colnames(zStats)<-c("code","cluster","GDDValue")
zStats<-separate(zStats,code,c("X","code"), sep ="X")
zStats<-separate(zStats,code,c("year","threshold"))
zStats$year<-as.numeric(zStats$year)

ggplot(zStats, aes(x=year,y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line() +
  geom_smooth(method = "lm")+
  ylim(c(0,200))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY for GDD Thresholds by Cluster"),
       x ="Year", y = "Day of Year")

# --- plot with sd intervals
# get zonal mean of each cluster
zStats<-as.data.frame(t(zonal(allGddraw, classMap, 'mean')))
zStats<-zStats[-1,]
zStats$yearLayer<-rownames(zStats)
zStats<-zStats %>% gather(yearLayer, 1:clusterN)
colnames(zStats)<-c("code","cluster","GDDValue")
zStats<-separate(zStats,code,c("X","code"), sep ="X")
zStats<-separate(zStats,code,c("year","threshold"))
zStats$year<-as.numeric(zStats$year)

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
zStats$sdevPos<-zStats$GDDValue+zStdev$GDDValue
zStats$sdevNeg<-zStats$GDDValue-zStdev$GDDValue

# get sdevs for each cluster
df.sd.GDD <- zStdev %>%
  group_by(cluster, threshold) %>% # add in threshold
  summarise(sdGDD = round(sd(GDDValue), 2))

ggplot(zStats, aes(x=year,y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line() +
  #geom_smooth(method = "lm")+
  geom_ribbon(aes(ymax = sdevPos, ymin = sdevNeg,linetype=NA), alpha = 0.3)+
  ylim(c(-5,205))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY for GDD Thresholds by Cluster"),
       x ="Year", y = "Day of Year")+
  geom_text(x = 1980, y = 200, 
            aes(label = paste0("200 GDD SD: ", sdGDD)), 
            data = df.sd.GDD, check_overlap = TRUE)

# get sdevs for each cluster
df.sd.GDD <- zStdev %>%
  group_by(cluster) %>% # add in threshold
  summarise(sdGDD = round(sd(GDDValue), 2))

#showTmpFiles()
#removeTmpFiles(h=0) # in hours

# zonal stats for clusters
# get some data 
elev<-raster("./mapFiles/X4_elev.grd")
# need to resample to X4
#normTmin<-stack("./mapFiles/normals_tmin.nc", varname="tmin_normal")
#normTmax<-stack("./mapFiles/normals_tmax.nc", varname="tmax_normal")

clusterStats<-as.data.frame(cbind(freq(classMap, useNA="no"),zonal(elev, classMap, 'median'),zonal(elev, classMap, 'sd'),
                                  zonal(meanDOY50_x4, classMap, 'median'),zonal(meanDOY50_x4, classMap, 'sd'),
                                  zonal(meanDOY200_x4, classMap, 'median'),zonal(meanDOY200_x4, classMap, 'sd'),
                                  zonal(meanDOY500_x4, classMap, 'median'),zonal(meanDOY500_x4, classMap, 'sd')))

clusterStats <- clusterStats[c(-3,-5,-7,-9,-11,-13,-15,-17)]
colnames(clusterStats)<-c("cluster","#pixels","medElev","sdevElev","doy50med","doy50sdev","doy200med","doy200sdev","doy500med","doy500sdev")
