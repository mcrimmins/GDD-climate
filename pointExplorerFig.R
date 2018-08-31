# point/click explorer for Raw Grids
# FOR DETRENDED GRIDS
# MAC 6/11/18

# generate multipanel bar plots of several locations
# 7/24/18 

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
load("./fixed/bothBaseT/detrendedBaseT0.RData")
#load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")
meanDOY50T0<-raster("./fixed/X4_meanDOY_baseT0_thresh50_1981-2010.grd")
meanDOY450T0<-raster("./fixed/X4_meanDOY_baseT0_thresh450_1981-2010.grd")
load("./fixed/bothBaseT/detrendedBaseT10.RData")
#load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
meanDOY50T10<-raster("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
meanDOY450T10<-raster("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
#corRaster<-corRasterDet_BaseT10

# remove unneeded vars
rm(gdd250_x4pval,gdd250_x4pval_0, gdd250_x4Slope, gdd250_x4Slope_0,
   gdd50_x4pval, gdd50_x4pval_0, gdd50_x4Slope, gdd50_x4Slope_0,
   gdd450_x4pval, gdd450_x4pval_0, gdd450_x4Slope, gdd450_x4Slope_0)

# load mask
maskNA<-raster("./fixed/maskNAalt.grd")
gdd50_x4T10 <- mask(gdd50_x4Resid, maskNA)
gdd450_x4T10 <- mask(gdd450_x4Resid, maskNA)
meanDOY50T10 <- mask(meanDOY50T10, maskNA)
meanDOY450T10 <- mask(meanDOY450T10, maskNA)

# base T0 mask
maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd")
gdd50_x4T0<- mask(gdd50_x4Resid_0, maskNA_0)
gdd450_x4T0<- mask(gdd450_x4Resid_0, maskNA_0)
meanDOY50T0 <- mask(meanDOY50T0, maskNA_0)
meanDOY450T0 <- mask(meanDOY450T0, maskNA_0)

# point/click time series maps; add raw, anomalies and zscores...
# plot(corRaster[[1]], 
#      zlim=c(-1,1),
#      breaks= seq(-1, 1, by = 0.1), 
#      col=colorRampPalette(c("blue", "white", "red"))(length(seq(-1, 1, by = 0.1))-1),
#      main="GDD50x4/GDD450x4 Detrended Pearson-Corr",
#      legend.args=list(text='r', side=4, font=2, line=2.3))
# plot(states, add=TRUE)
# 
# point<-click(corRaster[[1]], n=1, xy=TRUE, show=FALSE, type="p")

# # make map
# map<-get_map(location = c(lon = point$x, lat = point$y), 
#              zoom = 5, source = "google", maptype = "terrain") %>% ggmap() +
#   geom_point(data = point, aes(x = x, y = y), color = 'red', size = 3)

# points
# 35.983498, -89.127785  
x<-c(-89.127785, -88.006413)
y<-c(35.983498,  41.281065)
#x<-c(-81.322725, -87.199672, -92.672159)
#y<-c(27.011566, 36.178183, 41.947674)
# x<-c(-81.322725, -87.199672, -92.672159, -112.072393, -115.130783, -110.709217)
# y<-c(27.011566, 36.178183, 41.947674, 32.314482, 41.470576, 44.710696)

# east-lat transect
# orlando 28.553918, -81.386595, atlanta 33.723028, -84.382518,
# nashville 36.176936, -86.776413, Peoria 40.646502, -89.562244, Des Moines 41.676432, -93.544581
# Minneapolis 44.973139, -93.265222
x<-c(-81.386595, -86.776413, -93.544581)
y<-c(28.553918, 36.176936, 41.676432)
# point in upper peninsula
#46.612546, -85.327310
x<-c(-85.327310)
y<-c(46.612546)
# NW Nevada
#41.834227, -119.316589
x<-c(-119.316589)
y<-c(41.834227)
# Wind River WY 43.395436, -109.026911
x<-c(-109.026911)
y<-c(43.395436)

# CA mountain vs CO mountain
#39.195307, -106.494818
#38.900513, -120.151040
x<-c(-106.494818, -120.151040)
y<-c(39.195307, 38.900513)


point<-data.frame(y,x)

for(j in 1:nrow(point)){

  # get time series - BaseT0
  gdd50tsT0<-t(raster::extract(gdd50_x4T0, cellFromXY(gdd50_x4T0, c(point$x[j],point$y[j]))))
  gdd450tsT0<-t(raster::extract(gdd450_x4T0, cellFromXY(gdd450_x4T0, c(point$x[j],point$y[j]))))
  # get means
  mean50T0<-t(raster::extract(meanDOY50T0, cellFromXY(meanDOY50T0, c(point$x[j],point$y[j]))))
  mean450T0<-t(raster::extract(meanDOY450T0, cellFromXY(meanDOY450T0, c(point$x[j],point$y[j]))))
  
  # get time series - BaseT10
  gdd50tsT10<-t(raster::extract(gdd50_x4T10, cellFromXY(gdd50_x4T10, c(point$x[j],point$y[j]))))
  gdd450tsT10<-t(raster::extract(gdd450_x4T10, cellFromXY(gdd450_x4T10, c(point$x[j],point$y[j]))))
  # get means
  mean50T10<-t(raster::extract(meanDOY50T10, cellFromXY(meanDOY50T10, c(point$x[j],point$y[j]))))
  mean450T10<-t(raster::extract(meanDOY450T10, cellFromXY(meanDOY450T10, c(point$x[j],point$y[j]))))

gddTS<-as.data.frame(cbind(seq(1948, 2016, by=1),gdd50tsT0,gdd450tsT0,gdd50tsT10,gdd450tsT10))
colnames(gddTS)<-c("years","gdd50T0","gdd450T0","gdd50T10","gdd450T10")
  # Base T0
  gddTS$zProdT0<-scale(gddTS$gdd50T0, center=TRUE, scale=TRUE)*scale(gddTS$gdd450T0, center=TRUE, scale=TRUE)
  gddTS$gdd50T0<-mean50T0+gddTS$gdd50T0
  gddTS$gdd450T0<-mean450T0+gddTS$gdd450T0
  # Base T10
  gddTS$zProdT10<-scale(gddTS$gdd50T10, center=TRUE, scale=TRUE)*scale(gddTS$gdd450T10, center=TRUE, scale=TRUE)
  gddTS$gdd50T10<-mean50T10+gddTS$gdd50T10
  gddTS$gdd450T10<-mean450T10+gddTS$gdd450T10
#gddDiffTS<-as.data.frame(cbind(gddTS$years, gddTS$gdd450-gddTS$gdd50)) 
#  colnames(gddDiffTS)<-c("years","gdd450.50Diff")

  # GDD threshold plots
  gddTS<-melt(gddTS,measure.vars = c("gdd50T0","gdd450T0","gdd50T10","gdd450T10"))
  gddTS<-separate(gddTS,variable,c("thresh","base"), sep ="T")
  gddTS$point<-j
  gddTS<-melt(gddTS, measure.vars=c("zProdT0", "zProdT10"))
  colnames(gddTS)[7]<-"zValue"
  # thin out df
  gddTS <- gddTS[-which(gddTS$base=='0' & gddTS$variable=='zProdT10'), ]
  gddTS <- gddTS[-which(gddTS$base=='10' & gddTS$variable=='zProdT0'), ]
  

    # combine frames
    if (j==1){
      gddTSall <- gddTS
    }else{
      gddTSall <- rbind(gddTSall, gddTS) # brick or stack?
    }

  # create means
    meansT0<-data.frame(cbind("0",j,mean50T0,mean450T0))
    meansT10<-data.frame(cbind("10",j,mean50T10,mean450T10))
    means<-rbind(meansT0, meansT10)
    colnames(means)<-c("base","point","mean50", "mean450")
    # combine frames
    if (j==1){
      meansall <- means
    }else{
      meansall <- rbind(meansall, means) # brick or stack?
    }
    meansall$mean50<-as.numeric(as.character(meansall$mean50))
    meansall$mean450<-as.numeric(as.character(meansall$mean450))
}

# get group stats
library(dplyr)
# get SDs 
# gddTSall.sd <-gddTSall %>%
#   group_by(thresh,base,point) %>%
#   summarise(sd = round(sd(value), 2), zValue=round(mean(zValue), 2))
# gdd50sd <- gddTSall.sd[ which(gddTSall.sd$thresh=="gdd50"),]
#   gdd50sd$xpos<-150
#   gdd50sd$ypos<-Inf
# get SDs 
gddTSall.corr <-gddTSall %>%
  group_by(thresh,base,point) %>%
  summarise(corr=round(sum(zValue)/69, 2), zValue=round(mean(zValue), 2))
gdd50corr <- gddTSall.corr[ which(gddTSall.corr$thresh=="gdd50"),]
  gdd50corr$xpos<-175
  gdd50corr$ypos<-Inf  
  


p<-ggplot(gddTSall, aes(value,as.factor(years), color=zValue)) + 
  geom_line(size = 1, lineend = "round")+
  geom_point(size=0.5)+
  geom_vline(aes(xintercept=mean50), data=meansall) +
  geom_vline(aes(xintercept=mean450), data=meansall) +
  facet_grid(point~base)+
  scale_y_discrete(breaks=c("1950","1960","1970","1980","1990","2000","2010"),
                   labels=c("1950","1960","1970","1980","1990","2000","2010"))+
  scale_color_gradient2(low = "orangered4", mid="grey65", high = "green4", limits=c(-1,1), oob=squish)+
  theme_bw()

# p + geom_text(data=gdd50sd, aes(x=xpos, y=ypos, 
#                            label=paste0("SD: ", sd)), color="black",  size=3,
#            vjust=2, parse=FALSE)
 
p + geom_text(data=gdd50corr, aes(x=xpos, y=ypos, 
                                label=paste0("r= ", corr)), color="black",  size=3,
              vjust=2, parse=FALSE) 


# make map
map<-get_map(location = "USA", 
             zoom = 4, source = "google", maptype = "terrain") %>% ggmap() +
  #geom_point(data = point, aes(x = x, y = y, label=seq(1,j,1)), color = 'red', size = 3)
  geom_label(data = point, aes(x = x, y = y, label=seq(1,j,1)))





# OLD code from pointExplorer2.R
# p<-ggplot(gddTS, aes(value,as.factor(years), color=zProdT10)) + 
#    geom_line(size = 1)+
#    geom_vline(xintercept = mean50[[1]])+
#    geom_vline(xintercept = mean450[[1]])+
#    xlim(0,220)+
#   scale_y_discrete(breaks=c("1950","1960","1970","1980","1990","2000","2010"),
#                        labels=c("1950","1960","1970","1980","1990","2000","2010"))+
#   annotate("text", x = 2, y = 6, label = paste0("r=",round(cor(gdd50ts,gdd450ts),2)))+
#   annotate("text", x = 2, y = 9, label = paste0("df=",round(sd(gdd450ts-gdd50ts),2)))+
#   annotate("text", x = 2, y = 12, label = paste0("50=",round(sd(gdd50ts),2)))+
#   scale_color_gradient2(low = "orange", mid="grey94", high = "green", limits=c(-1,1), oob=squish)
# 
# ggdraw() +
#   draw_plot(p, 0, 0, 1, 1) +
#   draw_plot(map, 0.65, 0.05, 0.5, 0.4, scale=0.8)
# 
# 
# boxP<-ggplot(gddTS, aes(variable, value)) + 
#   geom_boxplot(notch = FALSE, fatten=3) +
#   geom_hline(yintercept = mean50[[1]])+
#   geom_hline(yintercept = mean450[[1]])+
#   ylim(0,180)+
#   coord_flip()
#   
# boxDiff<-ggplot(gddDiffTS, aes(x="",y=gdd450.50Diff))+
#   geom_boxplot()+
#   ylim(0,120)+
#   coord_flip()



#plot_grid(p, boxP,boxDiff, ncol = 1, align = 'v', axis = 'l')



# phenocal<-dataQ[,c("site_id","common_name","firstDate","lastDate")]
# phenocal<-melt(phenocal,measure.vars = c("firstDate","lastDate"))
# 
# ggplot(phenocal, aes(value, site_id, colour = common_name)) + 
#   geom_line(size = 6)
# 
# phenocal2<-dataQ[,c("common_name","first_yes_doy")]
# ggplot(phenocal2, aes(first_yes_doy, common_name)) + 
#   geom_point()


