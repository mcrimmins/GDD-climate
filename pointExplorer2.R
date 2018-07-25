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

# make map
map<-get_map(location = c(lon = point$x, lat = point$y), 
             zoom = 3, source = "google", maptype = "terrain") %>% ggmap() +
  geom_point(data = point, aes(x = x, y = y), color = 'red', size = 3)

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
gddDiffTS<-as.data.frame(cbind(gddTS$years, gddTS$gdd450-gddTS$gdd50)) 
  colnames(gddDiffTS)<-c("years","gdd450.50Diff")

# GDD threshold plots
gddTS<-melt(gddTS,measure.vars = c("gdd50","gdd450"))
p<-ggplot(gddTS, aes(value,as.factor(years), color=zProd)) + 
   geom_line(size = 1)+
   geom_vline(xintercept = mean50[[1]])+
   geom_vline(xintercept = mean450[[1]])+
   xlim(0,220)+
  scale_y_discrete(breaks=c("1950","1960","1970","1980","1990","2000","2010"),
                       labels=c("1950","1960","1970","1980","1990","2000","2010"))+
  annotate("text", x = 2, y = 6, label = paste0("r=",round(cor(gdd50ts,gdd450ts),2)))+
  annotate("text", x = 2, y = 9, label = paste0("df=",round(sd(gdd450ts-gdd50ts),2)))+
  annotate("text", x = 2, y = 12, label = paste0("50=",round(sd(gdd50ts),2)))+
  scale_color_gradient2(low = "orange", mid="grey94", high = "green", limits=c(-1,1), oob=squish)

ggdraw() +
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(map, 0.65, 0.05, 0.5, 0.4, scale=0.8)


boxP<-ggplot(gddTS, aes(variable, value)) + 
  geom_boxplot(notch = FALSE, fatten=3) +
  geom_hline(yintercept = mean50[[1]])+
  geom_hline(yintercept = mean450[[1]])+
  ylim(0,180)+
  coord_flip()
  
boxDiff<-ggplot(gddDiffTS, aes(x="",y=gdd450.50Diff))+
  geom_boxplot()+
  ylim(0,120)+
  coord_flip()



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


