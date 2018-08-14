# FIGURE 3
# Season bar plots - from pointExplorerFig.R
# MAC 08/12/2018

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

# points
# 35.983498, -89.127785  
#x<-c(-89.127785, -88.006413)
#y<-c(35.983498,  41.281065)
#x<-c(-81.322725, -87.199672, -92.672159)
#y<-c(27.011566, 36.178183, 41.947674)
# x<-c(-81.322725, -87.199672, -92.672159, -112.072393, -115.130783, -110.709217)
# y<-c(27.011566, 36.178183, 41.947674, 32.314482, 41.470576, 44.710696)

# east-lat transect
# orlando 28.553918, -81.386595, atlanta 33.723028, -84.382518,
# nashville 36.176936, -86.776413, Peoria 40.646502, -89.562244, Des Moines 41.676432, -93.544581
# Minneapolis 44.973139, -93.265222
x<-c(-81.386595, -84.382518, -86.776413, -93.544581, -85.327310)
y<-c(28.553918, 33.723028, 36.176936, 41.676432, 46.612546)
# point in upper peninsula
#46.612546, -85.327310
#x<-c(-85.327310)
#y<-c(46.612546)

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
gdd50corr$xpos<-260
gdd50corr$ypos<-Inf  
gdd50corr$letter<-c('a','c','e','g','i','b','d','f','h','j')
gdd50corr$letterX<--10
gdd50corr$letterY<-Inf # or Inf 
gdd50corr$corrLabel<-sprintf("%.2f", gdd50corr$corr)


p<-ggplot(gddTSall, aes(value,as.factor(years), color=zValue)) + 
  geom_line(size = 0.15, lineend = "round")+
  geom_point(size=0.1)+
  geom_vline(aes(xintercept=mean50), data=meansall, size=0.15, color='black') +
  geom_vline(aes(xintercept=mean450), data=meansall, size=0.15, color='black') +
  facet_grid(point~base)+
  scale_y_discrete(breaks=c("1950","1960","1970","1980","1990","2000","2010"),
                   labels=c("1950","1960","1970","1980","1990","2000","2010"))+
  scale_color_gradient2(low = "firebrick3", mid="grey65", high = "royalblue3", limits=c(-1,1), oob=squish,
                        name="Z Product")+
  # low = "orangered4", mid="grey65", high = "green4"
  xlim(-10, 285)+
  xlab("Day of Year") + ylab("Year")+ 
  theme_bw(base_size = 10, base_family = "Helvetica")+
  theme(legend.position="bottom",legend.direction="horizontal", legend.justification = "center",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        strip.background = element_blank(),
        strip.text.x = element_blank())+ # strip.text.y = element_blank()
        #legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5))


# p + geom_text(data=gdd50sd, aes(x=xpos, y=ypos, 
#                            label=paste0("SD: ", sd)), color="black",  size=3,
#            vjust=2, parse=FALSE)

pALL<-p + geom_text(data=gdd50corr, aes(x=xpos, y=ypos, 
                                  label=paste0("r= ", corrLabel)), color="black",  size=3,
              vjust=2, parse=FALSE) +
          geom_text(data=gdd50corr, aes(x=letterX, y=letterY, 
                                label=letter), color="black",  size=3,
            vjust=2, parse=FALSE)

ggsave(plot = pALL, width = 4, height = 5, units = "in",dpi = 300, filename = "./figs/fig3.png")

# make map
map<-get_map(location = "USA", 
             zoom = 4, source = "google", maptype = "terrain") %>% ggmap() +
  #geom_point(data = point, aes(x = x, y = y, label=seq(1,j,1)), color = 'red', size = 3)
  geom_label(data = point, aes(x = x, y = y, label=seq(1,j,1)),size=0.1)

#ggsave(plot = map, width = 2, height = 2, units = "in",dpi = 300, filename = "./figs/fig3map.png")

# ggplot inset data
states <- map_data("state")

# inset map:
insetmap<-ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey")  + # get the state border back on top
  coord_fixed(xlim=c(-100, -67), ylim=c(25, 50), ratio = 1.3) +
  geom_point(data = point, aes(x = x, y = y), size=1, color='red')+
  geom_text(data = point, aes(x = x, y = y, label=seq(1,j,1)),size=3, nudge_x = -2.5, nudge_y = 0, color='red')+
  theme_bw(base_size=5)
 
#ylab("") +
  #xlab("") +
  #theme_nothing()

#ggdraw()+
#  draw_plot(pALL)+
#  draw_plot(insetmap, x = 0, y = 0, scale = 0.5)

#A viewport taking up a fraction of the plot area
vp <- viewport(width = 0.28, height = 0.28, x = 0.2, y = 0.11)

#Just draw the plot twice
png("./figs/test.png", width = 4, height = 5, units = "in", res = 300L)
print(pALL)
print(insetmap, vp = vp)
dev.off()
