# point/click explorer for Raw Grids
# MAC 4/26/18

library(tidyr) # masks extract...
library(raster)
library(rasterVis)
library(sp)
library(maptools)
library(ggmap)
library(cowplot)
#library(gridExtra)

# set rasteroptions
rasterOptions(progress = 'text')

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)

clusterpoly <- readShapePoly("./mapFiles/cluster15finalFIXED_ZProd")

# layerstats, correlation between z-diffs and corr map
load("./fixed/zscore50_450AnomDetrendedPearson.RData")
load("./fixed/zscore50_450gddStats.RData")

# Load raw daily grids -----
maskNA<-raster("./fixed/maskNAalt.grd") 
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
# ----

# point/click time series maps; add raw, anomalies and zscores...
plot(corRaster[[1]], 
     zlim=c(-1,1),
     breaks= seq(-1, 1, by = 0.1), 
     col=colorRampPalette(c("blue", "white", "red"))(length(seq(-1, 1, by = 0.1))-1),
     main="GDD50x4/GDD500x4 Pearson-Corr",
     legend.args=list(text='days', side=4, font=2, line=2.3))
#plot(states, add=TRUE)
plot(clusterpoly, add=TRUE)

point<-click(corRaster[[1]], n=1, xy=TRUE, show=FALSE, type="p")

# Dan Herms data
#Dow Gardens, Midland, MI: 43.623310, -84.252870
x<-c(-84.252870)
y<-c(43.623310)
#Secrist Arboretum, Wooster, OH: 40.783453, -81.917416
x<-c( -81.917416)
y<-c(40.783453)

point<-data.frame(y,x)

# make map
map<-get_map(location = c(lon = point$x, lat = point$y), 
        zoom = 5, source = "google", maptype = "terrain") %>% ggmap() +
  geom_point(data = point, aes(x = x, y = y), color = 'red', size = 3)


gdd50ts<-t(raster::extract(gdd50raw_x4, cellFromXY(gdd50raw_x4, c(point$x,point$y))))
gdd250ts<-t(raster::extract(gdd250raw_x4, cellFromXY(gdd250raw_x4, c(point$x,point$y))))
gdd450ts<-t(raster::extract(gdd450raw_x4, cellFromXY(gdd450raw_x4, c(point$x,point$y))))

gddTS<-as.data.frame(cbind(seq(1948, 2016, by=1),gdd50ts,gdd250ts,gdd450ts))
colnames(gddTS)<-c("years","gdd50","gdd250","gdd450")
#cor(gddTS$gdd50,gddTS$gdd450)

# zScores
gddZ<-as.data.frame(cbind(gddTS$years,scale(gddTS[,2:4],center = TRUE, scale = TRUE)))
colnames(gddZ)[1]<-"years"
Zscores<-scale(gddTS[,c(2,4)],center = TRUE, scale = TRUE)
zStats<-as.data.frame(attr(Zscores,"scaled:scale"))
colnames(zStats)[1]<-"sdev"
zStats<-round(zStats,1)

# zDiff calcs changed to product
gddZ$gdd450_50diff<-gddZ$gdd450*gddZ$gdd50
diffSD<-round(sd(gddZ$gdd450_50diff),2)
corrDiff<-round(cor(gddZ$gdd450_50diff,gddZ$gdd50),2)

# zscores
#gddZ<-gddZ %>% gather(years,2:4)
#colnames(gddZ)[2:3]<-c("threshold","values")

zScatter<-ggplot(gddZ, aes(x=gdd50,y=gdd450)) +
  geom_point()+
  ylim(c(-4,4))+
  xlim(c(-4,4))+
  background_grid(major = "xy", minor = "xy")+
  geom_smooth(method = "lm", se = TRUE)+
  annotate(x=-3.5, y=3.5, 
           label=paste("r = ", round(cor(gddZ$gdd50,gddZ$gdd450),2)), 
           geom="text", size=5)+
  labs(title="early/late gdd Zscores",
       x ="GDD50z", y = "GDD450z")+
  geom_abline(intercept = 0, slope = 1)+
  geom_hline(yintercept=0, color='grey')+
  geom_vline(xintercept=0, color='grey')

# gdd diff raw time series 
gddRawDiff<-as.data.frame(cbind(gddTS$years,gddTS$gdd450-gddTS$gdd50))

rawDiffPlot<-ggplot(gddRawDiff,aes(x=V1,y=V2))+
  geom_line()+
  ylim(c(0,120))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title="Diff of GDD450/50 ",
       x ="Year", y = "days")+
  annotate(x=1955, y=15, label=paste("stdev = ", round(sd(gddRawDiff$V2),2)), 
                                        geom="text", size=5)+
  geom_hline(yintercept=round(mean(gddRawDiff$V2),2), color='grey')

# ggplot time series
gddTS<-gddTS %>% gather(years,2:4)
colnames(gddTS)[2:3]<-c("threshold","values")

doyPlot<-ggplot(gddTS, aes(x=years,y=values, color=factor(threshold))) +
  geom_line() +
  ylim(c(0,250))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title="DOY for GDD Thresholds",
       x ="Year", y = "Day of Year")+
  theme(legend.position = "bottom",
        legend.title = element_text(size=8),
        legend.text = element_text(size=8))
        
  # theme(legend.position = c(0, 0.25),
  #       legend.title = element_text(size=8),
  #       legend.text = element_text(size=8))

# zscores
gddZ<-gddZ %>% gather(years,2:4)
colnames(gddZ)[2:3]<-c("threshold","values")

zPlot<-ggplot(subset(gddZ,threshold %in% c("gdd50" , "gdd450")))+
  geom_line(aes(x=years,y=values, color=factor(threshold)))+
  ylim(c(-4,4))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title="Z-score of GDD50 and GDD450",
       x ="Year", y = "z-score")+
  theme(legend.position = "bottom",
        legend.title = element_text(size=8),
        legend.text = element_text(size=8))+
  annotate(x=1960, y=-3.5, label=paste("stdev50/450 = ", zStats$sdev[1],",",zStats$sdev[2]), 
           geom="text", size=5)+
  geom_hline(yintercept=0, color='grey')

diffPlot<-ggplot(subset(gddZ,threshold %in% c("gdd50","gdd450_50diff")))+
  geom_line(aes(x=years,y=values, color=factor(threshold)))+
  ylim(c(-4,4))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Dark2")+
  labs(title="Z-score of GDD50 and GDD450*50",
       x ="Year", y = "z-score")+
  annotate(x=1955, y=-3.5, 
         label=paste("diff stdev = ",diffSD), 
         geom="text", size=5)+
  annotate(x=1980, y=-3.5, label=paste("corr= ", corrDiff), 
           geom="text", size=5)+
  geom_hline(yintercept=0, color='black')

# zPlot<-ggplot(gddZ, aes(x=years,y=values, color=factor(threshold))) +
#   geom_line() +
#   ylim(c(-4,4))+
#   facet_wrap(~threshold, nrow = 3)+
#   background_grid(major = "xy", minor = "xy")+
#   scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
#   labs(title="DOY for GDD Thresholds at",
#        x ="Year", y = "Day of Year")

# make plot
plot_grid(doyPlot,diffPlot,rawDiffPlot,zScatter,zPlot,map, ncol = 2)

testPlot<-plot_grid(doyPlot,diffPlot,rawDiffPlot,zScatter,zPlot,map, ncol = 2)

save_plot("plot2by2.png", testPlot,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)

# calc z-score time series, diff, pearson r
