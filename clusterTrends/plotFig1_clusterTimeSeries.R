# Cluster time series from zonal stats
# code taken from plot_TopoWx_GDD_classifications
# MAC 03/23/2018

library(cowplot)
library(raster)
library(rasterVis)
library(sp)
library(maptools)
library(RStoolbox)
library(tidyr)
library(dplyr)
library(ggplotify)
library(grid)


# set rasteroptions
#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')
#rasterOptions(todisk = TRUE)
#tmpDir(create=TRUE)

# rsToolbox options
rsOpts(verbose=TRUE)

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)
USA<-getData('GADM', country='USA', level=0)

# save classMap
#load("./clusterTrends/cluster12classMap.RData")
unC<-readRSTBX(filename="./clusterTrends/cluster12classMap")
clusterN<-12

# create polygon shapefile
# library(rgdal)
#  clusterpoly<-rasterToPolygons(unC$map, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
#  proj4string(clusterpoly) <- "+proj=longlat +datum=WGS84"
#  writeOGR(clusterpoly, ".", "./clusterTrends/cluster12final_ZProd", driver="ESRI Shapefile")
cluster12poly <- readShapePoly("./clusterTrends/cluster12final_ZProd")
zonePoly <- readShapePoly("./clusterTrends/threeZone")
zonePoly<-zonePoly[zonePoly$layer==2,] # only central region

# load mask
maskNA<-raster("./fixed/maskNAalt.grd") 

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

# CONTINENTAL TRENDS
temp<-cellStats(allGddraw, stat='mean',na.rm=TRUE)
  zUSA<-as.data.frame(temp)
  zUSA$yearLayer<-rownames(zUSA)
  zUSA<-separate(zUSA,yearLayer,c("X","code"), sep ="X")
  zUSA<-separate(zUSA,code,c("year","GDD"))
  zUSA$year<-as.numeric(zUSA$year)
  # # linear trends
  library("broom")
  usaLM <- zUSA %>%
    group_by(GDD)
  USATrends<-do(usaLM,glance(lm(temp ~ year, data = .)))
  USASlopes<-do(usaLM,tidy(lm(temp ~ year, data = .)))
  ggplot(zUSA, aes(year,temp))+
    geom_line()+
    facet_wrap(.~GDD)+
    stat_smooth(method="lm")
  # get slopes
  USASlopes<-USASlopes[which(USASlopes$term=="year"),]
  USASlopes<-USASlopes[,c(1:3)]
    USASlopes$estimate<-USASlopes$estimate*10
# --- plot with sd intervals
# get zonal mean of each cluster
zStats<-as.data.frame(t(zonal(allGddraw,unC$map, fun='mean'))) # mean or median
zStats<-zStats[-1,]
zStats$yearLayer<-rownames(zStats)
#zStats<-zStats %>% gather(yearLayer, 1:clusterN)
zStats<-zStats%>% gather("cluster","GDDValue",-yearLayer)
colnames(zStats)<-c("code","cluster","GDDValue")
zStats<-separate(zStats,code,c("X","code"), sep ="X")
zStats<-separate(zStats,code,c("year","threshold"))
zStats$year<-as.numeric(zStats$year)

clusterNames<-c("Southwest","N Rockies","Ohio Valley","S Plains","C Plains","Pacific NW",
                "Gulf Coast","N Plains","Southeast","Upper Midwest","Northeast","S Rockies")
nameLUP<-as.data.frame(cbind(paste0("V",as.character(seq(1, clusterN, by=1))),
                             c("Southwest","N Rockies","Ohio Valley","S Plains","C Plains","Pacific NW",
                               "Gulf Coast","N Plains","Southeast","Upper Midwest","Northeast","S Rockies"))) 
# lookup table to add in names
new<-zStats
new[] <- nameLUP$V2[match(unlist(zStats), nameLUP$V1)]
zStats$cluster<-new$cluster
# factor order 
#zStats$cluster<-factor(zStats$cluster, levels=c("Southwest","S Rockies","C Plains","N Rockies","Pac NW","Texas",
#                                                  "TN Valley","S Plains","Midwest","N Plains","Florida","Gulf Coast","Southeast","OH Valley","Northeast"))
# zStats$cluster<-factor(zStats$cluster, levels=c("N Rockies","Upper Midwest","Northeast","Pacific NW","N Plains","Ohio Valley",
#                                                 "S Rockies","C Plains","Southeast","Southwest","S Plains","Gulf Coast"))
zStats$cluster<-factor(zStats$cluster, levels=c("Southwest","S Rockies","Pacific NW","N Rockies",
                                                "S Plains","C Plains","N Plains","Upper Midwest",
                                                "Gulf Coast","Southeast","Ohio Valley","Northeast"))


# early/late factor
zStats$period <- ifelse(zStats$year > 1980,"late", "early")

regions<-as.data.frame(cbind(c("Southwest","N Rockies","Ohio Valley","S Plains","C Plains","Pacific NW",
                 "Gulf Coast","N Plains","Southeast","Upper Midwest","Northeast","S Rockies"),
               c("West","West","East","Central","Central","West",
              "East","Central","East","Central","East","West")))
colnames(regions)<-c("cluster","region")
zStats<-merge(zStats,regions, by="cluster")

# # linear trends
library("broom")
groupsLM <- zStats %>%
  group_by(cluster, threshold)
GDDTrends<-do(groupsLM,glance(lm(GDDValue ~ year, data = .)))
GDDSlopes<-do(groupsLM,tidy(lm(GDDValue ~ year, data = .)))
GDDIntercept<-GDDSlopes[which(GDDSlopes$term=="(Intercept)"),]
GDDSlopes<-GDDSlopes[which(GDDSlopes$term=="year"),]
GDDTrends<-merge(GDDTrends, GDDSlopes, by=c("cluster","threshold"))
GDDTrends<-merge(GDDTrends, GDDIntercept, by=c("cluster","threshold"))
# Sen's method https://www.rdocumentation.org/packages/trend/versions/1.1.1/topics/sens.slope
# library(trend)
# groupsSens <- zStats %>%
#   group_by(cluster, threshold)
# GDDSens<-do(groupsSens,glance(sens.slope(.$GDDValue, conf.level = 0.95)))

# PLOT OF RAW TIME SERIES --- MANUSCRIPT FIG
zStats$threshold <- factor(zStats$threshold, levels = c("GDD50", "GDD250", "GDD450"))
zStats$region <- factor(zStats$region, levels = c("West", "Central", "East"))

grobWest = grobTree(textGrob("West", x=unit(0.22,"npc"), y=unit(0.98,"npc"), hjust=0,
                            gp=gpar(col="black", fontsize=8, fontface="bold")))
grobCent = grobTree(textGrob("Central", x=unit(0.51,"npc"), y=unit(0.98,"npc"), hjust=0,
                             gp=gpar(col="black", fontsize=8, fontface="bold")))
grobEast = grobTree(textGrob("East", x=unit(0.81,"npc"), y=unit(0.98,"npc"), hjust=0,
                             gp=gpar(col="black", fontsize=8, fontface="bold")))


# add 'region' to facet_wrap
p<-ggplot(zStats, aes(x=year,y=GDDValue, color=(threshold))) +
  facet_wrap(~cluster, nrow = 1, labeller = labeller(cluster = label_wrap_gen(10)))+
  geom_line(size=0.25)+
  #geom_line(aes(y=rollmean(GDDValue, 5, na.pad=TRUE)))+
  geom_smooth(method = "lm", size=0.25)+
  #geom_ribbon(aes(ymax = sdevPos, ymin = sdevNeg,linetype=NA), alpha = 0.3)+
  #ylim(c(1,220))+
  scale_y_continuous(limits = c(1,220), expand = c(0, 0))+
  xlim(c(1948,2022))+
  background_grid(major = "xy", minor = "xy")+
  #scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  scale_colour_manual(values = c("#7fcdbb","#1d91c0","#253494"), name="Threshold")+
  labs(x ="Year", y = "Day of Year")+
  scale_x_continuous(breaks=c(1950,1970,1990,2010))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(text = element_text(size=8))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8))+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text.x  = element_text(vjust=0.5))+
  theme(strip.background=element_rect(fill="white"))+
  theme(panel.spacing = unit(0.25, "lines"))+
  theme(plot.margin=unit(c(0.5,0.5,2.0,0.5),"cm"))
  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
# add asterrisk for sig 0.05
labels.sig <- GDDTrends[ which(GDDTrends$p.value.x<=0.05 & GDDTrends$p.value.x>0.001), ]
labels.sig$year<-2020
labels.sig$GDDValue<-(labels.sig$year*labels.sig$estimate.x+labels.sig$estimate.y)-2
# add asterrisk for sig 0.001
labels.sig2<- GDDTrends[ which(GDDTrends$p.value.x<=0.001), ]
labels.sig2$year<-2020
labels.sig2$GDDValue<-labels.sig2$year*labels.sig2$estimate.x+labels.sig2$estimate.y
# add asterrisk for sig 0.001
labels.sig3<- GDDTrends[ which(GDDTrends$p.value.x<=0.001), ]
labels.sig3$year<-2020
labels.sig3$GDDValue<-(labels.sig2$year*labels.sig2$estimate.x+labels.sig2$estimate.y)-5

#grid::grid.text(unit(0.33,"npc"),unit(0.90,"npc"),label = 'West', gp=gpar(fontsize=8))

pTS<-p + geom_text(data = labels.sig, label = "*", size=5, color="black")+
  geom_text(data = labels.sig2, label = "*", size=5, color="black")+
  geom_text(data = labels.sig3, label = "*", size=5, color="black")

# plot accompanying cluster map
darkcols<-c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
            "#fb9a99","#e31a1c","#fdbf6f","#ffff99",
            "#cab2d6","#6a3d9a","#ff7f00","#b15928")

classMap<-as.factor(unC$map)
# clip to USA for inset map
classMap<- mask(x = classMap, mask = USA)
# ---
rat <- levels(classMap)[[1]]
# cluster names
rat[["cluster"]]<-c("Southwest","N Rockies","Ohio Valley","S Plains","C Plains","Pacific NW",
                    "Gulf Coast","N Plains","Southeast","Upper Midwest","Northeast","S Rockies")
levels(classMap) <- rat 
# plot classified map
# pMap<-levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
#           margin=FALSE, main="50/250/450 Anom Zscore clustering")+
#   layer(sp.polygons(states))
pMap<-levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
                margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE), colorkey=list(space="right",
                width=1, height=0.9,labels=list(cex=0.55, font=1, col="black")))+
  layer(sp.polygons(states,col = 'gray40', lwd=0.1))+
  layer(sp.polygons(zonePoly,col = 'black', lwd=0.25))

#pMap<-as.grob(pMap)
#plot_grid(pTS, pMap, labels = "AUTO", ncol = 1, rel_widths = c(1,1), rel_heights = c(2,1))

# as inset in plot area
# pMap<-as.grob(pMap)
# p<-ggdraw(pTS) +
#   draw_plot(pMap, -0.17, -0.15, scale=0.37)

# as inset outside plot
pMap<-as.grob(pMap)
p<-ggdraw(pTS) +
  draw_plot(pMap, 0.24, -0.33, scale=0.50)+
  draw_plot_label(
    c("a", "b"),
    c(0.04, 0.47),
    c(0.97, 0.31),
    size = 12
  )
  #draw_plot(pMap, 0.3, -0.41, scale=0.35)
p<-p+annotation_custom(grobWest)+annotation_custom(grobCent)+annotation_custom(grobEast)
# plot to png
#png("/home/crimmins/RProjects/TopoWx/clusterTrends/figs/fig2TS.png", width = 7, height = 5, units = "in", res = 300L)
pdf("/home/crimmins/RProjects/TopoWx/clusterTrends/figs/fig2TS.pdf", width = 7, height = 5, pointsize = 8)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()


# FIGURE 2 ------ plot slope differences choropleth map
# clipped region polygon
# create polygon shapefile
# library(rgdal)
#  cluster12polyClip<-rasterToPolygons(classMap, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
#  proj4string(cluster12polyClip) <- "+proj=longlat +datum=WGS84"
#  writeOGR(cluster12polyClip, ".", "./clusterTrends/cluster12final_clipped", driver="ESRI Shapefile")

# GDD slope diffs
GDDSlopesDiff<-GDDSlopes[,c(1,2,4)]
  GDDSlopesDiff<-spread(data=GDDSlopesDiff,threshold,estimate)
  GDDSlopesDiff$gdd450_50<-(GDDSlopesDiff$GDD450-GDDSlopesDiff$GDD50)*10
  GDDSlopesDiff$gdd250_50<-(GDDSlopesDiff$GDD250-GDDSlopesDiff$GDD50)*10
  GDDSlopesDiff$gdd450_250<-(GDDSlopesDiff$GDD450-GDDSlopesDiff$GDD250)*10
GDDSlopesDiff$cluster<-as.character(GDDSlopesDiff$cluster)
  
library(colorRamps)
library(gridExtra)
cluster12poly <- readShapePoly("./clusterTrends/cluster12final_clipped")

#cluster12poly@data$names<-clusterNames
clusterNames2<-c("Southwest","Upper Midwest","Northeast","S Rockies","N Rockies",
  "Ohio Valley","S Plains","C Plains","Pacific NW","Gulf Coast",
  "N Plains","Southeast")
cluster12poly@data$names<-clusterNames2

cluster12poly@data<-merge(cluster12poly@data, GDDSlopesDiff, by.x="names", by.y="cluster")
  cluster12poly@data<-cluster12poly@data[match(clusterNames2, cluster12poly@data$names),]
# p1<-spplot(cluster12poly,"gdd450_50",col.regions=colorRampPalette(c("#af8dc3","white", "#7fbf7b"))(100),colorkey=FALSE,at=seq(-1.1, 1.1, 0.1))
# p2<-spplot(cluster12poly,"gdd250_50",col.regions=colorRampPalette(c("#af8dc3","white", "#7fbf7b"))(100),colorkey=FALSE,at=seq(-1.1, 1.1, 0.1))
# p3<-spplot(cluster12poly,"gdd450_250",col.regions=colorRampPalette(c("#af8dc3","white", "#7fbf7b"))(100),colorkey=list(space="bottom"),at=seq(-1.1, 1.1, 0.1))
# 
# p4<-plot_grid(as.grob(p2),as.grob(p1),as.grob(p3),
#               labels = c('a', 'b','c'), label_size = 12, ncol=1,rel_widths = c(1,1,3))


p3<-spplot(cluster12poly,c(8,6,7),col.regions=colorRampPalette(c("#af8dc3","white", "#7fbf7b"))(100),
           colorkey=list(space="bottom"),at=seq(-1.1, 1.1, 0.1),
           names.attr=c("c                                                                               ",
                        "b                                                                               ",
                        "a                                                                               "),
           par.settings = list(strip.background=list(col="white")),
           sub=list(label="Difference in trends (days/decade)",cex=0.8,font = 1))


#p4<-grid.arrange(p1,p2,p3,ncol=1)
#ggsave("/home/crimmins/RProjects/TopoWx/clusterTrends/figs/slopeDiffs.png",p4)
# plot to png
#png("/home/crimmins/RProjects/TopoWx/clusterTrends/figs/slopeDiffs.png", width = 5, height = 7, units = "in", res = 300L)
pdf("/home/crimmins/RProjects/TopoWx/clusterTrends/figs/slopeDiffs.pdf", width = 5, height = 7, pointsize = 8)
#grid.newpage()
print(p3, newpage = FALSE)
dev.off()

#plot(cluster12poly)
#text(coordinates(cluster12poly)[,1], coordinates(cluster12poly)[,2], cluster12poly$names, col="red")
  
  