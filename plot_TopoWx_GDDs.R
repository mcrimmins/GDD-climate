# plot TopoWx GDD grids
# 10/25/17

library(raster)
library(rasterVis)
library(sp)
library(maptools)

# set rasteroptions
rasterOptions(progress = 'text')

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)

# load NEON regions
proj <- CRS('+proj=longlat +ellps=WGS84')
##Modify next line to your folder
neonSHP <- readShapeLines('/home/crimmins/RProjects/TopoWx/mapFiles/NEON_Domains.shp', proj4string=proj)

gdd50<-stack("./fixed/anomDOY_baseT10_thresh50_1981-2010.grd")
#gdd100<-stack("./cyverse/anomDOY_baseT10_thresh100_1981-2010.grd")
gdd200<-stack("anomDOY_baseT10_thresh200_1981-2010.grd")
gdd500<-stack("anomDOY_baseT10_thresh500_1981-2010.grd")
Diff500_50<-stack("Diff500_50_baseT10_thresh200_1981-2010.grd")
gdd50_x4<-stack("X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd200_x4<-stack("X4_anomDOY_baseT10_thresh200_1981-2010.grd")
gdd500_x4<-stack("X4_anomDOY_baseT10_thresh500_1981-2010.grd")

# add layer names if needed
names(gdd50)<-as.character(seq(1948, 2016, by=1))
names(gdd100)<-as.character(seq(1948, 2016, by=1))
names(gdd200)<-as.character(seq(1948, 2016, by=1))
names(gdd500)<-as.character(seq(1948, 2016, by=1))

# # plot anomalies, 69=2016
# plot(gdd100, 69, col=colorRampPalette(c("red", "white", "blue"))(length(seq(-100, 100, by = 10))-1), breaks= seq(-100, 100, by = 10), main="")
#   title(main="TopoWx GDD100-BaseT10: 2016")
# 
# plot(gdd100, 59:69, nc=10, nr=4, col=colorRampPalette(c("red", "white", "blue"))(length(seq(-100, 100, by = 10))-1), breaks= seq(-100, 100, by = 10))

# rasterVis plots
my.at <- seq(-75, 75, 5)
my.at <- seq(-50, 50, 5)
#levelplot(gdd50, par.settings = RdBuTheme, at=my.at, main="GDD50-BaseT10")
levelplot(gdd50[[60:69]], par.settings = RdBuTheme, at=my.at, main="GDD50 Anoms (BaseT10-TopoWx,81-00 normal)")+
  layer(sp.polygons(states))
#levelplot(gdd200, par.settings = RdBuTheme, at=my.at, main="GDD200-BaseT10")
#levelplot(gdd500, par.settings = RdBuTheme, at=my.at, main="GDD500-BaseT10")

#levelplot(gdd50, layers=c(32,37), par.settings = RdBuTheme, at=my.at, main="GDD50-BaseT10, Yr:1981")
levelplot(gdd100, layers=c(58), par.settings = RdBuTheme, at=my.at, main="GDD100-BaseT10-TopoWx, Yr:2005")
#levelplot(gdd200, layers=c(32,37), par.settings = RdBuTheme, at=my.at, main="GDD200-BaseT10, Yr:1981")
#levelplot(gdd500, layers=c(32,37), par.settings = RdBuTheme, at=my.at, main="GDD500-BaseT10, Yr:1981")


levelplot(gdd500[[32]]-gdd50[[32]], par.settings = RdBuTheme, at=my.at, main="GDD500-GDD50 Anomaly Diff, Yr:2012")

levelplot(gdd500[[37]]-gdd50[[37]], par.settings = RdBuTheme, at=my.at, main="GDD500-GDD50 Anomaly Diff, Yr:2017")


# ---- Plot Mean DOYs
meanDOY50<-stack("meanDOY_baseT10_thresh50_1981-2010.grd")
meanDOY200<-stack("meanDOY_baseT10_thresh200_1981-2010.grd")
meanDOY500<-stack("meanDOY_baseT10_thresh500_1981-2010.grd")

meanDiff500.50<-meanDOY500-meanDOY50

# plot anomalies, 69=2016
plot(meanDiff500.50, 
     zlim=c(0,100),
     breaks= seq(0, 100, by = 10), 
     col=colorRampPalette(c("red", "yellow", "blue"))(length(seq(0, 100, by = 10))-1),
     main="Avg num of days from GDD50 to GDD500 threshold (BaseT10,TopoWx 81-00)",
     legend.args=list(text='days', side=4, font=2, line=2.3))
plot(states, add=TRUE)

par(mfrow=c(2, 1))
plot(meanDOY200, 
     zlim=c(0,240),
     breaks= seq(0, 240, by = 20), 
     col=colorRampPalette(c("red", "yellow", "blue"))(length(seq(0, 240, by = 20))-1),
     main="Mean GDD200 DOY (BaseT10,TopoWx 81-00)",
     legend.args=list(text='day of year', side=4, font=2, line=2.3))

plot(meanDOY500, 
     zlim=c(0,240),
     breaks= seq(0, 240, by = 20), 
     col=colorRampPalette(c("red", "yellow", "blue"))(length(seq(0, 240, by = 20))-1),
     main="Mean GDD500 DOY (BaseT10,TopoWx 81-00)",
     legend.args=list(text='day of year', side=4, font=2, line=2.3))
#plot(states, add=TRUE)

# mean and diff plot using rastervis
my.at <- seq(0, 70, 5)
levelplot(meanDiff500.50, at=my.at, par.settings = viridisTheme,
  margin=FALSE, main="Avg num of days from GDD50 to GDD500 threshold (BaseT10,TopoWx 81-00)") + 
  layer(sp.polygons(states))

my.at <- seq(0, 240, 20)
levelplot(meanDOY50, at=my.at, par.settings = magmaTheme,
          margin=FALSE, main="Mean GDD50 DOY (BaseT10,TopoWx 81-00)") + 
  layer(sp.polygons(states))

my.at <- seq(0, 240, 20)
levelplot(meanDOY500, at=my.at, par.settings = magmaTheme,
          margin=FALSE, main="Mean GDD500 DOY (BaseT10,TopoWx 81-00)") + 
  layer(sp.polygons(states))

# combined plot
my.at <- seq(0, 8, 2)
my2.at <- seq(0, 240, 20)
p<-levelplot(meanDiff500.50, at=my.at, par.settings = viridisTheme,
          margin=FALSE, main="Avg num of days from GDD50 to GDD500 threshold (BaseT10,TopoWx 81-00)") + 
  layer(sp.polygons(states))
p + contourplot(meanDOY50, at=my2.at,
              margin=FALSE, main="Mean GDD50 DOY (BaseT10,TopoWx 81-00)", label.style = 'align',
              labels = list(cex = 0.8))

# reverse
my.at <- seq(0, 100, 10)
my2.at <- seq(0, 180, 20)
p<-levelplot(meanDOY50, at=my2.at, par.settings = viridisTheme,
             margin=FALSE, main="Mean GDD50 DOY (BaseT10,TopoWx 81-00)") + 
  layer(sp.polygons(states))
p + contourplot(meanDiff500.50, at=my.at,
                margin=FALSE, main="Mean GDD50 DOY (BaseT10,TopoWx 81-00)", label.style = 'align',
                labels = list(cex = 0.8))

# test rastervis plot with no margin
levelplot(meanDOY50, margin=FALSE) + 
  layer(sp.polygons(states))

# rasterVis plots
my.at <- seq(-75, 75, 5)
#levelplot(gdd50, par.settings = RdBuTheme, at=my.at, main="GDD50-BaseT10")
levelplot(gdd100[[1:33]], par.settings = RdBuTheme, at=my.at, main="GDD100-BaseT10-TopoWx")


# standard dev - parallel?
gddAnomSD <- overlay(gdd200, fun=sd)

beginCluster(3)
gddAnomSD <- clusterR(Diff500_50, overlay, args=list(fun=sd))
endCluster()


my.at <- seq(0, 30, 5)
levelplot(gddAnomSD, at=my.at, margin=FALSE, main="Std Dev of GDD500-50 Anom 1948-2016 (TopoWx 81-00)") + 
  layer(sp.polygons(states))

# # Difference Plots
# Diff500_50<- overlay(gdd500,
#                       gdd50,
#                       fun=function(r1, r2){return(r1-r2)})
# # write Raster to file
# writeRaster(Diff500_50,filename="Diff500_50_baseT10_thresh200_1981-2010.grd", overwrite=TRUE)


names(Diff500_50)<-as.character(seq(1948, 2016, by=1))
my.at <- seq(-75, 75, 5)
#levelplot(gdd50, par.settings = RdBuTheme, at=my.at, main="GDD50-BaseT10")
levelplot(Diff500_50[[34:69]], par.settings = RdBuTheme, at=my.at, main="Anoms GDD500-GDD50 (BaseT10-TopoWx,81-00 normal)")

# violin plot
bwplot(Diff500_50[[50:69]],
       xlab="Year",
       ylab="Anom500-Anom50 in Days",
       ylim=c(-60,60))

# unsupervised classifications
library(RStoolbox)
unC <- unsuperClass(gdd500, nSamples = 50000, nClasses = 12, nStarts = 5)
## Plots
colors <- rainbow(12)
plot(unC$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
legend("bottom", legend = paste0("C",1:12), fill = colors,
       title = "Classes", horiz = TRUE, bty = "n",col=2,cex=0.5,pt.cex=0.5)

clusters<-as.data.frame(t(unC$model$centers))
plot(seq(1948, 2016, by=1),clusters$'5', type='l', col='cadetblue')
lines(seq(1948, 2016, by=1),clusters$'3', col='green')
lines(seq(1948, 2016, by=1),clusters$'4', col='light green')
legend("bottomleft",  c('2','4','1'), lty=c(1,1,1), col=c("cadetblue","green","light green"))

  # classify with 50/200/500 combo
  gdd50raw_x4<-stack("X4_rawDOY_baseT10_thresh50.grd")
  gdd200raw_x4<-stack("X4_rawDOY_baseT10_thresh200.grd")
  gdd500raw_x4<-stack("X4_rawDOY_baseT10_thresh500.grd")
  
  allGdd<-stack(gdd50raw_x4,gdd200raw_x4,gdd500raw_x4)
  unC <- unsuperClass(allGdd, nSamples = 50000, nClasses = 5, nStarts = 5)
  ## Plots
  levelplot(unC$map, margin=FALSE)

# # aggregate to lower resolution grids
# gdd50_x4 <- aggregate(gdd50, fact=4, fun=mean)
# writeRaster(gdd50_x4,filename="X4_anomDOY_baseT10_thresh50_1981-2010.grd", overwrite=TRUE)
# gdd500_x4 <- aggregate(gdd500, fact=4, fun=mean)
# writeRaster(gdd500_x4,filename="X4_anomDOY_baseT10_thresh500_1981-2010.grd", overwrite=TRUE)
# gdd200_x4 <- aggregate(gdd200, fact=4, fun=mean)
# writeRaster(gdd200_x4,filename="X4_anomDOY_baseT10_thresh200_1981-2010.grd", overwrite=TRUE)
# raw grids
  # gdd50raw<-stack("rawDOY_baseT10_thresh50.grd")
  #   gdd50raw_x4 <- aggregate(gdd50raw, fact=4, fun=mean)
  #   writeRaster(gdd50raw_x4,filename="X4_rawDOY_baseT10_thresh50.grd", overwrite=TRUE)
  # gdd200raw<-stack("rawDOY_baseT10_thresh200.grd")
  #   gdd200raw_x4 <- aggregate(gdd200raw, fact=4, fun=mean)
  #   writeRaster(gdd200raw_x4,filename="X4_rawDOY_baseT10_thresh200.grd", overwrite=TRUE)
  # gdd500raw<-stack("rawDOY_baseT10_thresh500.grd")
  #   gdd500raw_x4 <- aggregate(gdd500raw, fact=4, fun=mean)
  #   writeRaster(gdd500raw_x4,filename="X4_rawDOY_baseT10_thresh500.grd", overwrite=TRUE)  
  # 
  
  meanDOY50<-raster("meanDOY_baseT10_thresh50_1981-2010.grd")
  meanDOY50_x4 <- aggregate(meanDOY50, fact=4, fun=mean)
   writeRaster(meanDOY50_x4,filename="X4_meanDOY_baseT10_thresh50_1981-2010.grd", overwrite=TRUE)
 
  meanDOY200<-raster("meanDOY_baseT10_thresh200_1981-2010.grd")
  meanDOY200_x4 <- aggregate(meanDOY200, fact=4, fun=mean)
   writeRaster(meanDOY200_x4,filename="X4_meanDOY_baseT10_thresh200_1981-2010.grd", overwrite=TRUE)
   
  meanDOY500<-raster("meanDOY_baseT10_thresh500_1981-2010.grd")
  meanDOY500_x4 <- aggregate(meanDOY500, fact=4, fun=mean)
   writeRaster(meanDOY500_x4,filename="X4_meanDOY_baseT10_thresh500_1981-2010.grd", overwrite=TRUE)
   
  
# correlation of raster
corRaster<-corLocal(gdd50_x4,gdd500_x4, test=TRUE, method="kendall")
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Kendall-Corr")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Kendall-Corr")+ 
  layer(sp.polygons(states))

corRaster<-corLocal(gdd50_x4,gdd500_x4, test=TRUE, method="pearson")
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Pearson-Corr")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Pearson-Corr p=val")+ 
  layer(sp.polygons(states))


# split period correlations
corRaster48.81<-corLocal(gdd50_x4[[1:34]],gdd500_x4[[1:34]], test=TRUE, method="kendall")
corRaster82.16<-corLocal(gdd50_x4[[35:69]],gdd500_x4[[35:69]], test=TRUE, method="kendall")
writeRaster(corRaster48.81,filename="gdd500_gdd50_corrRaster4881.grd", overwrite=TRUE)
writeRaster(corRaster82.16,filename="gdd500_gdd50_corrRaster8216.grd", overwrite=TRUE)

my.at <- seq(-1, 1, 0.1)
levelplot(corRaster48.81[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main=" 1948-1981 GDD50x4/GDD500x4 Kendall-Corr")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster82.16[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main=" 1982-2016 GDD50x4/GDD500x4 Kendall-Corr")+ 
  layer(sp.polygons(states))

corrDiff<-corRaster82.16[[1]]-corRaster48.81[[1]]
my.at <- seq(-0.5, 0.5, 0.1)
levelplot(corrDiff, par.settings = BuRdTheme, at=my.at, margin=FALSE, main="Change in Kendall Corr 1982-2016/1948-1981 GDD50x4/GDD500x4 Kendall-Corr")+ 
  layer(sp.polygons(states))



# Mann Kendall test of raster
library(Kendall)
raster.sl <- calc(gdd500_x4,function(x){MannKendall((x))$sl})
raster.tau <- calc(gdd500_x4,function(x){MannKendall((x))$tau})
#raster.S <- calc(gdd500,function(x){MannKendall((x))$S})

my.at <- seq(-0.5, 0.5, 0.05)
levelplot(raster.tau, par.settings = RdBuTheme, margin=FALSE, at=my.at, main="Mann-Kendall Tau - GDD500x4-B10 Anoms")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(raster.sl, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Mann-Kendall Tau p-value - GDD500x4-B10 Anoms")+ 
  layer(sp.polygons(states))


# rasterPCA 
pcs<-rasterPCA(gdd50_x4, nSamples = 10000, nComp = 5)


# leaflet map of rasters
library(leaflet)
# blues to red colors
#col=c("#ff5050", "#ffffff", "#0066cc")
col="magma"
pal <- colorNumeric(col, values(meanDOY500_x4),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(meanDOY500_x4, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(meanDOY500_x4),
            title = "Mean GDD500 DOY")


# ---- interannual time series 
# extract time series from point
library(tidyr) # masks extract...
library(cowplot)
library(raster)
library(sp)
library(maptools)

gdd50raw<-stack("rawDOY_baseT10_thresh50.grd")
gdd100raw<-stack("./cyverse/rawDOY_baseT10_thresh100.grd")
gdd200raw<-stack("rawDOY_baseT10_thresh200.grd")
gdd500raw<-stack("rawDOY_baseT10_thresh500.grd")

meanDOY50<-stack("meanDOY_baseT10_thresh50_1981-2010.grd")
meanDOY500<-stack("meanDOY_baseT10_thresh500_1981-2010.grd")
meanDiff500.50<-meanDOY500-meanDOY50

states <- getData('GADM', country='United States', level=1)

plot(meanDiff500.50, 
     zlim=c(0,100),
     breaks= seq(0, 100, by = 10), 
     col=colorRampPalette(c("red", "yellow", "blue"))(length(seq(0, 100, by = 10))-1),
     main="Avg num of days from GDD50 to GDD500 threshold (BaseT10,TopoWx 81-00)",
     legend.args=list(text='days', side=4, font=2, line=2.3))
plot(states, add=TRUE)

point<-click(meanDiff500.50, n=1, xy=TRUE, show=FALSE, type="p")

gdd50ts<-t(raster::extract(gdd50raw, cellFromXY(gdd50raw, c(point$x,point$y))))
gdd100ts<-t(raster::extract(gdd100raw, cellFromXY(gdd100raw, c(point$x,point$y))))
gdd200ts<-t(raster::extract(gdd200raw, cellFromXY(gdd200raw, c(point$x,point$y))))
gdd500ts<-t(raster::extract(gdd500raw, cellFromXY(gdd500raw, c(point$x,point$y))))

gddTS<-as.data.frame(cbind(seq(1948, 2016, by=1),gdd50ts,gdd100ts,gdd200ts,gdd500ts))
colnames(gddTS)<-c("years","gdd50","gdd100","gdd200","gdd500")

gddTS<-gddTS %>% gather(years,2:5)
colnames(gddTS)[2:3]<-c("threshold","values")

ggplot(gddTS, aes(x=years,y=values, color=factor(threshold))) +
  geom_line() +
  ylim(c(0,150))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY for GDD Thresholds at ",point$y,",",point$x),
     x ="Year", y = "Day of Year")


#plot(seq(1948, 2016, by=1),test)


# ---- RAW DIFFs
# 500-50 diff from yearly raw
raw50<-stack("rawDOY_baseT10_thresh50.grd")
raw500<-stack("rawDOY_baseT10_thresh500.grd")

# rawDiff500.50<-raw500-raw50
# writeRaster(rawDiff500.50,filename="rawDiff500-50_baseT10.grd", overwrite=TRUE)
rawDiff500.50<-stack("rawDiff500-50_baseT10.grd")
names(rawDiff500.50)<-as.character(seq(1948, 2016, by=1))

my.at <- seq(10, 120, 5)
#levelplot(gdd50, par.settings = RdBuTheme, at=my.at, main="GDD50-BaseT10")
levelplot(rawDiff500.50[[45:69]], par.settings = viridisTheme, at=my.at, margin=FALSE, main="500-50GDD DOY Difference")

# stdev from raw diffs
beginCluster(3)
diffSD <- clusterR(rawDiff500.50, overlay, args=list(fun=sd))
endCluster()
#writeRaster(diffSD,filename="stdev_rawDiff500-50_baseT10.grd", overwrite=TRUE)

my.at <- seq(0, 25, 5)
levelplot(diffSD, par.settings = viridisTheme, at=my.at, margin=FALSE, main="Stdev 500-50GDD DOY 48-16")+
  layer(sp.polygons(states))

# mean from raw diffs
beginCluster(3)
diffMean <- clusterR(rawDiff500.50, overlay, args=list(fun=mean))
endCluster()
writeRaster(diffMean,filename="mean_rawDiff500-50_baseT10.grd", overwrite=TRUE)

my.at <- seq(0, 20, 1)
levelplot(450/diffMean, par.settings = viridisTheme, at=my.at, margin=FALSE, main="Mean 500-50GDD DOY 48-16")+
  layer(sp.polygons(states))

# COV 
my.at <- seq(0, 0.5, 0.05)
levelplot(diffSD/diffMean, par.settings = viridisTheme, at=my.at, margin = list(FUN = median), main="COV 500-50GDD DOY (48-16)")+
  layer(sp.polygons(neonSHP))

# # aggregate to lower resolution grids
# rawDiff500.50_x4 <- aggregate(rawDiff500.50, fact=4, fun=mean)
#writeRaster(rawDiff500.50_x4,filename="X4_rawDiff500-50_baseT10.grd", overwrite=TRUE)
 # Mann Kendall test of raster
 library(Kendall)
 raster.sl <- calc(rawDiff500.50_x4,function(x){MannKendall((x))$sl})
 raster.tau <- calc(rawDiff500.50_x4,function(x){MannKendall((x))$tau})
 #raster.S <- calc(gdd500,function(x){MannKendall((x))$S})
 
 my.at <- seq(-0.5, 0.5, 0.05)
 levelplot(raster.tau, par.settings = RdBuTheme, margin=FALSE, at=my.at, main="M-K Tau - Yearly GDD500-50 Spring Window Length (48-16)")+ 
   layer(sp.polygons(states)) +
   layer(sp.polygons(neonSHP,lwd=0.8, col='darkgray'))
 my.at <- seq(0, 0.1, 0.01)
 levelplot(raster.sl, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="M-K Tau p-val - Yearly GDD500-50 Spring Window Length (48-16)")+ 
   layer(sp.polygons(states))+
   layer(sp.polygons(neonSHP,lwd=0.8, col='darkgray'))
 
 # extract time series from point
 test<-t(extract(rawDiff500.50_x4, cellFromXY(rawDiff500.50_x4, c(-99.044140, 41.082488))))
 plot(seq(1948, 2016, by=1),test)
 
 # extract TS by shapefile
 neonTS <- t(extract(rawDiff500.50_x4, neonSHP, na.rm=TRUE, fun=mean, df=TRUE))
 neonTS <- neonTS[-1, ]
 colnames(neonTS)<-neonSHP@data$DomainName
 neonTS<-as.data.frame(neonTS)
 neonTS$year<-seq(1948, 2016, by=1)
 library(reshape2)
 library(ggplot2)
  dfm <- melt(neonTS, id.vars=c("year"))
  ggplot(dfm, aes(x=year, y=value, color=variable)) + geom_line()+
    stat_smooth(method = "lm", se = FALSE)
# ----- 

  
# ---- reclassify trend maps
  # Mann Kendall test of raster
  # library(Kendall)
  # raster.sl <- calc(gdd500_x4,function(x){MannKendall((x))$sl})
  # raster.tau <- calc(gdd500_x4,function(x){MannKendall((x))$tau})  
  # writeRaster(raster.tau,filename="./trendGrids/gdd500_x4_MKTrend_tau_48-16.grd", overwrite=TRUE)
  # writeRaster(raster.sl,filename="./trendGrids/gdd500_x4_MKTrend_pval_48-16.grd", overwrite=TRUE)
  # 
  # load trend grids
  gdd500Trend<-raster("./trendGrids/gdd500_x4_MKTrend_tau_48-16.grd")
  gdd50Trend<-raster("./trendGrids/gdd50_x4_MKTrend_tau_48-16.grd")
  
  ## reclasiffy
  gdd50Trend<- reclassify(gdd50Trend, c(-1,-0.2,1,  -0.2,0.2,2, 0.2,1,3))
  gdd50Trend<-as.factor(gdd50Trend)
  
  gdd500Trend<- reclassify(gdd500Trend, c(-1,-0.2,10,  -0.2,0.2,20, 0.2,1,30))
  gdd500Trend<-as.factor(gdd500Trend)
  
  combinedTrend<-gdd500Trend+gdd50Trend
  combinedTrend<-as.factor(combinedTrend)
  
  rat <- levels(combinedTrend)[[1]]
  rat[["trends"]] <- c("early-early","NC-early", "early-NC","NC-NC","late-NC","early-late","NC-late","late-late")
  levels(combinedTrend) <- rat
  
  # ## Add a landcover column to the Raster Attribute Table
  # rat <- levels(gdd50Trend)[[1]]
  # rat[["landcover"]] <- c("earlier","no change", "later")
  # levels(gdd50Trend) <- rat
  # 
  # rat <- levels(gdd500Trend)[[1]]
  # rat[["landcover"]] <- c("earlier","no change", "later")
  # levels(gdd500Trend) <- rat
  # test<-crosstab(gdd50Trend,gdd500Trend)
  
  ## Plot
  levelplot(combinedTrend, col.regions=(topo.colors(8)), xlab="", ylab="", main="MK-Tau Trend Classifications 50/500GDD")+
    layer(sp.polygons(states))
    
 # traditional least squares
 # https://matinbrandt.wordpress.com/2013/11/15/pixel-wise-time-series-trend-anaylsis-with-ndvi-gimms-and-r/
  # time <- 1:nlayers(gdd50_x4) 
  # fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }}
  # gdd50_x4.slope=calc(gdd50_x4, fun)
  # fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }}
  # gdd50_x4.pval <- calc(gdd50_x4, fun=fun)
  # writeRaster(gdd50_x4.slope,filename="./trendGrids/gdd50_x4_linear_slope_48-16.grd", overwrite=TRUE)
  # writeRaster(gdd50_x4.pval,filename="./trendGrids/gdd50_x4_linear_pval_48-16.grd", overwrite=TRUE)
  # 
  
  # recent trends 33-69 1980-2016
  # time <- 1:nlayers(gdd500_x4[[33:69]]) 
  # fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }}
  # gdd500_x4.slope=calc(gdd500_x4[[33:69]], fun)
  # fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }}
  # gdd500_x4.pval <- calc(gdd500_x4[[33:69]], fun=fun)
  
#  writeRaster(gdd500_x4.slope,filename="./trendGrids/gdd500_x4_linear_slope_80-16.grd", overwrite=TRUE)
#  writeRaster(gdd500_x4.pval,filename="./trendGrids/gdd500_x4_linear_pval_80-16.grd", overwrite=TRUE)
  
  # check skewness for trend analyses
  library(moments)
  gdd500_x4Skew<-overlay(gdd500_x4, fun=skewness)
  my.at <- seq(-2, 2, 0.1)
  levelplot( gdd500_x4Skew, par.settings = RdBuTheme, margin=FALSE, at=my.at, main="GDD500_x4 Skewness (48-16)")+ 
    layer(sp.polygons(states))
  
  
  # load trend grids
  gdd500Trend<-raster("./trendGrids/gdd500_x4_linear_slope_48-16.grd")
  gdd50Trend<-raster("./trendGrids/gdd50_x4_linear_slope_48-16.grd")
  
  ## reclasiffy
  gdd50Trend<- reclassify(gdd50Trend, c(-999,-0.1,1,  -0.1,0.1,2, 0.1,999,3))
  gdd50Trend<-as.factor(gdd50Trend)
  
  gdd500Trend<- reclassify(gdd500Trend, c(-999,-0.1,10,  -0.1,0.1,20, 0.1,999,30))
  gdd500Trend<-as.factor(gdd500Trend)
  
  combinedTrend<-gdd500Trend+gdd50Trend
  combinedTrend<-as.factor(combinedTrend)
  
  rat <- levels(combinedTrend)[[1]]
  rat[["trends"]] <- c("early-early","NC-early","late-early","early-NC","NC-NC","late-NC","early-late","NC-late","late-late")
  levels(combinedTrend) <- rat 
  
  ## Plot
  levelplot(combinedTrend, col.regions=(topo.colors(9)), xlab="", ylab="", main="Linear Trend Classifications 50/500GDD")+
    layer(sp.polygons(states))
  
  # mask by pval
  gdd500pval<-raster("./trendGrids/gdd500_x4_linear_pval_48-16.grd")
  gdd50pval<-raster("./trendGrids/gdd50_x4_linear_pval_48-16.grd")
  
  m = c(0, 0.05, 1, 0.05, 1, 0)
  rclmat = matrix(m, ncol=3, byrow=TRUE)
  
  # m = c(0, 0.05, 2, 0.05, 1, 0)
  # rclmat2 = matrix(m, ncol=3, byrow=TRUE)
  
  p.mask500 = reclassify(gdd500pval, rclmat)
  fun=function(x) { x[x<1] <- NA; return(x)}
  p.mask500.NA = calc(p.mask500, fun)
  
  p.mask50 = reclassify(gdd50pval, rclmat)
  fun=function(x) { x[x<1] <- NA; return(x)}
  p.mask50.NA = calc(p.mask50, fun)
  
  p.combined=p.mask50.NA+p.mask500.NA
  trend.sig = mask(combinedTrend, p.combined)
 
  # masked with pvals 
  levelplot(trend.sig, col.regions=(topo.colors(9)), xlab="", ylab="", main="Linear Trend Classifications 50/500GDD (48-16)")+
    layer(sp.polygons(states))
    
  # trend classification with sig overlay
p<- levelplot(combinedTrend, col.regions=(topo.colors(9)), xlab="", ylab="", main="Linear Trend Classifications 50/500GDD (48-16)")+
    layer(sp.polygons(states))
    p + contourplot(trend.sig, 
                  margin=FALSE, labels=NULL)
  
  
  # standard trend plot
  my.at <- seq(0, 0.1, 0.01)
  levelplot(gdd500pval, par.settings = RdBuTheme, margin=FALSE, at=my.at, main="Linear Trend p-val in 500GDD Anoms days/yr (80-16)")+ 
    layer(sp.polygons(states))+
    layer(sp.polygons(neonSHP,lwd=0.8, col='darkgray'))
  
  
  # plot meanDiff500.50_x4 vs corrs
  meanDiff500.50_x4 <- aggregate(meanDiff500.50, fact=4, fun=mean)
  plot(corRaster[[1]],meanDiff500.50_x4, xlim=c(0, 0.8), ylim=c(0, 80))
  
  temp<-stack(corRaster[[1]],meanDiff500.50_x4)
  names(temp)<-c("corr","diff")
  hexbinplot(corr~diff|cut(x,6), data = temp, ylim=c(0, 0.8), xlim=c(0, 80))
  xyplot(corr~diff|cut(x,4), data = temp, ylim=c(0, 0.8), xlim=c(0, 80))