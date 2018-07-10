# Grid modeling
# 05/23/18 MAC

library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)

states <- getData('GADM', country='United States', level=1)
#clusterpoly <- readShapePoly("./mapFiles/cluster12finalFIXED")
#clusterpoly <- readShapePoly("./mapFiles/cluster15finalFIXED_ZProd")
#load("./fixed/cluster15classMap.RData")

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')

# mean for clusterR
meanBaseT<- function(x){
  meanT<- mean(x, na.rm=TRUE)
  return(meanT)
}
# sd for clusterR
sdBaseT<- function(x){
  sdT<- sd(x, na.rm=TRUE)
  return(sdT)
}


# load mask
maskNA<-raster("./fixed/maskNAalt.grd") 

# try zonal stats by cluster map
gdd50raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh50.grd")
  gdd50raw_x4 <- mask(gdd50raw_x4, maskNA)
#gdd250raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh250.grd")
#  gdd250raw_x4 <- mask(gdd250raw_x4, maskNA)
gdd450raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh450.grd")
  gdd450raw_x4 <- mask(gdd450raw_x4, maskNA)
# set names
names(gdd50raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
#names(gdd250raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")


# calc grids
beginCluster(7)
  meanDiffGDD450_50SD <- clusterR(gdd450raw_x4-gdd50raw_x4, overlay, args=list(fun=sdBaseT))
  meanDayGDD50 <- clusterR(gdd50raw_x4, overlay, args=list(fun=meanBaseT))
  meanDayGDD50SD <- clusterR(gdd50raw_x4, overlay, args=list(fun=sdBaseT))
endCluster()

# corr grid -- do detrended too
pearsonCorr2<-corLocal(gdd50raw_x4,gdd450raw_x4, test=TRUE, method="pearson")
# detrended corr
load("./fixed/zscore50_450AnomDetrendedPearson.RData")
pearsonCorr<-corRaster
rm(corRaster)

# plots 
 my.at <- seq(0, 25, 1)
 levelplot(meanDiffGDD450_50SD, par.settings = YlOrRdTheme, margin=FALSE, at=my.at, main="TopoWx 48-16 SDev GDD450-GDD50 Difference")+
   layer(sp.polygons(states))
my.at <- seq(0, 200, 5)
levelplot(meanDayGDD50, par.settings = viridisTheme, margin=FALSE, at=my.at, main="Topowx 48-16 Mean GDD50 Day of Year")+
  layer(sp.polygons(states))
my.at <- seq(0, 20, 0.5)
levelplot(meanDayGDD50SD, par.settings = YlOrRdTheme, margin=FALSE, at=my.at, main="Topowx 48-16 Sdev of GDD50 Day of Year")+
  layer(sp.polygons(states))

# ratio and pearson maps
my.at <- seq(0.5, 1.5, 0.05)
levelplot(meanDiffGDD450_50SD/meanDayGDD50SD, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Topowx 48-16 meanDiffGDD450_50SD/meanDayGDD50SD")+
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(pearsonCorr[[1]], par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Observed Detrended Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(pearsonCorr2[[1]], par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Observed Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))
my.at <- seq(-0.15, 0.15, 0.01)
levelplot(pearsonCorr2[[1]]-pearsonCorr[[1]], par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Obs-Det Pearson r GDD50vGDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))


#layerStats(stack(meanDiffGDD450_50SD/meanDayGDD50SD,pearsonCorr[[1]]),'pearson', na.rm = TRUE)
#temp<-stack(meanDiffGDD450_50SD/meanDayGDD50SD,pearsonCorr[[1]])
#names(temp)<-c('ratio','corr')
#hexbinplot(ratio~corr, temp, data = temp)

# modeling - plot prediction and resid 
dataStack<- stack(mask(pearsonCorr[[1]],maskNA), 
                  mask(meanDiffGDD450_50SD,maskNA),
                  mask(meanDayGDD50,maskNA))
v <- data.frame((values(dataStack)))
#names(v) <- c('Pearson', 'mean 10C to GDD50 doy', 'Sdev 10C to GDD50 doy','GDD50 to GDD450 doy',
#              'Sdev GDD50 to GDD450 doy','mean >10C doy', 'sdev >10C doy', 'mean GDD450 doy', 'sdev GDD450 doy')
names(v) <- c('pearsonCorr','meanDiffGDD450_50SD','meanDayGDD50')

#v<-v[,c(1:3)]

lmBest <- lm(pearsonCorr~meanDiffGDD450_50SD+meanDayGDD50,
             data=v) 
summary(lmBest)
temp<- stack(mask(meanDiffGDD450_50SD,maskNA),
             mask(meanDayGDD50,maskNA))
names(temp) <- c('meanDiffGDD450_50SD','meanDayGDD50')
r1 <- predict(temp, lmBest, progress='text')
my.at <- seq(-1, 1, 0.1)
levelplot(r1, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="PREDICTED Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))
my.at <- seq(-0.3, 0.3, 0.01)
levelplot(pearsonCorr[[1]]-r1, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="PREDICTED-OBSERVED Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(pearsonCorr[[1]], par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Observed Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))

# scatteplot
vars<-stack(pearsonCorr[[1]], r1)
names(vars)<-c("observed","predicted")
hexbinplot(observed~predicted, data=vars, alpha=0.5)

# variable correlations
library(corrplot)
res <- cor(v, method = "pearson", use="pairwise.complete.obs")
corrplot(res, method="number",type = "upper", order = "original",
         tl.col = "black", tl.srt = 45, title="Map Vars Pearson Corrs")