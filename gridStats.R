# Grid exploratory stats
# 03/29/18 MAC

library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)

states <- getData('GADM', country='United States', level=1)
#clusterpoly <- readShapePoly("./mapFiles/cluster12finalFIXED")
clusterpoly <- readShapePoly("./mapFiles/cluster15finalFIXED_ZProd")
load("./fixed/cluster15classMap.RData")

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')

# new grids
gdd50_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd250_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh250_1981-2010.grd")
gdd450_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh450_1981-2010.grd")

# load mask
maskNA<-raster("./fixed/maskNAalt.grd") 
gdd50_x4 <- mask(gdd50_x4, maskNA)
gdd250_x4 <- mask(gdd250_x4, maskNA)
gdd450_x4 <- mask(gdd450_x4, maskNA)

# add names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")

# check skewness for trend analyses
library(moments)
gdd450_x4Skew<-overlay(gdd450_x4, fun=skewness)
my.at <- seq(-2, 2, 0.1)
levelplot( gdd450_x4Skew, par.settings = RdBuTheme, margin=FALSE, at=my.at, main="GDD450_x4 Skewness (48-16)")+ 
  layer(sp.polygons(states))



# sdev grids
beginCluster(7)
sdGDD50 <- clusterR(gdd50_x4, overlay, args=list(fun=sd))
sdGDD450 <- clusterR(gdd450_x4, overlay, args=list(fun=sd))
endCluster()
my.at <- seq(0, 2, 0.05)
levelplot(sdGDD450/sdGDD50,par.settings = RdBuTheme,at=my.at,margin=FALSE, main=" SDEV gdd50-gdd450")+
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))


# detrended corrs ---- try Kendall...
# regression of values in one brick (or stack) with 'time' - get residuals
gdd50_x4[is.na(gdd50_x4)] <- 0
gdd450_x4[is.na(gdd450_x4)] <- 0
time <- 1:nlayers(gdd50_x4)
residFun <- function(x) { lm(x ~ time)$residuals }
slopeFun <- function(x) { m=lm(x ~ time); summary(m)$coefficients[2] }
pvalFun <- function(x) { m=lm(x ~ time); summary(m)$coefficients[8] }

  # parallell calc
  ptm <- proc.time()
  beginCluster(7)
    gdd50_x4Resid <- clusterR(gdd50_x4, calc, args=list(fun=residFun), export=c('time'))
    gdd450_x4Resid <- clusterR(gdd450_x4, calc, args=list(fun=residFun), export=c('time'))
    # slope
    gdd50_x4Slope <- clusterR(gdd50_x4, calc, args=list(fun=slopeFun), export=c('time'))
    gdd450_x4Slope <- clusterR(gdd450_x4, calc, args=list(fun=slopeFun), export=c('time'))
    # pval
    gdd50_x4pval <- clusterR(gdd50_x4, calc, args=list(fun=pvalFun), export=c('time'))
    gdd450_x4pval <- clusterR(gdd450_x4, calc, args=list(fun=pvalFun), export=c('time'))
  endCluster()
  proc.time() - ptm

  # detrended correlations
  corRasterDet<-corLocal(gdd50_x4Resid,gdd450_x4Resid, test=TRUE, method="pearson")
  corRaster<-corLocal(gdd50_x4,gdd450_x4, test=TRUE, method="pearson")
  #save(corRaster, file="./fixed/zscore50_450AnomDetrendedPearson.RData")
  #load("./fixed/zscore50_450AnomDetrendedPearson.RData")
  my.at <- seq(-1, 1, 0.1)
  levelplot(corRaster[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 Detrended Pearson-Corr")+ 
    #layer(sp.polygons(states))+
    layer(sp.polygons(clusterpoly))
  my.at <- seq(0, 0.1, 0.01)
  levelplot(corRaster[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 Pearson-Corr")+ 
    layer(sp.polygons(states)) +
    layer(sp.polygons(clusterpoly)) 
  
  # obs-detrened corrs
  my.at <- seq(-0.15, 0.15, 0.01)
  levelplot(corRaster[[1]]-corRasterDet[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="Obs-Detrended GDD50x4/GDD450x4 Pearson-Corr")+ 
    layer(sp.polygons(states))
  hist(corRaster[[1]]-corRasterDet[[1]])

  # trends  
  my.at <- seq(-0.5, 0.5, 0.01)
  levelplot(gdd50_x4Slope, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 Linear Trend, TopoWx 1948-2016")+
  layer(sp.polygons(states))
  my.at <- seq(-0.5, 0.5, 0.01)
  levelplot(gdd450_x4Slope, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450x4 Linear Trend, TopoWx 1948-2016")+
    layer(sp.polygons(states))
  my.at <- seq(0.0, 0.05, 0.001)
  levelplot(gdd50_x4pval, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 Linear Trend-pval, TopoWx 1948-2016")+
    layer(sp.polygons(states))
  my.at <-  seq(0.0, 0.05, 0.001)
  levelplot(gdd450_x4pval, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450x4 Linear Trend-pval, TopoWx 1948-2016")+
    layer(sp.polygons(states))
  

# compare raw/detrended corrs
  library(psych)
 pairedCorrs <- overlay(corRaster[[1]],corRasterDet[[1]], fun=function(x,y){ m=paired.r(x,y,NULL, 69); m[[3]]} )
  #  tempX<-corrGDDall[which(corrGDDall$corrLabels=="Cor.GDD500y" & corrGDDall$cluster=="Mid Atlantic" & corrGDDall$variable=="Cor.GDD50"),]
#  tempY<-corrGDDlate[which(corrGDDlate$corrLabels=="Cor.GDD500y" & corrGDDlate$cluster=="Mid Atlantic" & corrGDDlate$variable=="Cor.GDD50"),]
#  paired.r(tempX[1,4],tempY[1,4],NULL, 69, 36) # independent correlations, different sample sizes
  
    
# ----
  
  
  
  
# try horizon plot
load("./fixed/Prodzscore50_450gddStats.RData")
library(zoo)
timeIndex<-seq(as.Date('1948-01-01'), by='year', length=69)
gddZ<-setZ(prod450_50, timeIndex)
names(gddZ)<-paste0(seq(1948, 2016, by=1),".GDD")
#histogram(gddZ)
hovmoller(crop(gddZ,extent(-100,-66.67083, 24.09583, 51.19583)),par.settings=BuRdTheme(), 
          at=seq(-5, 5, 0.5), main="50*450 Prod - East")

#hovmoller(gddZ,par.settings=RdBuTheme(), at=seq(-40, 40, 2), main="GDD450 Anomalies")
#horizonplot(gddZ, digits=0,at=seq(-40, 40, 2), dirXY=y)

# correlations
# correlation of raster
corRaster<-corLocal(gdd50_x4,gdd450_x4, test=TRUE, method="pearson")
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 Pearson-Corr")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 Pearson-Corr")+ 
  layer(sp.polygons(states))

# difference in days 
# ---- Plot Mean DOYs
meanDOY50<-stack("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
meanDOY250<-stack("./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd")
meanDOY450<-stack("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
meanDiff450.50<-meanDOY450-meanDOY50

# mean and diff plot using rastervis
my.at <- seq(20, 100, 5)
levelplot(meanDiff450.50, at=my.at, par.settings = viridisTheme,
          margin=FALSE, main="Avg num of days from GDD50 to GDD450 threshold (BaseT10,TopoWx 81-00)") + 
  layer(sp.polygons(states))

# corr correlations with length of season
load("./fixed/zscore50_450AnomDetrendedPearson.RData")
# change extent
e <- extent(-100, -66.67083 , 24.09583, 51.19583)
grid1<- crop(meanDiff450.50, e)
grid2<- crop(corRaster[[1]],e)

ZProdCorr<-layerStats(brick(grid1,grid2), 'pearson', na.rm = TRUE)
plot(grid1,grid2, xlim=c(30,90), maxpixels=500000)

# # # or anoms
gdd50_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd250_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh250_1981-2010.grd")
gdd450_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh450_1981-2010.grd")

# standardize grids with z-scores
zscore<-function(x){scale(x, center = TRUE, scale = TRUE)}
ptm <- proc.time()
beginCluster(6)
gdd50_x4 <- clusterR(gdd50_x4, calc, args=list(fun=zscore))
gdd250_x4 <- clusterR(gdd250_x4, calc, args=list(fun=zscore))
gdd450_x4 <- clusterR(gdd450_x4, calc, args=list(fun=zscore))
endCluster()
proc.time() - ptm  

# mask out   
maskNA<-raster("./fixed/maskNAalt.grd") 
gdd50_x4 <-  mask(gdd50_x4, maskNA)  
gdd250_x4 <- mask(gdd250_x4, maskNA)
gdd450_x4 <- mask(gdd450_x4, maskNA)

# set names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")

# product of z-scores
prod450_50<-gdd450_x4*gdd50_x4
beginCluster(7)
  #sdProd <- clusterR(prod450_50, overlay, args=list(fun=sd))
  meanProd<-clusterR(prod450_50, overlay, args=list(fun=mean))
endCluster()
#save(prod450_50,sdProd,meanProd, file="./fixed/Prodzscore50_450gddStats.RData")
#load("./fixed/Prodzscore50_450gddStats.RData")
my.at <- seq(0, 1, 0.1)
levelplot(meanProd,par.settings = BuRdTheme,at=my.at,margin=FALSE, main="mean z-score Prod in gdd50/gdd450")+
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))
# yearly z product maps
my.at <- seq(-5, 5, 0.1)
levelplot(prod450_50[[51:69]],par.settings = BuRdTheme,at=my.at,margin=FALSE, main="z-score Prod in gdd50/gdd450")+
  layer(sp.polygons(states))
# layerstats, correlation between z-diffs and corr map
load("./fixed/zscore50_450AnomDetrendedPearson.RData")
ZProdCorr<-layerStats(brick(meanProd,corRaster[[1]]), 'pearson', na.rm = TRUE)

# diff in z-score maps
diff450_50<-gdd450_x4-gdd50_x4
beginCluster(7)
  sdDiff <- clusterR(diff450_50, overlay, args=list(fun=sd))
endCluster()
#save(diff450_50,sdDiff, file="./fixed/zscore50_450gddStats.RData")
#load("./fixed/zscore50_450gddStats.RData")
my.at <- seq(0.5, 1.5, 0.1)
levelplot(sdDiff,par.settings = BuRdTheme,at=my.at,margin=FALSE, main="sdev z-score Diff in gdd50/gdd450")+
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))
# layerstats, correlation between z-diffs and corr map
load("./fixed/zscore50_450AnomDetrendedPearson.RData")
load("./fixed/zscore50_450gddStats.RData")
ZDiffCorr<-layerStats(brick(sdDiff,corRaster[[1]]), 'pearson', na.rm = TRUE)
# diff maps
my.at <- seq(-3, 3, 0.1)
levelplot(diff450_50[[46:69]],par.settings = BuRdTheme,at=my.at,margin=FALSE, main="z-score Diff in gdd50/gdd450")
# comparison maps
my.at <- seq(-3, 3, 0.1)
levelplot(stack(diff450_50[[15]],gdd450_x4[[15]],gdd50_x4[[15]]),layout=c(1,3),
          par.settings = BuRdTheme,at=my.at,margin=FALSE, main="z-score Diff in gdd450-gdd450, Zgdd50, & Zgdd450")

corRaster<-corLocal(gdd50_x4,diff450_50, test=TRUE, method="pearson")
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/diff450 Pearson-Corr")+ 
  layer(sp.polygons(states))

# histogram by classmap
load("./fixed/zscore50_450AnomDetrendedPearson.RData")
rat <- levels(classMap)[[1]]
# cluster names
rat[["cluster"]]<-c("TN Valley","N Plains","N Rockies","Pac NW","OH Valley","Florida",
                    "Texas","S Plains","Gulf Coast","Northeast","Midwest","C Plains",
                    "Southwest","S Rockies","Southeast")
levels(classMap) <- rat 
s <- stack(corRaster[[1]], classMap)
names(s) <- c('Corr', 'Cluster')

histogram(~Corr|Cluster, data=s,
          scales=list(relation='free'),
          strip=strip.custom(strip.levels=TRUE), main="Detrended 50/450 Pearson Corr by Region",
          xlab="Pearson r", ylab="% area", col="red", nint=50)

# check standardized anom plots
#my.at <- seq(-4, 4, 0.1)
#levelplot(gdd50_x4[[60:69]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50 Anomalies - zscore")+ 
#  layer(sp.polygons(states))

corRaster<-corLocal(gdd50_x4,gdd500_x4, test=TRUE, method="pearson")
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Pearson-Corr")+ 
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Pearson-Corr p=val")+ 
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))

corRaster<-corLocal(gdd50_x4,gdd500_x4, test=TRUE, method="kendall")
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Kendall-Corr")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Kendall-Corr p=val")+ 
  layer(sp.polygons(states))

# combine into stack
allGdd<-stack(gdd50_x4,gdd250_x4,gdd450_x4)


pcaGDD<-rasterPCA(diff450_50, nSamples = 250000, nComp = 8) # or allGdd
my.at <- seq(-10, 10, 1)
levelplot(pcaGDD$map,par.settings = RdBuTheme,at=my.at, main="GDD50/250/450 Z-score - PCA")+
       layer(sp.polygons(states))
# get % var explained
eigs <- pcaGDD$model$sdev^2
plot(eigs / sum(eigs))
plot(pcaGDD$model)

# pca by GDD
pcaGDD50<-rasterPCA(gdd50_x4, nSamples = 250000, nComp = 6)
pcaGDD200<-rasterPCA(gdd200_x4, nSamples = 250000, nComp = 6)
pcaGDD500<-rasterPCA(gdd500_x4, nSamples = 250000, nComp = 6)
allPCA<-stack(pcaGDD50$map,pcaGDD200$map,pcaGDD500$map)
# get % var explained
eigs <- pcaGDD50$model$sdev^2
plot(eigs / sum(eigs))
plot(pcaGDD50$model)


# pca of zProd
load("./fixed/Prodzscore50_450gddStats.RData")
pcaProd<-rasterPCA(prod450_50, nSamples = 250000, nComp = 8)
eigs <- pcaProd$model$sdev^2
plot(eigs / sum(eigs))
plot(pcaProd$model)
plot(pcaProd$map)
my.at <- seq(-10, 10, 0.5)
levelplot(pcaProd$map,settings = RdBuTheme,at=my.at, main="GDD50*450 Z-score - PCA")+
  #layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))
plot(seq(1948,2016,1),pcaProd$model$loadings[,2], type="l")

# raw data grid stats
# new grids
gdd50_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh50.grd")
gdd250_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh250.grd")
gdd450_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh450.grd")

# load mask
maskNA<-raster("./fixed/maskNAalt.grd") 
gdd50_x4 <- mask(gdd50_x4, maskNA)
gdd250_x4 <- mask(gdd250_x4, maskNA)
gdd450_x4 <- mask(gdd450_x4, maskNA)

# add names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")

# diff in days
diff450_50<-gdd450_x4-gdd50_x4
beginCluster(7)
  meanDiff <- clusterR(diff450_50, overlay, args=list(fun=mean))
  sdDiff <- clusterR(diff450_50, overlay, args=list(fun=sd))
endCluster()
my.at <- seq(40, 100, 2)
levelplot(meanDiff,par.settings = BuRdTheme,at=my.at,margin=FALSE, main="mean Diff in gdd50/gdd450")+
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))
my.at <- seq(1, 20, 1)
levelplot(sdDiff,par.settings = BuRdTheme,at=my.at,margin=FALSE, main="sdev Diff in gdd50/gdd450")+
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))
my.at <- seq(0, 0.25, 0.01)
levelplot(sdDiff/meanDiff,par.settings = BuRdTheme,at=my.at,margin=FALSE, main="Coeff of var (%, sdev/mean) in Diff in gdd50/gdd450")+
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly))

#corrDiffs<-layerStats(stack())

# coeff of var
# cvgdd50<-cv(gdd50_x4, na.rm = TRUE)
# cvgdd450<-cv(gdd450_x4, na.rm = TRUE)
# my.at <- seq(-75, 5, 5)
# levelplot(cvgdd450-cvgdd50,par.settings = BuRdTheme,at=my.at,margin=FALSE, main="GDD50 Coeff of Var %")+
#   layer(sp.polygons(states))+
#   layer(sp.polygons(clusterpoly))

