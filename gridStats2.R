# Grid exploratory stats
# 03/29/18 MAC
# UPDATED for use with BaseT10, BaseT0
# 06/03/18 MAC

library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)

# map layers
states <- getData('GADM', country='United States', level=1)

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')

# new grids
gdd50_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd250_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh250_1981-2010.grd")
gdd450_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh450_1981-2010.grd")
gdd50_x4_0<-stack("./fixed/X4_anomDOY_baseT0_thresh50_1981-2010.grd")
gdd250_x4_0<-stack("./fixed/X4_anomDOY_baseT0_thresh250_1981-2010.grd")
gdd450_x4_0<-stack("./fixed/X4_anomDOY_baseT0_thresh450_1981-2010.grd")

# load mask
maskNA<-raster("./fixed/maskNAalt.grd")
gdd50_x4 <- mask(gdd50_x4, maskNA)
gdd250_x4 <- mask(gdd250_x4, maskNA)
gdd450_x4 <- mask(gdd450_x4, maskNA)
# base T0 mask
maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd") 
gdd50_x4_0 <- mask(gdd50_x4_0, maskNA_0)
gdd250_x4_0 <- mask(gdd250_x4_0, maskNA_0)
gdd450_x4_0 <- mask(gdd450_x4_0, maskNA_0)

# add names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")
names(gdd50_x4_0)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4_0)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4_0)<-paste0(seq(1948, 2016, by=1),".GDD450")

# sdev grids 450-50
diff450_50<-gdd450_x4-gdd50_x4
diff450_50_0<-gdd450_x4_0-gdd50_x4_0
# 250-50
diff250_50<-gdd250_x4-gdd50_x4
diff250_50_0<-gdd250_x4_0-gdd50_x4_0
# 450-250
diff450_250<-gdd450_x4-gdd250_x4
diff450_250_0<-gdd450_x4_0-gdd250_x4_0

beginCluster(7)
  sdGDD50 <- clusterR(gdd50_x4, overlay, args=list(fun=sd))
  sdGDD250 <- clusterR(gdd250_x4, overlay, args=list(fun=sd))
  sdGDD450 <- clusterR(gdd450_x4, overlay, args=list(fun=sd))
  sdGDD50_0 <- clusterR(gdd50_x4_0, overlay, args=list(fun=sd))
  sdGDD250_0 <- clusterR(gdd250_x4_0, overlay, args=list(fun=sd))
  sdGDD450_0 <- clusterR(gdd450_x4_0, overlay, args=list(fun=sd))
  
  sdDiff250_50 <- clusterR(diff250_50, overlay, args=list(fun=sd))
  sdDiff250_50_0 <- clusterR(diff250_50_0, overlay, args=list(fun=sd))  
  sdDiff50_450 <- clusterR(diff450_50, overlay, args=list(fun=sd))
  sdDiff50_450_0 <- clusterR(diff450_50_0, overlay, args=list(fun=sd))
  sdDiff450_250 <- clusterR(diff450_250, overlay, args=list(fun=sd))
  sdDiff450_250_0 <- clusterR(diff450_250_0, overlay, args=list(fun=sd))
  
endCluster()
rm(diff450_50, diff450_50_0, diff450_250, diff450_250_0, diff250_50, diff250_50_0)
save(sdGDD50,sdGDD450, sdGDD50_0,sdGDD450_0,sdDiff50_450,sdDiff50_450,sdDiff50_450_0,
     sdDiff250_50, sdDiff250_50_0,sdDiff450_250,sdDiff450_250_0, sdGDD250, sdGDD250_0, 
     file="./fixed/bothBaseT/sDevsBothBaseT.RData")
#load("./fixed/bothBaseT/sDevsBothBaseT.RData")

# my.at <- seq(0, 2, 0.05)
# levelplot(sdGDD450/sdGDD50,par.settings = RdBuTheme,at=my.at,margin=FALSE, main=" SDEV gdd50-gdd450")+
#   layer(sp.polygons(states))+
#   layer(sp.polygons(clusterpoly))

# detrended corrs ---- try Kendall...
# regression of values in one brick (or stack) with 'time' - get residuals
#gdd50_x4[is.na(gdd50_x4)] <- 0
#gdd250_x4[is.na(gdd250_x4)] <- 0
#gdd450_x4[is.na(gdd450_x4)] <- 0
#gdd50_x4_0[is.na(gdd50_x4_0)] <- 0
#gdd250_x4_0[is.na(gdd250_x4_0)] <- 0
gdd450_x4_0[is.na(gdd450_x4_0)] <- 0

    time <- 1:nlayers(gdd450_x4_0)
    residFun <- function(x) { lm(x ~ time)$residuals }
    slopeFun <- function(x) { m=lm(x ~ time); summary(m)$coefficients[2] }
    pvalFun <- function(x) { m=lm(x ~ time); summary(m)$coefficients[8] }

# parallell calc Base T10
ptm <- proc.time()
beginCluster(7)
   #gdd50_x4Resid <- clusterR(gdd50_x4, calc, args=list(fun=residFun), export=c('time'))
    gdd250_x4Resid <- clusterR(gdd250_x4, calc, args=list(fun=residFun), export=c('time'))
   #gdd450_x4Resid <- clusterR(gdd450_x4, calc, args=list(fun=residFun), export=c('time'))
    # slope
   #gdd50_x4Slope <- clusterR(gdd50_x4, calc, args=list(fun=slopeFun), export=c('time'))
    gdd250_x4Slope <- clusterR(gdd250_x4, calc, args=list(fun=slopeFun), export=c('time'))
   #gdd450_x4Slope <- clusterR(gdd450_x4, calc, args=list(fun=slopeFun), export=c('time'))
    # pval
   #gdd50_x4pval <- clusterR(gdd50_x4, calc, args=list(fun=pvalFun), export=c('time'))
    gdd250_x4pval <- clusterR(gdd250_x4, calc, args=list(fun=pvalFun), export=c('time'))
   #gdd450_x4pval <- clusterR(gdd450_x4, calc, args=list(fun=pvalFun), export=c('time'))
endCluster()
proc.time() - ptm
save(gdd50_x4Resid,gdd250_x4Resid,gdd450_x4Resid,gdd50_x4Slope,gdd250_x4Slope,gdd450_x4Slope,
     gdd50_x4pval,gdd250_x4pval,gdd450_x4pval,
     file="./fixed/bothBaseT/detrendedBaseT10.RData")
#load("./fixed/bothBaseT/detrendedBaseT10.RData")

# parallell calc Base T0
ptm <- proc.time()
beginCluster(7)
    #gdd50_x4Resid_0 <- clusterR(gdd50_x4_0, calc, args=list(fun=residFun), export=c('time'))
    #gdd250_x4Resid_0 <- clusterR(gdd250_x4_0, calc, args=list(fun=residFun), export=c('time'))
    gdd450_x4Resid_0 <- clusterR(gdd450_x4_0, calc, args=list(fun=residFun), export=c('time'))
    # slope
    #gdd50_x4Slope_0 <- clusterR(gdd50_x4_0, calc, args=list(fun=slopeFun), export=c('time'))
    #gdd250_x4Slope_0 <- clusterR(gdd250_x4_0, calc, args=list(fun=slopeFun), export=c('time'))
    gdd450_x4Slope_0 <- clusterR(gdd450_x4_0, calc, args=list(fun=slopeFun), export=c('time'))
    # pval
    #gdd50_x4pval_0 <- clusterR(gdd50_x4_0, calc, args=list(fun=pvalFun), export=c('time'))
    #gdd250_x4pval_0 <- clusterR(gdd250_x4_0, calc, args=list(fun=pvalFun), export=c('time'))
    gdd450_x4pval_0 <- clusterR(gdd450_x4_0, calc, args=list(fun=pvalFun), export=c('time'))
endCluster()
proc.time() - ptm
save(gdd50_x4Resid_0,gdd250_x4Resid_0,gdd450_x4Resid_0,gdd50_x4Slope_0,gdd250_x4Slope_0,gdd450_x4Slope_0,
     gdd50_x4pval_0,gdd250_x4pval_0,gdd450_x4pval_0,
     file="./fixed/bothBaseT/detrendedBaseT0.RData")
#load("./fixed/bothBaseT/detrendedBaseT0.RData")

#gdd450_x4Resid_0<-gdd450_x4Resid_0b
#gdd450_x4Slope_0<-gdd450_x4Slope_0b
#gdd450_x4pval_0<-gdd450_x4pval_0b

# detrended correlations Base T10
load("./fixed/bothBaseT/detrendedBaseT10.RData")
# 450 v 50
corRasterDet_BaseT10<-corLocal(gdd50_x4Resid,gdd450_x4Resid, test=TRUE, method="pearson")
corRaster_BaseT10<-corLocal(gdd50_x4,gdd450_x4, test=TRUE, method="pearson")
# 250 v 50
corRasterDet_BaseT10_250v50<-corLocal(gdd250_x4Resid,gdd50_x4Resid, test=TRUE, method="pearson")
corRaster_BaseT10_250v50<-corLocal(gdd250_x4,gdd50_x4, test=TRUE, method="pearson")
# 450 v 250
corRasterDet_BaseT10_450v250<-corLocal(gdd250_x4Resid,gdd450_x4Resid, test=TRUE, method="pearson")
corRaster_BaseT10_450v250<-corLocal(gdd250_x4,gdd450_x4, test=TRUE, method="pearson")
save(corRaster_BaseT10,corRasterDet_BaseT10, corRasterDet_BaseT10_250v50,corRaster_BaseT10_250v50,
     corRasterDet_BaseT10_450v250, corRaster_BaseT10_450v250,
     file = "./fixed/bothBaseT/pearsonCorrBaseT10.RData")
#load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")

# detrended correlations Base T10
load("./fixed/bothBaseT/detrendedBaseT0.RData")
# 450 v 50
corRasterDet_BaseT0<-corLocal(gdd50_x4Resid_0,gdd450_x4Resid_0, test=TRUE, method="pearson")
corRaster_BaseT0<-corLocal(gdd50_x4_0,gdd450_x4_0, test=TRUE, method="pearson")
# 250 v 50
corRasterDet_BaseT0_250v50<-corLocal(gdd250_x4Resid_0,gdd50_x4Resid_0, test=TRUE, method="pearson")
corRaster_BaseT0_250v50<-corLocal(gdd250_x4_0,gdd50_x4_0, test=TRUE, method="pearson")
# 450 v 250
corRasterDet_BaseT0_450v250<-corLocal(gdd250_x4Resid_0,gdd450_x4Resid_0, test=TRUE, method="pearson")
corRaster_BaseT0_450v250<-corLocal(gdd250_x4_0,gdd450_x4_0, test=TRUE, method="pearson")

save(corRaster_BaseT0,corRasterDet_BaseT0, corRasterDet_BaseT0_250v50,corRaster_BaseT0_250v50,
     corRasterDet_BaseT0_450v250,corRaster_BaseT0_450v250,
     file = "./fixed/bothBaseT/pearsonCorrBaseT0.RData")
#load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")



# detrended sdevs
load("./fixed/bothBaseT/detrendedBaseT10.RData")
load("./fixed/bothBaseT/detrendedBaseT0.RData")
# sdev grids
diff450_50Res<-gdd450_x4Resid-gdd50_x4Resid
diff450_50_0Res<-gdd450_x4Resid_0-gdd50_x4Resid_0
# 250-50
diff250_50Res<-gdd250_x4Resid-gdd50_x4Resid
diff250_50_0Res<-gdd250_x4Resid_0-gdd50_x4Resid_0
# 450-250
diff450_250Res<-gdd450_x4Resid-gdd250_x4Resid
diff450_250_0Res<-gdd450_x4Resid_0-gdd250_x4Resid_0

beginCluster(7)
  #sdGDD50Res <- clusterR(gdd50_x4Resid, overlay, args=list(fun=sd))
  #sdGDD250Res <- clusterR(gdd250_x4Resid, overlay, args=list(fun=sd))
  #sdGDD450Res <- clusterR(gdd450_x4Resid, overlay, args=list(fun=sd))
  
  #sdGDD50_0Res <- clusterR(gdd50_x4Resid_0, overlay, args=list(fun=sd))
  #sdGDD250_0Res <- clusterR(gdd250_x4Resid_0, overlay, args=list(fun=sd))
  sdGDD450_0Res <- clusterR(gdd450_x4Resid_0, overlay, args=list(fun=sd))
  
  #sdDiff50_450Res <- clusterR(diff450_50Res, overlay, args=list(fun=sd))
  sdDiff50_450_0Res <- clusterR(diff450_50_0Res, overlay, args=list(fun=sd))
  #sdDiff450_250Res <- clusterR(diff450_250Res, overlay, args=list(fun=sd))
  sdDiff450_250_0Res <- clusterR(diff450_250_0Res, overlay, args=list(fun=sd))
  #sdDiff250_50Res <- clusterR(diff250_50Res, overlay, args=list(fun=sd))
  #sdDiff250_50_0Res <- clusterR(diff250_50_0Res, overlay, args=list(fun=sd))
  
endCluster()
rm(diff450_50Res, diff450_50_0Res)
save(sdGDD50Res,sdGDD250Res,sdGDD450Res, sdGDD50_0Res,sdGDD250_0Res,sdGDD450_0Res,sdDiff50_450Res,sdDiff50_450Res,
     sdDiff50_450_0Res, sdDiff450_250Res, sdDiff450_250_0Res,sdDiff250_50Res, sdDiff250_50_0Res,
     file="./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")
#load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")

# STOPPED HERE

# plots
# zscores ----
load("./fixed/bothBaseT/detrendedBaseT0.RData")
# standardize grids with z-scores
zscore<-function(x){scale(x, center = TRUE, scale = TRUE)}
ptm <- proc.time()
beginCluster(6)
  gdd50_x4Zresid <- clusterR(gdd50_x4Resid, calc, args=list(fun=zscore))
  gdd450_x4Zresid <- clusterR(gdd450_x4Resid, calc, args=list(fun=zscore))
endCluster()
proc.time() - ptm  
#gdd50Zresid_sd<-calc(gdd50_x4Zresid, fun=sd)
library(moments)
resid<-overlay(gdd50_x4Resid_0, fun=skewness)
my.at <- seq(-2, 2, 0.1)
levelplot( resid, par.settings = RdBuTheme, margin=FALSE, at=my.at, main="GDD50_x4 Detrended Base T0  Skewness (48-16)")+ 
  layer(sp.polygons(states))
# ----

# correlations
load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")
# plot all corrs in one stack
allCorr<-stack(corRasterDet_BaseT0[[1]], corRasterDet_BaseT10[[1]], 
               corRasterDet_BaseT0_250v50[[1]], corRasterDet_BaseT10_250v50[[1]],
               corRasterDet_BaseT0_450v250[[1]], corRasterDet_BaseT10_450v250[[1]])
names(allCorr)<-c("50v450 BaseT0", "50v450 BaseT10",
                  "50v250 BaseT0", "50v250 BaseT10",
                  "250v450 BaseT0", "250v450 BaseT10")
my.at <- seq(0, 1, 0.1)
corrFig<-levelplot(allCorr, layout=c(2,3), par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
histFig<-histogram(allCorr, layout=c(2,3))
print(corrFig, split=c(1,1,1,2), more=TRUE)
print(histFig, split=c(1,2,1,2))
# corr diffs
corrDiffs<-stack(
               corRasterDet_BaseT0_450v250[[1]]-corRasterDet_BaseT0_250v50[[1]],
               corRasterDet_BaseT10_450v250[[1]]-corRasterDet_BaseT10_250v50[[1]])
names(corrDiffs)<-c("BaseT0 450/250-250/50", "BaseT10 450/250-250/50")
my.at <- seq(-0.6, 0.6, 0.05)
levelplot(corrDiffs, layout=c(2,1), par.settings = BuRdTheme, at=my.at, margin=FALSE, main="Pearson-Corr Diffs (TopoWx 48-16)")+ 
  layer(sp.polygons(states))

# 

# 50 v 450 
my.at <- seq(0, 1, 0.1)
levelplot(corRaster_BaseT0[[1]], par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT0[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT10[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT10[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
# 50 v 250 
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT0_250v50[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD250x4 BaseT0 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT0_250v50[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD250x4 BaseT0 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT10_250v50[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD250x4 BaseT10 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT10_250v50[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD250x4 BaseT10 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
# 450 v 250 
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT0_450v250[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD450x4/GDD250x4 BaseT0 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT0_450v250[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD450x4/GDD250x4 BaseT0 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT10_450v250[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD450x4/GDD250x4 BaseT10 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT10_450v250[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD450x4/GDD250x4 BaseT10 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
# pvals
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster_BaseT0[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Pearson-Corr Pval (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRasterDet_BaseT0[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Detrended Pearson-Corr Pval (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster_BaseT10[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Pearson-Corr Pval (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRasterDet_BaseT10[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Detrended Pearson-Corr Pval (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
# correlation diffs
my.at <- seq(-0.2, 0.2, 0.01)
levelplot(corRaster_BaseT10[[1]]-corRasterDet_BaseT10[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Obs-Det Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-0.2, 0.2, 0.01)
levelplot(corRaster_BaseT0[[1]]-corRasterDet_BaseT0[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Obs-Det Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))

# compare raw/detrended corrs
library(psych)
pairedCorrs_baseT10 <- overlay(corRaster_BaseT10[[1]],corRasterDet_BaseT10[[1]], fun=function(x,y){ m=paired.r(x,y,NULL, 69); m[[3]]} )
pairedCorrs_baseT0 <- overlay(corRaster_BaseT0[[1]],corRasterDet_BaseT0[[1]], fun=function(x,y){ m=paired.r(x,y,NULL, 69); m[[3]]} )
my.at <- seq(0, 0.2, 0.01)
levelplot(pairedCorrs_baseT10, par.settings = BuRdTheme, at=my.at, margin=FALSE, main="Fisher Z pval BaseT10 Obs-Det Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.2, 0.01)
levelplot(pairedCorrs_baseT0, par.settings = BuRdTheme, at=my.at, margin=FALSE, main="Fisher Z pval BaseT0 Obs-Det Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))


# trends 
#load("./fixed/bothBaseT/detrendedBaseT10.RData")
my.at <- seq(-0.5, 0.5, 0.01)
levelplot(gdd50_x4Slope, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 BaseT10 Linear Trend, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <- seq(-0.5, 0.5, 0.01)
levelplot(gdd450_x4Slope, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450x4 BaseT10 Linear Trend, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <- seq(0.0, 0.05, 0.001)
levelplot(gdd50_x4pval, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 BaseT10 Linear Trend-pval, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 0.05, 0.001)
levelplot(gdd450_x4pval, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450x4 BaseT10 Linear Trend-pval, TopoWx 1948-2016")+
  layer(sp.polygons(states))
#load("./fixed/bothBaseT/detrendedBaseT0.RData")
my.at <- seq(-0.5, 0.5, 0.01)
levelplot(gdd50_x4Slope_0, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 BaseT0 Linear Trend, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <- seq(-0.5, 0.5, 0.01)
levelplot(gdd450_x4Slope_0, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450x4 BaseT0 Linear Trend, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <- seq(0.0, 0.05, 0.001)
levelplot(gdd50_x4pval_0, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 BaseT0 Linear Trend-pval, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 0.05, 0.001)
levelplot(gdd450_x4pval_0, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450x4 BaseT0 Linear Trend-pval, TopoWx 1948-2016")+
  layer(sp.polygons(states))



# plots - standard devs
#load("./fixed/bothBaseT/sDevsBothBaseT.RData")
# load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")
my.at <-  seq(0.0, 20, 0.5)
levelplot(sdGDD50, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Sdev GDD50-BaseT10 DOY, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 20, 0.5)
levelplot(sdGDD450, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Sdev GDD450-BaseT10 DOY, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 20, 0.5)
levelplot(sdDiff50_450, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Sdev GDD450-GDD50 Diff-BaseT10 DOY, TopoWx 1948-2016")+
  layer(sp.polygons(states))
# base T0
my.at <-  seq(0.0, 20, 0.5)
levelplot(sdGDD50_0, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Sdev GDD50-BaseT0 DOY, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 20, 0.5)
levelplot(sdGDD450_0, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Sdev GDD450-BaseT0 DOY, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 20, 0.5)
levelplot(sdDiff50_450_0, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Sdev GDD450-GDD50 Diff-BaseT0 DOY, TopoWx 1948-2016")+
  layer(sp.polygons(states))
# SDev ratio 450 v 50
my.at <-  seq(0.0, 1.5, 0.05)
levelplot(sdDiff50_450/sdGDD50, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Sdev GDD450-GDD50 Diff-BaseT10 DOY/Sdev GDD50, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 1.5, 0.05)
levelplot(sdDiff50_450_0/sdGDD50_0, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Sdev GDD450-GDD50 Diff-BaseT0 DOY/Sdev GDD50, TopoWx 1948-2016")+
  layer(sp.polygons(states))

# SDev ratio 450 v 50 detrended 
my.at <-  seq(0.7, 1.1, 0.01)
levelplot(sdDiff50_450Res/sdGDD50Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Det Sdev GDD450-GDD50 Diff-BaseT10 DOY/Sdev GDD50, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 1.5, 0.05)
levelplot(sdDiff50_450_0Res/sdGDD50_0Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Det Sdev GDD450-GDD50 Diff-BaseT0 DOY/Sdev GDD50, TopoWx 1948-2016")+
  layer(sp.polygons(states))
# SDev ratio 250 v 50
my.at <-  seq(0.0, 1.5, 0.05)
levelplot(sdDiff250_50Res/sdGDD50Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Det Sdev GDD250-GDD50 Diff-BaseT10 DOY/Sdev GDD50, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 1.5, 0.05)
levelplot(sdDiff250_50_0Res/sdGDD50_0Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main=" Det Sdev GDD250-GDD50 Diff-BaseT0 DOY/Sdev GDD50, TopoWx 1948-2016")+
  layer(sp.polygons(states))
# SDev ratio 250 v 450
my.at <-  seq(0.0, 1.5, 0.05)
levelplot(sdDiff450_250Res/sdGDD250Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Det Sdev GDD450-GDD250 Diff-BaseT10 DOY/Sdev GDD250, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 1.5, 0.05)
levelplot(sdDiff450_250_0Res/sdGDD250_0Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main=" Det Sdev GDD450-GDD250 Diff-BaseT0 DOY/Sdev GDD250, TopoWx 1948-2016")+
  layer(sp.polygons(states))

vars<-stack(corRasterDet_BaseT0[[1]], sdDiff50_450_0Res/sdGDD50_0Res)
names(vars)<-c("corr","ratio")
hexbinplot(corr~ratio, data=vars, alpha=0.5, type=c("r","g"))
#plot(sdDiff50_450_0Res/sdGDD50_0Res,corRasterDet_BaseT0[[1]], maxpixels=700000, xlim=c(0.7, 1.05))

my.at <-  seq(0.0, 6.0, 0.1)
levelplot(sdGDD450_0Res/sdGDD50_0Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Det sdevGDD450/sdevGDD50 BaseT0, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 6.0, 0.1)
levelplot(sdGDD450Res/sdGDD50Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Det sdevGDD450/sdevGDD50 BaseT10, TopoWx 1948-2016")+
  layer(sp.polygons(states))

#writeRaster(sdDiff50_450Res, filename="sdev450-50BaseT10.asc", format="ascii", overwrite=TRUE)

# shapiro test for normality
# load("./fixed/bothBaseT/detrendedBaseT10.RData")
# ShapiroFun <- function(x) { result=shapiro.test(x); result$p.value }
#   gdd50_x4Resid[is.na(gdd50_x4Resid)] <- 0
# beginCluster(7)
#   sdGDD50 <- clusterR(gdd50_x4Resid, calc, args=list(fun=shapiro.test))
# endCluster()
# my.at <-  seq(0.0, 4.0, 0.1)
# levelplot(sdGDD450_0Res/sdGDD50_0Res, par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Det Sdev GDD450-GDD50 Diff-BaseT10 DOY/Sdev GDD50, TopoWx 1948-2016")+
#   layer(sp.polygons(states))

# Coeff of Variation


# linear model  BASE 10----
# load mask
maskNA<-raster("./fixed/maskNAalt.grd") 
# load datasets
load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")
meanDayGDD50<-raster("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
#meanDayGDD250<-raster("./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd")
meanDayGDD450<-raster("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")

# layerstats
meanDiff<-meanDayGDD450-meanDayGDD50
temp<-stack(mask((sdDiff50_450Res)/(sdGDD50Res),maskNA), mask(corRasterDet_BaseT10[[1]],maskNA))
#temp<-stack(mask(sdDiff50_450Res,maskNA), mask(corRasterDet_BaseT10[[1]],maskNA))
#temp<-stack(mask(sdGDD50Res,maskNA), mask(corRasterDet_BaseT10[[1]],maskNA))
#temp<-stack(mask(sdDiff50_450Res,maskNA), mask(sdGDD50Res,maskNA))
temp <- crop(temp, extent(-100, -80, 35, 45))
layerStats(temp, 'pearson', na.rm = TRUE)
# 
corr <- cut(mask(corRasterDet_BaseT10[[1]],maskNA), seq(0.1, 0.9, 0.1))
temp<-stack(mask(sdDiff50_450Res,maskNA), mask(sdGDD50Res,maskNA), corr)
names(temp) <- c('duration', 'start', 'corr')
temp <- crop(temp, extent(-100, -80, 35, 45))
xyplot(duration ~ start, groups = corr, data = temp, pch=20, type=c("p","g","r"),
       auto.key = list(space = 'right'),
       alpha = 0.2)
# 
# gdd50 <- cut( mask(sdGDD50Res,maskNA), 8)
# temp<-stack(mask(sdDiff50_450Res,maskNA)/mask(sdGDD50Res,maskNA), gdd50, mask(corRasterDet_BaseT10[[1]],maskNA))
# names(temp) <- c('index', 'start', 'corr')
# temp <- crop(temp, extent(-100, -80, 35, 45))
# xyplot(index ~ corr, groups = start, data = temp, pch=20, type=c("p","g","r"),
#        auto.key = list(space = 'right'),
#        alpha = 0.1)

# modeling - plot prediction and resid 
# 50v450
    dataStack<- stack(mask(corRasterDet_BaseT10[[1]],maskNA), 
                      mask(sdDiff50_450Res,maskNA),
                      mask(sdGDD50Res,maskNA),
                      mask(meanDayGDD50,maskNA))
    v <- data.frame((values(dataStack)))
    #names(v) <- c('Pearson', 'mean 10C to GDD50 doy', 'Sdev 10C to GDD50 doy','GDD50 to GDD450 doy',
    #              'Sdev GDD50 to GDD450 doy','mean >10C doy', 'sdev >10C doy', 'mean GDD450 doy', 'sdev GDD450 doy')
    names(v) <- c('pearsonCorr','meanDiffGDD450_50SD','meanDayGDD50SD','meanDayGDD50')
    
    #v<-v[,c(1:3)]
    
    lmBest <- lm(pearsonCorr~meanDiffGDD450_50SD+meanDayGDD50SD+meanDayGDD50,
                 data=v) 
    summary(lmBest)

# 50v250 model
    # modeling - plot prediction and resid 
    dataStack<- stack(mask(corRasterDet_BaseT10_250v50[[1]],maskNA), 
                      mask(sdDiff250_50Res,maskNA),
                      mask(sdGDD50Res,maskNA),
                      mask(meanDayGDD50,maskNA))
    v <- data.frame((values(dataStack)))
    #names(v) <- c('Pearson', 'mean 10C to GDD50 doy', 'Sdev 10C to GDD50 doy','GDD50 to GDD450 doy',
    #              'Sdev GDD50 to GDD450 doy','mean >10C doy', 'sdev >10C doy', 'mean GDD450 doy', 'sdev GDD450 doy')
    names(v) <- c('pearsonCorr','meanDiffGDD250_50SD','meanDayGDD50SD','meanDayGDD50')
    
    #v<-v[,c(1:3)]
    
    lmBest <- lm(pearsonCorr~meanDiffGDD250_50SD+meanDayGDD50SD+meanDayGDD50,
                 data=v) 
    summary(lmBest)

# 250v450 model
    # modeling - plot prediction and resid 
    dataStack<- stack(mask(corRasterDet_BaseT10_450v250[[1]],maskNA), 
                      mask(sdDiff450_250Res,maskNA),
                      mask(sdGDD250Res,maskNA),
                      mask(meanDayGDD250,maskNA))
    v <- data.frame((values(dataStack)))
    #names(v) <- c('Pearson', 'mean 10C to GDD50 doy', 'Sdev 10C to GDD50 doy','GDD50 to GDD450 doy',
    #              'Sdev GDD50 to GDD450 doy','mean >10C doy', 'sdev >10C doy', 'mean GDD450 doy', 'sdev GDD450 doy')
    names(v) <- c('pearsonCorr','meanDiffGDD450_250SD','meanDayGDD250SD','meanDayGDD250')
    
    #v<-v[,c(1:3)]
    
    lmBest <- lm(pearsonCorr~meanDiffGDD450_250SD+meanDayGDD250SD+meanDayGDD250,
                 data=v) 
    summary(lmBest)
    


# R Stats Book ----
pairs(v, panel=panel.smooth)
library(mgcv)
par(mfrow=c(2,2))
gamBest <- gam(pearsonCorr~s(meanDiffGDD450_50SD)+s(meanDayGDD50SD)+s(meanDayGDD50),
             data=v) 
plot(gamBest)
summary(gamBest)
library(tree)
treeBest<-tree(pearsonCorr~., data=v)
plot(treeBest)
text(treeBest)
# ----

temp<- stack( 
             mask(sdDiff50_450Res,maskNA),
             mask(sdGDD50Res,maskNA),
             mask(meanDayGDD50,maskNA))
names(temp) <- c('meanDiffGDD450_50SD','meanDayGDD50SD','meanDayGDD50')
r1 <- predict(temp, lmBest, progress='text')
my.at <- seq(-1, 1, 0.1)
levelplot(r1, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="PREDICTED Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))
my.at <- seq(-0.3, 0.3, 0.01)
levelplot(corRasterDet_BaseT10[[1]]-r1, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="PREDICTED-OBSERVED Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT10[[1]], par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Observed Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))

# linear model BASE 0----
# load mask
maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd") 
# load datasets
load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")
load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")
meanDayGDD50<-raster("./fixed/X4_meanDOY_baseT0_thresh50_1981-2010.grd")
meanDayGDD250<-raster("./fixed/X4_meanDOY_baseT0_thresh250_1981-2010.grd")

# layerstats
temp<-stack(mask(sdDiff50_450_0Res/sdGDD50_0Res,maskNA_0), mask(corRasterDet_BaseT0[[1]],maskNA_0))
layerStats(temp, 'pearson', na.rm = TRUE)

# modeling - plot prediction and resid 
    # 450v50
    dataStack<- stack(mask(corRasterDet_BaseT0[[1]],maskNA_0), 
                      mask(sdDiff50_450_0Res,maskNA_0),
                      mask(sdGDD50_0Res,maskNA_0),
                      mask(meanDayGDD50,maskNA_0))
    v <- data.frame((values(dataStack)))
    #names(v) <- c('Pearson', 'mean 10C to GDD50 doy', 'Sdev 10C to GDD50 doy','GDD50 to GDD450 doy',
    #              'Sdev GDD50 to GDD450 doy','mean >10C doy', 'sdev >10C doy', 'mean GDD450 doy', 'sdev GDD450 doy')
    names(v) <- c('pearsonCorr','meanDiffGDD450_50SD','meanDayGDD50SD','meanDayGDD50')
    
    #v<-v[,c(1:3)]
    
    lmBest <- lm(pearsonCorr~meanDiffGDD450_50SD+meanDayGDD50SD+meanDayGDD50,
                 data=v) 
    summary(lmBest)

    # 50v250
    dataStack<- stack(mask(corRasterDet_BaseT0[[1]],maskNA_0), 
                      mask(sdDiff250_50_0Res,maskNA_0),
                      mask(sdGDD50_0Res,maskNA_0),
                      mask(meanDayGDD50,maskNA_0))
    v <- data.frame((values(dataStack)))
    #names(v) <- c('Pearson', 'mean 10C to GDD50 doy', 'Sdev 10C to GDD50 doy','GDD50 to GDD450 doy',
    #              'Sdev GDD50 to GDD450 doy','mean >10C doy', 'sdev >10C doy', 'mean GDD450 doy', 'sdev GDD450 doy')
    names(v) <- c('pearsonCorr','meanDiffGDD250_50SD','meanDayGDD50SD','meanDayGDD50')
    
    #v<-v[,c(1:3)]
    
    lmBest <- lm(pearsonCorr~meanDiffGDD250_50SD+meanDayGDD50SD+meanDayGDD50,
                 data=v) 
    summary(lmBest)

    # 250v450
    dataStack<- stack(mask(corRasterDet_BaseT0[[1]],maskNA_0), 
                      mask(sdDiff450_250_0Res,maskNA_0),
                      mask(sdGDD250_0Res,maskNA_0),
                      mask(meanDayGDD250,maskNA_0))
    v <- data.frame((values(dataStack)))
    #names(v) <- c('Pearson', 'mean 10C to GDD50 doy', 'Sdev 10C to GDD50 doy','GDD50 to GDD450 doy',
    #              'Sdev GDD50 to GDD450 doy','mean >10C doy', 'sdev >10C doy', 'mean GDD450 doy', 'sdev GDD450 doy')
    names(v) <- c('pearsonCorr','meanDiffGDD450_250SD','meanDayGDD250SD','meanDayGDD250')
    
    #v<-v[,c(1:3)]
    
    lmBest <- lm(pearsonCorr~meanDiffGDD450_250SD+meanDayGDD250SD+meanDayGDD250,
                 data=v) 
    summary(lmBest)
    

# https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html
library(olsrr)
library(arm)
coefplot(lmBest)
library(visreg)
visreg(lmBest)

# R Stats Book ----
pairs(v, panel=panel.smooth)
library(mgcv)
par(mfrow=c(2,2))
gamBest <- gam(pearsonCorr~s(meanDiffGDD250_50SD)+s(meanDayGDD50SD)+s(meanDayGDD50),
               data=v) 
plot(gamBest)
summary(gamBest)
library(tree)
treeBest<-tree(pearsonCorr~., data=v)
plot(treeBest)
text(treeBest)
# ----


# plot map of resids
temp<- stack( 
  mask(sdDiff50_450_0Res,maskNA_0),
  mask(sdGDD50_0Res,maskNA_0),
  mask(meanDayGDD50,maskNA_0))
names(temp) <- c('meanDiffGDD450_50SD','meanDayGDD50SD','meanDayGDD50')

r1 <- predict(temp, lmBest, progress='text')
my.at <- seq(-1, 1, 0.1)
levelplot(r1, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="PREDICTED Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))
my.at <- seq(-0.3, 0.3, 0.01)
levelplot(corRasterDet_BaseT0[[1]]-r1, par.settings = BuRdTheme, margin=FALSE, at=my.at, main="PREDICTED-OBSERVED Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT0[[1]], par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Observed Pearson r GDD50 v GDD450 1948-2016 TopoWx")+ 
  layer(sp.polygons(states))


# scatteplot 
# obs v pred
vars<-stack(corRasterDet_BaseT0[[1]], r1)
names(vars)<-c("observed","predicted")
hexbinplot(observed~predicted, data=vars, alpha=0.5, type=c("r","g"))

# corr vs DOY
vars<-stack(sdDiff50_450Res, meanDayGDD50)
names(vars)<-c("corr","GDD50")
hexbinplot(GDD50~corr, data=vars, alpha=0.5, type=c("r","g"))
