# Potential fig 1 plot
# TopoWx 01/19/18
# adjusted for new, corrected GDD grids

#library(cowplot)
library(raster)
library(rasterVis)
library(sp)
library(maptools)
library(gridExtra)
#library(RStoolbox)
#library(tidyr)
#library(dplyr)
library(parallel)

# set rasteroptions
rasterOptions(progress = 'text')
tmpDir(create=TRUE)

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)
USA<-getData('GADM', country='USA', level=0)

# # # or anoms
gdd50_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd250_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh250_1981-2010.grd")
gdd450_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh450_1981-2010.grd")

# mask out elevations   
maskNA<-raster("./fixed/maskNAalt.grd") 
gdd50_x4 <-  mask(gdd50_x4, maskNA)  
gdd250_x4 <- mask(gdd250_x4, maskNA)
gdd450_x4 <- mask(gdd450_x4, maskNA)
# mask out Canada
gdd50_x4 <-  mask(gdd50_x4, USA)  
gdd250_x4 <- mask(gdd250_x4, USA)
gdd450_x4 <- mask(gdd450_x4, USA)

# standardize grids with z-scores
zscore<-function(x){scale(x, center = TRUE, scale = TRUE)}
ptm <- proc.time()
  beginCluster(6)
    gdd50_x4_Z<- clusterR(gdd50_x4, calc, args=list(fun=zscore))
    gdd250_x4_Z<- clusterR(gdd250_x4, calc, args=list(fun=zscore))
    gdd450_x4_Z<- clusterR(gdd450_x4, calc, args=list(fun=zscore))
endCluster()
proc.time() - ptm  

# ---- Plot Mean DOYs
meanDOY50_x4<-raster("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
  meanDOY50_x4 <- mask(meanDOY50_x4, maskNA)
  meanDOY50_x4 <- mask(meanDOY50_x4, USA)
meanDOY250_x4<-raster("./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd")
  meanDOY250_x4 <- mask(meanDOY250_x4, maskNA)
  meanDOY250_x4 <- mask(meanDOY250_x4, USA)
meanDOY450_x4<-raster("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
  meanDOY450_x4 <- mask(meanDOY450_x4, maskNA)
  meanDOY450_x4 <- mask(meanDOY450_x4, USA)
# set names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")

# plot mean DOY
# DOY colorramp
mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))

meanDOY<-stack(meanDOY50_x4,meanDOY250_x4,meanDOY450_x4)
names(meanDOY)<-c("GDD50","GDD250","GDD450")
my.at <- seq(0, 240, 10)
meanFig<-levelplot(meanDOY, layout=c(1,3),at=my.at, par.settings = mapTheme,
          margin=FALSE, main="a.) Mean GDD", colorkey=list(space="bottom"),
          sub=list(label="Day of Year",cex=0.75,font = 1),
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE)) + 
          layer(sp.polygons(states,col = 'gray40', lwd=0.1))

# # plot mean DOY diffs
# meanDOYdiffs<-stack(meanDOY450_x4-meanDOY50_x4,meanDOY250_x4-meanDOY50_x4,meanDOY450_x4-meanDOY250_x4)
# names(meanDOYdiffs)<-c("GDD450-50","GDD250-50","GDD450-250")
# my.at <- seq(0, 80, 5)
# meanDiffFig<-levelplot(meanDOYdiffs, layout=c(1,3),at=my.at, par.settings = viridisTheme,
#                    margin=FALSE, main="Diffs in Mean GDD DOY", colorkey=list(space="bottom"))

# plot anomaly years
# GDD2012<-stack(gdd50_x4[[58]],gdd250_x4[[58]],gdd450_x4[[58]],gdd50_x4[[63]],gdd250_x4[[63]],gdd450_x4[[63]],
#                gdd50_x4[[65]],gdd250_x4[[65]],gdd450_x4[[65]])
# names(meanDOY)<-c("2005 GDD50","2005 GDD250","2005 GDD450","2010 GDD50","2010 GDD250","2010 GDD450","2012 GDD50","2012 GDD250","2012 GDD450")
# my.at <- seq(-50, 50, 1)
# anomFig<-levelplot(GDD2012, layout=c(3,3), par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD Anomalies")+ 
#      layer(sp.polygons(states))
 # GDD2005<-stack(gdd50_x4[[58]],gdd250_x4[[58]],gdd450_x4[[58]])
 # names(GDD2005)<-c("2005 GDD50","2005 GDD250","2005 GDD450")
 # GDD2010<-stack(gdd50_x4[[63]],gdd250_x4[[63]],gdd450_x4[[63]])
 # names(GDD2010)<-c("2010 GDD50","2010 GDD250","2010 GDD450")
 # GDD2012<-stack(gdd50_x4[[65]],gdd250_x4[[65]],gdd450_x4[[65]])
 # names(GDD2012)<-c("2012 GDD50","2012 GDD250","2012 GDD450")
 
 # z-score version
 GDD2005<-stack(gdd50_x4_Z[[2]],gdd250_x4_Z[[2]],gdd450_x4_Z[[2]])
 names(GDD2005)<-c("GDD50","GDD250","GDD450")
 GDD2010<-stack(gdd50_x4_Z[[29]],gdd250_x4_Z[[29]],gdd450_x4_Z[[29]])
 names(GDD2010)<-c("GDD50","GDD250","GDD450")
 GDD2012<-stack(gdd50_x4_Z[[63]],gdd250_x4_Z[[63]],gdd450_x4_Z[[63]])
 names(GDD2012)<-c("GDD50","GDD250","GDD450")
 
my.at <- seq(-3, 3, 0.1) # my.at <- seq(-50, 50, 1)
  GDD2005Fig<-levelplot(GDD2005, layout=c(1,3), par.settings = RdBuTheme, at=my.at,
                        margin=FALSE, main="b.) 1949",  colorkey=list(space="bottom"),
                        sub=list(label="Z-score GDD Anomaly",cex=0.75,font = 1),
                        xlab=NULL, ylab=NULL, scales=list(draw=FALSE))+  
                        layer(sp.polygons(states,col = 'gray40', lwd=0.1))
  GDD2010Fig<-levelplot(GDD2010, layout=c(1,3), par.settings = RdBuTheme, at=my.at,
                        colorkey=list(space="bottom"), margin=FALSE, main="c.) 1976",
                        sub=list(label="Z-score GDD Anomaly",cex=0.75,font = 1),
                        xlab=NULL, ylab=NULL, scales=list(draw=FALSE)) +
                        layer(sp.polygons(states,col = 'gray40', lwd=0.1))
  GDD2012Fig<-levelplot(GDD2012, layout=c(1,3), par.settings = RdBuTheme, at=my.at,
                        margin=FALSE, main="d.) 2010",  
                        colorkey=list(space="bottom"),
                        #colorkey=FALSE,
                        sub=list(label="Z-score GDD Anomaly",cex=0.75,font = 1),
                        xlab=NULL, ylab=NULL, scales=list(draw=FALSE))+ 
                        layer(sp.polygons(states,col = 'gray40', lwd=0.1))
  
#grid.arrange(meanFig, anomFig, ncol=2)
# print(meanFig, split=c(1,1,4,1), more=TRUE)
# print(GDD2005Fig, split=c(2,1,4,1), more=TRUE)
# print(GDD2010Fig, split=c(3,1,4,1), more=TRUE)
# print(GDD2012Fig, split=c(4,1,4,1))

# plot to png
png("/home/crimmins/RProjects/TopoWx/clusterTrends/figs/supp1_exampleDOY.png", width = 7, height = 5, units = "in", res = 300L)
#grid.newpage()
print(meanFig, split=c(1,1,4,1), more=TRUE)
print(GDD2005Fig, split=c(2,1,4,1), more=TRUE)
print(GDD2010Fig, split=c(3,1,4,1), more=TRUE)
print(GDD2012Fig, split=c(4,1,4,1))
dev.off()
