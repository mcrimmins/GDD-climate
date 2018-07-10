# TopoWx Normals
# 5/31/18 MAC

library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)
library(parallel)

states <- getData('GADM', country='United States', level=1)
#clusterpoly <- readShapePoly("./mapFiles/cluster12finalFIXED")
#clusterpoly <- readShapePoly("./mapFiles/cluster15finalFIXED_ZProd")
#load("./fixed/cluster15classMap.RData")

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')

# load data
# load mask
maskNA<-raster("./fixed/maskNAalt.grd") 

# load normals
tmaxNorm<-stack("./normals/normals_tmax.nc")
  tmaxNorm <- aggregate(tmaxNorm, fact=4, fun=mean)
tminNorm<-stack("./normals/normals_tmin.nc")
  tminNorm <- aggregate(tminNorm, fact=4, fun=mean)
#gdd50raw_x4 <- mask(gdd50raw_x4, maskNA)

# calculate mean
  # grid functions
  f1<-function(x,y){return((x+y)/2)}
  f2<-function(i){return(overlay(tmaxNorm[[i]], tminNorm[[i]], fun=f1, unstack=TRUE))}
  # calc daily avgs using parallel
  cores <- detectCores() - 1
  cl <- makeCluster(cores)
  ptm <- proc.time()
    tmeanNorm<-mclapply(1:nlayers(tmaxNorm), f2)
  proc.time() - ptm
  stopCluster(cl)
  tmeanNorm<-stack(tmeanNorm)
  names(tmeanNorm)<-month.abb[seq(1,12,1)]
  
# plot normals
  my.at <- seq(-2, 2, 1)
  levelplot( tmeanNorm[[1:6]], par.settings = BuRdTheme, margin=FALSE, at=my.at, main="Monthly Avg Temps - -2-2C Range")+ 
    layer(sp.polygons(states))
