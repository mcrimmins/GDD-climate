# calculate GDD doy from daily tmin and tmax TopoWx data
# MAC 08/24/17
# adapted from GDD_tmeanTopoWx_parallel.R
# MAC 3/11/18

#Load necessary packages
library(raster)
library(ncdf4)
library(parallel)

# set rasteroptions
rasterOptions(progress = 'text')

# load cluster map ----
load("~/RProjects/TopoWx/cluster12classMap.RData")
rm(maskNA, states)
classMap<-as.factor(unC$map)
rat <- levels(classMap)[[1]]
# cluster names
rat[["cluster"]]<-c("N Rockies","Northeast","S Plains","S Rockies","N Plains",
                    "Midwest","Southeast","Mid Atlantic","Upper Midwest","Southwest",
                    "Gulf Coast","Pacific NW")
#rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 
# ----

# reduce the cell size by 50% and double the number of rows and columns.      
inputRaster <- raster("~/RProjects/TopoWx/mapFiles/elev.nc")      
inCols <- ncol(inputRaster)
inRows <- nrow(inputRaster)
resampledRaster <- raster(ncol=(inCols), nrow=(inRows))
extent(resampledRaster) <- extent(inputRaster)
classMapOrigRes <- resample(classMap,resampledRaster,method='ngb')
classMapOrigRes<-as.factor(classMapOrigRes)
rat <- levels(classMapOrigRes)[[1]]
# cluster names
rat[["cluster"]]<-c("N Rockies","Northeast","S Plains","S Rockies","N Plains",
                    "Midwest","Southeast","Mid Atlantic","Upper Midwest","Southwest",
                    "Gulf Coast","Pacific NW")
#rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMapOrigRes) <- rat 



# grid functions
f1<-function(x,y){return((x+y)/2)}
f2<-function(i){return(overlay(tmin[[i]], tmax[[i]], fun=f1, unstack=TRUE,overwrite=TRUE,
                               filename=paste0("/home/crimmins/RProjects/TopoWx/tmpFiles/tavg_",tmin[[i]]@z[[1]])))}

baseT<-10 # O or 10C (32F and 50F)
thresh<-200 # 50, 100, 200, 500
GDDdoy<- function(x){
  x[x < baseT] <- 0
  x<-cumsum(x)
  doy<-which.max(x >= thresh)
  if(length(doy)==0){
    doy<-NA
  }else{
    doy<-doy
  }
  return(doy)
}

#Set years
yr1<-1948
yr2<-2016

for(i in yr1:yr2){
  paste0(i)
  topowx.min <- paste0("/scratch/crimmins/topowx/download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmin/tmin_",i,".nc")
  topowx.max <- paste0("/scratch/crimmins/topowx/download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmax/tmax_",i,".nc")
  #"load" data into R via Raster package
  tmin <- brick(topowx.min, varname="tmin",  ncdf=TRUE)
  tmax <- brick(topowx.max, varname="tmax",  ncdf=TRUE)

  
  
  ptm <- proc.time()
  # # get zonal mean of each cluster
   zStats<-as.data.frame(t(zonal(tmin, classMapOrigRes, 'mean')))
  # zStats<-zStats[-1,]
  # zStats$yearLayer<-rownames(zStats)
  # zStats<-zStats %>% gather(yearLayer, 1:clusterN)
  # colnames(zStats)<-c("code","cluster","GDDValue")
  # zStats<-separate(zStats,code,c("X","code"), sep ="X")
  # zStats<-separate(zStats,code,c("year","threshold"))
  # zStats$year<-as.numeric(zStats$year)
   ptm <- proc.time()
   
   
  # calc daily avgs using parallel
  cores <- detectCores() - 1
  cl <- makeCluster(cores)
    ptm <- proc.time()
    tavg<-mclapply(1:nlayers(tmin), f2)
    proc.time() - ptm
  stopCluster(cl)

  # create rasterbrick from temp files
  fileList<-list.files(path="/home/crimmins/RProjects/TopoWx/tmpFiles",pattern="*.gri", full.names=TRUE)
  temp <- sapply(fileList, raster)  # this is equivalent to your for loop 
  tavg <- stack(temp) 

# parallell calc
  ptm <- proc.time()
    beginCluster(3)
    tempGrid <- clusterR(tavg, calc, args=list(fun=GDDdoy), export=c('baseT','thresh'))
    endCluster()
  proc.time() - ptm

# store tempGrids in stack  
  if (i==yr1){
    tempGrid2 <- tempGrid
  }else{
    tempGrid2 <- stack(tempGrid2, tempGrid) # brick or stack?
  }
  print(i)

# clean out temp files
  do.call(file.remove, list(list.files("/home/crimmins/RProjects/TopoWx/tmpFiles", full.names = TRUE)))
} 

# set names of layers to years
names(tempGrid2)<-as.character(seq(1948, 2016, by=1))

# write out tempGrid2 stack -- large file and may not be necessary
writeRaster(tempGrid2,filename="rawDOY_baseT10_thresh200.grd", overwrite=TRUE )
  
# ---- stack calculations  
# calculate mean DOY
# print("Calculating mean DOY")
# beginCluster(3)
#   meanDOY <- clusterR(tempGrid2, overlay, args=list(fun=mean))
# endCluster()
# writeRaster(meanDOY,filename="meanDOY_baseT10_thresh100.grd", overwrite=TRUE )

# calculate mean DOY 1981-2010 base period
print("Calculating mean DOY - 1981-2010")
beginCluster(3)
meanDOYsub <- clusterR(tempGrid2[[34:63]], overlay, args=list(fun=mean))
endCluster()
writeRaster(meanDOYsub,filename="meanDOY_baseT10_thresh200_1981-2010.grd", overwrite=TRUE )

# calc anomalies
#tempGrid2<-stack("/home/crimmins/RProjects/TopoWx/cyverse/rawDOY_baseT10_thresh100.grd")
#meanDOYsub<-stack("/home/crimmins/RProjects/TopoWx/cyverse/meanDOY_baseT10_thresh100_1981-2010.grd")
anom <- function(x, y) { 
  x - y 
} 
print("Calculating DOY anomalies")
anomDOY <- overlay(x = tempGrid2, y = meanDOYsub, fun = anom) # change base period to meanDOYsub if needed

# set names of layers to years
#names(anomDOY)<-as.character(seq(1981, 2017, by=1))

# write Raster to file
writeRaster(anomDOY,filename="/home/crimmins/RProjects/TopoWx/cyverse/anomDOY_baseT10_thresh100_1981-2010.grd", overwrite=TRUE)

# clean up rasters
# https://www.rdocumentation.org/packages/raster/versions/2.5-8/topics/rasterTmpFile
# https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive
#rasterTmpFile(prefix='r_tmp_')
#showTmpFiles()
#removeTmpFiles(h=0) # in hours


