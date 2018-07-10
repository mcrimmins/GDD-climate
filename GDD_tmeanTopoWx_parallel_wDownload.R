# calculate GDD doy from daily tmin and tmax TopoWx data
# MAC 08/24/17
# CUSTOM SCRIPT FOR CYVERSE INSTANCE

#Load necessary packages
library(raster)
library(ncdf4)
library(parallel)

# start clock to time script
ptm <- proc.time()

# set rasteroptions
rasterOptions(progress = 'text')

# set directories - only needed for CyVerse instances
dir.create("./tmpFiles")

# grid functions
f1<-function(x,y){return((x+y)/2)}
f2<-function(i){return(overlay(tmin[[i]], tmax[[i]], fun=f1, unstack=TRUE,overwrite=TRUE,
                               filename=paste0("./tmpFiles/tavg_",tmin[[i]]@z[[1]])))}

baseT<-0 # O or 10C (32F and 50F)
thresh<-250 # 50, 100, 200, 500
GDDdoy<- function(x){
  x<-ifelse(x>=baseT, x-baseT, 0)
  #x[x < baseT] <- 0
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
  
  # temporarily download year files and then delete
  # download test files; changed to "wget" from "curl"
  print("Downloading TopoWx Files")
  download.file(paste0("https://download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmax/tmax_",i,".nc"), destfile = "./tmpFiles/tmax.nc", method="curl")
  download.file(paste0("https://download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmin/tmin_",i,".nc"), destfile = "./tmpFiles/tmin.nc", method="curl")
  print("Done downloading")
  
  # read in netcdfs
  topowx.min <- "tmax.nc"
  topowx.max <- "tmin.nc"
  #"load" data into R via Raster package
  tmin <- brick(topowx.min, varname="tmin",  ncdf=TRUE)
  tmax <- brick(topowx.max, varname="tmax",  ncdf=TRUE)

  # calc daily avgs using parallel
  cores <- detectCores() - 1
  cl <- makeCluster(cores)
    ptm <- proc.time()
    tavg<-mclapply(1:nlayers(tmin), f2)
    proc.time() - ptm
  stopCluster(cl)

  # create rasterbrick from temp files
  fileList<-list.files(path="./tmpFiles",pattern="*.gri", full.names=TRUE)
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
  do.call(file.remove, list(list.files("./tmpFiles", full.names = TRUE)))
} 

# set names of layers to years
names(tempGrid2)<-as.character(seq(1948, 2016, by=1))

# write out tempGrid2 stack -- large file and may not be necessary
writeRaster(tempGrid2,filename="rawDOY_baseT0_thresh250.grd", overwrite=TRUE )
  
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
writeRaster(meanDOYsub,filename="meanDOY_baseT0_thresh250_1981-2010.grd", overwrite=TRUE )

# calc anomalies
anom <- function(x, y) { 
  x - y 
} 
print("Calculating DOY anomalies")
anomDOY <- overlay(x = tempGrid2, y = meanDOYsub, fun = anom) # change base period to meanDOYsub if needed

# set names of layers to years
#names(anomDOY)<-as.character(seq(1981, 2017, by=1))

# write Raster to file
writeRaster(anomDOY,filename="anomDOY_baseT0_thresh250_1981-2010.grd", overwrite=TRUE)

# end timer
proc.time() - ptm


# clean up rasters
# https://www.rdocumentation.org/packages/raster/versions/2.5-8/topics/rasterTmpFile
# https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive
#rasterTmpFile(prefix='r_tmp_')
#showTmpFiles()
#removeTmpFiles(h=0) # in hours


