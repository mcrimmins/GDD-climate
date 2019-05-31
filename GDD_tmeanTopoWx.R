# calculate GDD doy from daily tmin and tmax TopoWx data
# MAC 08/24/17

#Load necessary packages
library(raster)
library(ncdf4)
library(parallel)

# set rasteroptions
rasterOptions(progress = 'text')

# download test files
#download.file("https://download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmax/tmax_2016.nc", destfile = "./topoWxData/tmax_2016.nc", method="curl")
#download.file("https://download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmin/tmin_2016.nc", destfile = "./topoWxData/tmin_2016.nc", method="curl")

#Set call to dataset via OpenDap
 yr<-2016
 topowx.min <- paste0("/scratch/crimmins/topowx/download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmin/tmin_",yr,".nc")
 topowx.max <- paste0("/scratch/crimmins/topowx/download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmax/tmax_",yr,".nc")

# local files
topowx.min <- paste0("./topoWxData/tmin_",yr,".nc")
topowx.max <- paste0("./topoWxData/tmax_",yr,".nc")


#"load" data into R via Raster package
tmin <- brick(topowx.min, varname="tmin",  ncdf=TRUE)
tmax <- brick(topowx.max, varname="tmax",  ncdf=TRUE)

# point extract testing...
# hard code in points
# point<-data.frame(36.176936,-86.776413) # Nashville
# #point<-data.frame(41.676432,-93.544581) # Des Moines
# colnames(point)<-c("y","x")
# # get time series
# tminTS<-t(raster::extract(tmin, cellFromXY(tmin, c(point$x,point$y))))
# tmaxTS<-t(raster::extract(tmax, cellFromXY(tmax, c(point$x,point$y))))



# subset for testing - Jan 1 thru July 1
#tmin <- subset(tmin, 1:182)
#tmax <- subset(tmax, 1:182)

# crop extent
# e <- extent(-100, -66.7, 24.1, 51.2)
# tminCrop <- crop(tmin, e)


# subset for testing
#tmin <- subset(tmin, 1:30)
#tmax <- subset(tmax, 1:30)

# grid functions
f1<-function(x,y){return((x+y)/2)}
f2<-function(i){return(overlay(tmin[[i]], tmax[[i]], fun=f1, unstack=TRUE,overwrite=TRUE,filename=paste0("/home/crimmins/RProjects/TopoWx/tmpFiles/tavg_",tmin[[i]]@z[[1]])))}

# no parallel
ptm <- proc.time()
  tavg<-lapply(1:3, f2)
proc.time() - ptm

# trying parallel on this
cores <- detectCores() - 1
cl <- makeCluster(cores)
  ptm <- proc.time()
  tavg<-mclapply(1:nlayers(tmin), f2)
  proc.time() - ptm
# one or more parLapply calls
stopCluster(cl)


# create rasterbrick from temp files
fileList<-list.files(path="/home/crimmins/RProjects/TopoWx/tmpFiles",pattern="*.gri", full.names=TRUE)
temp <- sapply(fileList, raster)  # this is equivalent to your for loop 
tavg <- stack(temp) 
# ptm <- proc.time()
#   tavg <- lapply(1:10, overlay(tmin[[1]], tmax[[1]], fun=f1, unstack=TRUE, filename=paste0("/home/crimmins/RProjects/TopoWx/tmpFiles/tavg_",tmin[[1]]@z[[1]]))) # runs for hours
# proc.time() - ptm

# test write of netcdf
# writeRaster(tavg, filename='test.nc', format="CDF", overwrite=TRUE, datatype='FLT4S')

# breaks after about 54 layers
# for(i in 1:nlayers(tmin)){
#   tempGrid <- overlay(tmin[[i]], tmax[[i]], fun=function(x,y){return((x+y)/2)})
#   if (i==1){
#     tavg <- tempGrid
#   }else{
#     tavg <- stack(tavg, tempGrid) # brick or stack?
#   }
#     print(i)
# }

# removeTmpFiles(h=0) # clears out temp files

plot(tavg, 1, col=colorRampPalette(c("blue", "yellow", "red"))(length(seq(-20, 30, by = 5))-1), breaks= seq(-20, 30, by = 5))


# Test DOY calculation

baseT<-10 # O or 10C (32F and 50F)
thresh<-100
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


tavgSub <- subset(tavg, 1:366)

ptm <- proc.time()
  tempGrid <- calc(x=tavgSub, fun=GDDdoy)
proc.time() - ptm


# parallell calc
baseT<-10 # O or 10C (32F and 50F)
thresh<-100
GDDdoy2<- function(x){
  x[x < baseT] <- 0 # baseT
  x<-cumsum(x)
  doy<-which.max(x >= thresh) # thresh
  if(length(doy)==0){
    doy<-NA
  }else{
    doy<-doy
  }
  return(doy)
}

ptm <- proc.time()
  beginCluster(3)
  tempGrid <- clusterR(tavgSub, calc, args=list(fun=GDDdoy2), export=c('baseT','thresh'))
  endCluster()
proc.time() - ptm

# clean up rasters
# https://www.rdocumentation.org/packages/raster/versions/2.5-8/topics/rasterTmpFile
# https://stackoverflow.com/questions/25426405/raster-package-taking-all-hard-drive
#rasterTmpFile(prefix='r_tmp_')
showTmpFiles()
removeTmpFiles(h=0) # in hours


