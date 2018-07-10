# aggregate and develop mask files for grids...
# pulled code from other scripts like assess_GDD500.R and plot_TopoWx_GDDs.R
# 3/23/18 MAC

library(raster)
library(rasterVis)
library(sp)
library(maptools)

# set rasteroptions
rasterOptions(progress = 'text')

# load grids
gdd50 <-stack("./fixed/anomDOY_baseT10_thresh50_1981-2010.grd")
gdd250<-stack("./fixed/anomDOY_baseT10_thresh250_1981-2010.grd")
gdd450<-stack("./fixed/anomDOY_baseT10_thresh450_1981-2010.grd")

# aggregate to lower resolution grids
gdd50_x4 <- aggregate(gdd50, fact=4, fun=mean)
writeRaster(gdd50_x4,filename="./fixed/X4_anomDOY_baseT10_thresh50_1981-2010.grd", overwrite=TRUE)
gdd450_x4 <- aggregate(gdd450, fact=4, fun=mean)
writeRaster(gdd450_x4,filename="./fixed/X4_anomDOY_baseT10_thresh450_1981-2010.grd", overwrite=TRUE)
gdd250_x4 <- aggregate(gdd250, fact=4, fun=mean)
writeRaster(gdd250_x4,filename="./fixed/X4_anomDOY_baseT10_thresh250_1981-2010.grd", overwrite=TRUE)

#raw grids
gdd50raw<-stack("./fixed/rawDOY_baseT10_thresh50.grd")
  gdd50raw_x4 <- aggregate(gdd50raw, fact=4, fun=mean)
  writeRaster(gdd50raw_x4,filename="./fixed/X4_rawDOY_baseT10_thresh50.grd", overwrite=TRUE)
gdd250raw<-stack("./fixed/rawDOY_baseT10_thresh250.grd")
  gdd250raw_x4 <- aggregate(gdd250raw, fact=4, fun=mean)
  writeRaster(gdd250raw_x4,filename="./fixed/X4_rawDOY_baseT10_thresh250.grd", overwrite=TRUE)
gdd450raw<-stack("./fixed/rawDOY_baseT10_thresh450.grd")
  gdd450raw_x4 <- aggregate(gdd450raw, fact=4, fun=mean)
  writeRaster(gdd450raw_x4,filename="./fixed/X4_rawDOY_baseT10_thresh450.grd", overwrite=TRUE)

# mean DOY grids  
  meanDOY50<-raster("./fixed/meanDOY_baseT10_thresh50_1981-2010.grd")
  meanDOY50_x4 <- aggregate(meanDOY50, fact=4, fun=mean)
  writeRaster(meanDOY50_x4,filename="./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd", overwrite=TRUE)
  
  meanDOY250<-raster("./fixed/meanDOY_baseT10_thresh250_1981-2010.grd")
  meanDOY250_x4 <- aggregate(meanDOY250, fact=4, fun=mean)
  writeRaster(meanDOY250_x4,filename="./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd", overwrite=TRUE)
  
  meanDOY450<-raster("./fixed/meanDOY_baseT10_thresh450_1981-2010.grd")
  meanDOY450_x4 <- aggregate(meanDOY450, fact=4, fun=mean)
  writeRaster(meanDOY450_x4,filename="./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd", overwrite=TRUE)  
  
  
# rm unused vars...
  
    
# create mask
  #idx = which.min(gdd500raw_x4)
  
  # find 0's in GDD50
  gdd50raw_x4<- reclassify(gdd50raw_x4, c(-999,0,1,  1,366,0))
  gdd50raw_x4<-as.factor(gdd50raw_x4)
  sum0GDD50<- sum(gdd50raw_x4)
  
  # find 0's in GDD500
  gdd450raw_x4<- reclassify(gdd450raw_x4, c(-999,0,1,  1,366,0))
  gdd450raw_x4<-as.factor(gdd450raw_x4)
  sum0GDD450<- sum(gdd450raw_x4)
  
  # id all areas that have at least one year with a 0 GDD50 or GDD500 DOY
  sum0GDDall<-sum0GDD50+sum0GDD450
  # set to NA to create mask
  sum0GDDall[sum0GDDall > 0] <- NA
  # plot(sum0GDDall) # check
  
  writeRaster(sum0GDDall,filename="./fixed/maskNA.grd", overwrite=TRUE)
  
 
# alternate mask using original resolution grid
  
#raw grids
gdd50raw<-stack("./fixed/rawDOY_baseT10_thresh50.grd")
gdd250raw<-stack("./fixed/rawDOY_baseT10_thresh250.grd")
gdd450raw<-stack("./fixed/rawDOY_baseT10_thresh450.grd")

# find 0's in GDD50
gdd50raw<- reclassify(gdd50raw, c(-999,0,1,  1,366,0))
gdd50raw<-as.factor(gdd50raw)
sum0GDD50<- sum(gdd50raw)

# find 0's in GDD500
gdd450raw<- reclassify(gdd450raw, c(-999,0,1,  1,366,0))
gdd450raw<-as.factor(gdd450raw)
sum0GDD450<- sum(gdd450raw)

# id all areas that have at least one year with a 0 GDD50 or GDD500 DOY
sum0GDDall<-sum0GDD50+sum0GDD450
# aggregate to coarse res
sum0GDDall <- aggregate(sum0GDDall, fact=4, fun=max) 
# set to NA to create mask
sum0GDDall[sum0GDDall > 0] <- NA
# plot(sum0GDDall) # check  
writeRaster(sum0GDDall,filename="./fixed/maskNAalt.grd", overwrite=TRUE)
  
   # # find elevations of masked out areas ----
  # elev<-raster("./mapFiles/X4_elev.grd")
  # gdd50raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh50.grd")
  # gdd450raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh450.grd")
  # 
  # # find 0's in GDD50
  # gdd50raw_x4<- reclassify(gdd50raw_x4, c(-999,0,1,  1,366,0))
  # gdd50raw_x4<-as.factor(gdd50raw_x4)
  # sum0GDD50<- sum(gdd50raw_x4)
  # 
  # # find 0's in GDD500
  # gdd450raw_x4<- reclassify(gdd450raw_x4, c(-999,0,1,  1,366,0))
  # gdd450raw_x4<-as.factor(gdd450raw_x4)
  # sum0GDD450<- sum(gdd450raw_x4)
  # 
  # # id all areas that have at least one year with a 0 GDD50 or GDD500 DOY
  # sum0GDDall<-sum0GDD50+sum0GDD450
  # # set to NA to create mask
  # sum0GDDall[sum0GDDall > 0] <- 2
  # 
  # zonal(elev, sum0GDDall, 'mean')
  # zonal(elev, sum0GDDall, 'min')
  # zonal(elev, sum0GDDall, 'max')