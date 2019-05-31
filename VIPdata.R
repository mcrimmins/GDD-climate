# read/analyze VIP data
# MAC 04/25/19

library(rgdal)
library(gdalUtils)
library(raster)


# get file list
fileNames<-(list.files("/scratch/crimmins/vip/ndvi/vip.arizona.edu/vipdata/V4/DATAPOOL/PHENOLOGY/"))

# initialize empty stacks
startOfSeason <- stack()
peakSeason<-stack()
numSeasons<-stack()
reliability<-stack()

for(i in 1:length(fileNames)){
  sds <- get_subdatasets(paste0('/scratch/crimmins/vip/ndvi/vip.arizona.edu/vipdata/V4/DATAPOOL/PHENOLOGY/',fileNames[i]))
  # get start of season
  r <- raster(sds[1])
  ry <- flip(r, direction='y')
  extent(ry)=c(xmn=-180, xmx=180, ymn=-90, ymx=90)
  # crop
  e <- extent(-125.0042, -66.67083, 24.09583, 51.19583)
  rc <- crop(ry, e)
  # write to stack
  startOfSeason <- stack(startOfSeason , rc )
  
  # get start of season
  r <- raster(sds[4])
  ry <- flip(r, direction='y')
  extent(ry)=c(xmn=-180, xmx=180, ymn=-90, ymx=90)
  # crop
  e <- extent(-125.0042, -66.67083, 24.09583, 51.19583)
  rc <- crop(ry, e)
  # write to stack
  peakSeason <- stack(peakSeason , rc )

  # get num of seasons
  r <- raster(sds[25])
  ry <- flip(r, direction='y')
  extent(ry)=c(xmn=-180, xmx=180, ymn=-90, ymx=90)
  # crop
  e <- extent(-125.0042, -66.67083, 24.09583, 51.19583)
  rc <- crop(ry, e)
  # write to stack
  numSeasons <- stack(numSeasons , rc )
  
  # get reliability
  r <- raster(sds[26])
  ry <- flip(r, direction='y')
  extent(ry)=c(xmn=-180, xmx=180, ymn=-90, ymx=90)
  # crop
  e <- extent(-125.0042, -66.67083, 24.09583, 51.19583)
  rc <- crop(ry, e)
  # write to stack
  reliability <- stack(reliability, rc)
      
  print(fileNames[i])
}

# write out data layers
writeRaster(startOfSeason,filename="/scratch/crimmins/vip/processed/NDVI_VIP_SOS_1981_2016.grd", overwrite=TRUE )
writeRaster(peakSeason,filename="/scratch/crimmins/vip/processed/NDVI_VIP_peakSeason_1981_2016.grd", overwrite=TRUE )
writeRaster(numSeasons,filename="/scratch/crimmins/vip/processed/NDVI_VIP_numSeasons_1981_2016.grd", overwrite=TRUE )
writeRaster(reliability,filename="/scratch/crimmins/vip/processed/NDVI_VIP_reliability_1981_2016.grd", overwrite=TRUE )

# calculating anomalies
library(parallel)
print("Calculating mean DOY - 1981-2016")
beginCluster(7)
  meanDOYsos <- clusterR(startOfSeason, overlay, args=list(fun=mean))
endCluster()
writeRaster(meanDOYsos,filename="/scratch/crimmins/vip/processed/NDVI_VIP_meanSOS_1981_2016.grd", overwrite=TRUE )

print("Calculating median DOY - 1981-2016")
beginCluster(7)
  medianDOYsos <- clusterR(startOfSeason, overlay, args=list(fun=median))
endCluster()
writeRaster(medianDOYsos,filename="/scratch/crimmins/vip/processed/NDVI_VIP_medianSOS_1981_2016.grd", overwrite=TRUE )


# calc anomalies
beginCluster(7)
  medianDOYsos <- clusterR(startOfSeason, overlay, args=list(fun=median))
endCluster()
anom <- function(x, y) { 
  x - y 
} 
print("Calculating DOY anomalies")
anomDOYmean <- overlay(x = startOfSeason, y = meanDOYsos, fun = anom) # change base period to meanDOYsub if needed
anomDOYmedian <- overlay(x = startOfSeason, y = medianDOYsos, fun = anom) # change base period to meanDOYsub if needed

writeRaster(anomDOYmean,filename="/scratch/crimmins/vip/processed/NDVI_VIP_meanSOSanom_1981_2016.grd", overwrite=TRUE )
writeRaster(anomDOYmedian,filename="/scratch/crimmins/vip/processed/NDVI_VIP_medianSOSanom_1981_2016.grd", overwrite=TRUE )


# analyze VIP data
startOfSeason<-stack("/scratch/crimmins/vip/processed/NDVI_VIP_SOS_1981_2016.grd")
peakSeason<-stack("/scratch/crimmins/vip/processed/NDVI_VIP_peakSeason_1981_2016.grd")
numSeasons<-stack("/scratch/crimmins/vip/processed/NDVI_VIP_numSeasons_1981_2016.grd")
reliability<-stack("/scratch/crimmins/vip/processed/NDVI_VIP_reliability_1981_2016.grd")

names(startOfSeason)<-seq(1981, 2016, by=1)

# diff period of record
# calc anomalies
# calc anomalies
beginCluster(7)
  meanDOYsos <- clusterR(startOfSeason[[1:19]], overlay, args=list(fun=mean))
endCluster()

anom <- function(x, y) { 
  x - y 
} 
print("Calculating DOY anomalies")
anomDOYmean <- overlay(x = startOfSeason[[1:19]], y = meanDOYsos, fun = anom) # change base period to meanDOYsub if needed



# corLocal
corVIP<-corLocal(startOfSeason[[20:36]], peakSeason[[20:36]], method="kendall", test=TRUE)

plot(corVIP, 
     zlim=c(-1,1),
     breaks= seq(-1, 1, by = 0.1), 
     col=colorRampPalette(c("blue", "white", "red"))(length(seq(-1, 1, by = 0.1))-1),
     main="VIP Start of Season vs Peak Season Day - Pearson",
     legend.args=list(text='r', side=4, font=2, line=2.3))

y <- focal(corVIP, w=matrix(1, 3, 3), mean)

# plot results
library(rasterVis)
# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)
names(anomDOYmean)<-as.character(seq(2000, 2016, by=1))

my.at <- seq(-50, 50, 5)
#levelplot(gdd50, par.settings = RdBuTheme, at=my.at, main="GDD50-BaseT10")
levelplot(anomDOYmean, par.settings = RdBuTheme, at=my.at, main="Start of Season Anoms (81-16 mean)")
  #layer(sp.polygons(states))

# correlations
test<-corLocal(startOfSeason,peakSeason, method="kendall", test=TRUE)
plot(test[[1]], 
     zlim=c(-1,1),
     breaks= seq(-1, 1, by = 0.1), 
     col=colorRampPalette(c("blue", "white", "red"))(length(seq(-1, 1, by = 0.1))-1),
     main="VIP Start of Season vs Peak Season Day - Pearson",
     legend.args=list(text='r', side=4, font=2, line=2.3))
plot(states, add=TRUE)
