# assess GDD500 at higher elevations
# develop mask to NA these areas? 
# use elevation
# 12/23/17 MAC

gdd50raw_x4<-stack("X4_rawDOY_baseT10_thresh50.grd")
gdd500raw_x4<-stack("X4_rawDOY_baseT10_thresh500.grd")

#idx = which.min(gdd500raw_x4)

# find 0's in GDD50
gdd50raw_x4<- reclassify(gdd50raw_x4, c(-999,0,1,  1,365,0))
gdd50raw_x4<-as.factor(gdd50raw_x4)
sum0GDD50<- sum(gdd50raw_x4)

# find 0's in GDD500
gdd500raw_x4<- reclassify(gdd500raw_x4, c(-999,0,1,  1,365,0))
gdd500raw_x4<-as.factor(gdd500raw_x4)
sum0GDD500<- sum(gdd500raw_x4)

# id all areas that have at least one year with a 0 GDD50 or GDD500 DOY
sum0GDDall<-sum0GDD50+sum0GDD500
# set to NA to create mask
sum0GDDall[sum0GDDall > 0] <- NA
# plot(sum0GDDall) # check

writeRaster(sum0GDDall,filename="maskNA.grd", overwrite=TRUE)

