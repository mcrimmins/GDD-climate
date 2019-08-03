# write out study data to netcdfs for data dryad
# MAC 07/28/19

library(raster)
library(ncdf4)

# new grids
gdd50_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh50.grd")
gdd250_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh250.grd")
gdd450_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh450.grd")
gdd50_x4_0<-stack("./fixed/X4_rawDOY_baseT0_thresh50.grd")
gdd250_x4_0<-stack("./fixed/X4_rawDOY_baseT0_thresh250.grd")
gdd450_x4_0<-stack("./fixed/X4_rawDOY_baseT0_thresh450.grd")

# add names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50_DOY_baseT10")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250_DOY_baseT10")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450_DOY_baseT10")
names(gdd50_x4_0)<-paste0(seq(1948, 2016, by=1),".GDD50_DOY_baseT0")
names(gdd250_x4_0)<-paste0(seq(1948, 2016, by=1),".GDD250_DOY_baseT0")
names(gdd450_x4_0)<-paste0(seq(1948, 2016, by=1),".GDD450_DOY_baseT0")


# write to netcdf
writeRaster(gdd50_x4, "./figs/dataRepository/GDD50_DOY_BaseT10", "CDF", overwrite=TRUE,
            varname="Day of Year at 50 GDDs-Base T10C",varunit="DOY",longname="Day of Year")

writeRaster(gdd250_x4, "./figs/dataRepository/GDD250_DOY_BaseT10", "CDF", overwrite=TRUE,
            varname="Day of Year at 250 GDDs-Base T10C",varunit="DOY",longname="Day of Year")

writeRaster(gdd450_x4, "./figs/dataRepository/GDD450_DOY_BaseT10", "CDF", overwrite=TRUE,
            varname="Day of Year at 450 GDDs-Base T10C",varunit="DOY",longname="Day of Year")

writeRaster(gdd50_x4_0, "./figs/dataRepository/GDD50_DOY_BaseT0", "CDF", overwrite=TRUE,
            varname="Day of Year at 50 GDDs-Base T0C",varunit="DOY",longname="Day of Year")

writeRaster(gdd250_x4_0, "./figs/dataRepository/GDD250_DOY_BaseT0", "CDF", overwrite=TRUE,
            varname="Day of Year at 250 GDDs-Base T0C",varunit="DOY",longname="Day of Year")

writeRaster(gdd450_x4_0, "./figs/dataRepository/GDD450_DOY_BaseT0", "CDF", overwrite=TRUE,
            varname="Day of Year at 450 GDDs-Base T0C",varunit="DOY",longname="Day of Year")


# check
#test<-stack("./figs/dataRepository/GDD50_DOY_BaseT10.nc")
# fname<-"./figs/dataRepository/GDD50_DOY_BaseT10.nc"
# # open a netCDF file
# ncin <- nc_open(fname)
# print(ncin)
