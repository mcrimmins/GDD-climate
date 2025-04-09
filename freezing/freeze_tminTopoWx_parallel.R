# calculate avg first freeze date from daily tmin TopoWx data
# MAC 05/15/20

#Load necessary packages
library(raster)
library(ncdf4)
library(parallel)
library(rgdal)

# set rasteroptions
rasterOptions(progress = 'text')

# first freeze day function
frzdoy<- function(x){
  doy<-min(which(x <= 0))
  if(length(doy)==0){
    doy<-NA
  }else{
    doy<-doy
  }
  return(doy)
}

# County as LMUs ----
us<-getData('GADM', country='USA', level=2)
counties<-subset(us, NAME_1=="Arizona" | NAME_1=="New Mexico")
county<-subset(counties, NAME_2=="Pima")
# CNF Districts
CNF<-readOGR(dsn = "./freezing/shapes", layer = "CNF_Districts")
CNF<-spTransform(CNF, crs(county))

# load data for each year
#Set years
yr1<-1980
yr2<-2010

firstFreeze <- stack()

for(i in yr1:yr2){
  paste0(i)
  topowx.min <- paste0("/scratch/crimmins/topowx/download.scrim.psu.edu/TopoWx/release_2017-07/daily/tmin/tmin_",i,".nc")
  #"load" data into R via Raster package
  tmin <- brick(topowx.min, varname="tmin",  ncdf=TRUE)
  tmin <- crop(tmin, CNF)
  tmin <- tmin[[214:nlayers(tmin)]]
  
  # parallell calc
  #ptm <- proc.time()
    beginCluster(7)
      tempGrid <- clusterR(tmin, calc, args=list(fun=frzdoy))
    endCluster()
  #proc.time() - ptm
    tempGrid<-tempGrid+214
  # set NA values to end of year
    tempGrid[is.na(tempGrid)] <- 365
  
  firstFreeze <- stack(firstFreeze, tempGrid)
  print(i)
  
}


# set names of layers to years
names(firstFreeze)<-as.character(seq(1980, 2010, by=1))

# write out tempGrid2 stack -- large file and may not be necessary
writeRaster(firstFreeze,filename="./freezing/CNF_firstFreeze_DOY_1980_2010_topoWX.grd", overwrite=TRUE )
  
# ---- stack calculations  
# calculate mean DOY
beginCluster(6)
  meanDOY <- clusterR(firstFreeze, overlay, args=list(fun=mean,na.rm=T))
endCluster()
writeRaster(meanDOY,filename="./freezing/CNF_MEAN_firstFreeze_DOY_1980_2010_topoWX.grd", overwrite=TRUE )

# write out meanDOY to ESRI ascii
temp<-raster("./freezing/CNF_MEAN_firstFreeze_DOY_1980_2010_topoWX.grd")
#temp <- projectRaster(temp, crs=crs(CNF))

writeRaster(temp,filename="./freezing/CNF_MEAN_firstFreeze_DOY_1980_2010_topoWX.asc", overwrite=TRUE )

# # min
# beginCluster(6)
#   minDOY <- clusterR(firstFreeze, overlay, args=list(fun=min,na.rm=T))
# endCluster()
# 
# # max
# beginCluster(6)
#   maxDOY <- clusterR(firstFreeze, overlay, args=list(fun=max,na.rm=T))
# endCluster()

 # beginCluster(6)
 #   medianDOY <- clusterR(firstFreeze, overlay, args=list(fun=median,na.rm=T))
 # endCluster()

# set INF to NA
#meanDOY[meanDOY ==Inf] <- NA
meanDOY<-round(meanDOY, 0)

# make Leaflet Maps
library(leaflet)
library(leafem)
library(htmlwidgets)
pal=colorNumeric(c("blue","green","yellow", "orange"), values(meanDOY),
                 na.color = "transparent")
leafMap<-leaflet() %>% addTiles() %>%
  addRasterImage(meanDOY, colors = pal, maxBytes=20000000,opacity = 0.8, layerId = "avg-DOY") %>%
  addMouseCoordinates() %>%
  addImageQuery(meanDOY, type="mousemove", layerId = "avg-DOY", digits = 0, prefix = "") %>%
  addLegend(pal = pal, values = values(meanDOY),
            title="Average First Freeze (Day of Year)")%>%
  addPolygons(data=CNF, color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.01,
              highlightOptions = highlightOptions(color = "red", weight = 2,
                                                  bringToFront = TRUE),
              label=as.character(CNF$FORESTNAME),
              labelOptions = labelOptions(direction = "auto"))
saveWidget(leafMap, file="/home/crimmins/RProjects/TopoWx/freezing/CNF_Average_First_Freeze_1980-2010.html", selfcontained = TRUE)

