# get TopoWx data with Geoknife
# 11/14/17 MAC

library(dplyr)
library(tidyr)
library(plyr)
library(geoknife)
library(ggplot2)
library(scales)
library(rgdal)
library(rgeos)
library(maptools)

# get cluster boundaries into GDP https://cida.usgs.gov/gdp/client/#!advanced/spatial
cluster12 <- readShapePoly("./mapFiles/cluster12poly")
#proj4string(cluster12) <- "+proj=longlat +datum=WGS84"
#writeOGR(cluster12, ".", "./mapFiles/cluster12rgdal", driver="ESRI Shapefile")

# cluster centroid
clusterCenter<-gCentroid(cluster12,byid=TRUE)
centers<-as.data.frame(t(clusterCenter@coords))
colnames(centers)<-paste0("cluster",cluster12@data$layer)
#location <- simplegeom(centers)
# multiple at a time?
location<-simplegeom(centers[,3])

# geoknife by shapefile polygons ----
# check geoms https://owi.usgs.gov/R/training-curriculum/usgs-packages/geoknife-data/
# setup a default stencil by using webgeom and not supplying any arguments
# stencilCluster <- webgeom()
# # now determine what geoms are available with the default
# geomsCluster <- query(stencilCluster, "geoms")
# # set geoms
# geom(stencilCluster) <- "upload:cluster12_B"
# # now that geom is set, you can query for available attributes
# query(stencilCluster, "attributes")
# attribute(stencilCluster)<-"SP_ID"
# # OPTIONAL set to single cluster boundary -----
# cluster_values<-query(stencilCluster,"values")
# # now set the values to the Driftless Area and Blue Ridge ecoregions
# values(stencilCluster) <- cluster_values[12] # select different regions 8-midatl, 2 SP_ID=SW, 12 SPI_ID=Up MW
# values(stencilCluster)
# ----          

# set algorithm ----
# setup a default knife by using webprocess and not supplying any arguments
# default_knife <- webprocess()
# # now determine what web processing algorithms are available with the default
# default_algorithms <- query(default_knife, 'algorithms')
# algorithm(default_knife) <- default_algorithms[4] # set to unweighted
# algorithm(default_knife)
# -----

# ---- from https://owi.usgs.gov/R/training-curriculum/usgs-packages/geoknife-exercises/
# First, you need to query for all web data
all_webdata <- query("webdata")

# Setup the maximum air temp fabric using the URL in all_webdata
airtemp_title <- "TopoWx: Topoclimatic Daily Air Temperature Dataset for the Conterminous United States"
airtemp_url <-  url(all_webdata[airtemp_title])
airtemp_fabric <- webdata(list(
  url = airtemp_url,
  variables = c("tmax","tmin"),
  times = as.POSIXct(c("1948-01-01 12:01", "2016-12-31 11:59"), tz = "UTC")
))

# Now setup the stencil; Points can be specified as longitude latitude pairs
# NPN site 43.08535, -70.69133 NaNs, try 43.093317, -70.689563
# NPN TN 36.440838, -84.323586
# MN near Fargo 47.102994, -96.687827
# FL near Orlando 29.233094, -81.904940
#coords<-c(-81.904940, 29.233094)
#location <- simplegeom(coords)

# Leave the default knife since we want an average over the stencil
# Execute the geoknife job
centroids_job_1244 <- geoknife(stencil = location, fabric = airtemp_fabric, wait=FALSE, email="crimmins@email.arizona.edu")
centroids_job_156 <- geoknife(stencil = location, fabric = airtemp_fabric, wait=TRUE)


# cluster12 job
#airtemp_job_upMW <- geoknife(stencil = stencilCluster, fabric = airtemp_fabric, wait=FALSE, email="crimmins@email.arizona.edu")

# check job
#check(airtemp_job_upMW)
check(centroids_job_156)



# Download the data
tempData <- result(centroids_job_156)
# save downloaded data?
#save.image("~/RProjects/TopoWx/TopoWxTS_FL.RData")
save(tempData, file="~/RProjects/TopoWx/ts/TopoWXts_MIDATL.RData")

#---- load existing data?
#load("~/RProjects/TopoWx/ts/TopoWxTS_FL.RData")

# change column name to match
colnames(tempData)[2]<-"bufferedPoint"

# reshape and get tmean
tempDataWide <- spread(tempData, variable, bufferedPoint)
tempDataWide$tmean<-(tempDataWide$tmax+tempDataWide$tmin)/2
# add date vars
tempDataWide$month<-as.numeric(format(tempDataWide$DateTime, "%m"))
tempDataWide$year<-as.numeric(format(tempDataWide$DateTime, "%Y"))
tempDataWide$day<-as.numeric(format(tempDataWide$DateTime, "%d"))
tempDataWide$doy<-strptime(tempDataWide$DateTime, "%Y-%m-%d")$yday+1  
#tempDataWide<-filter(tempDataWide, month!=2 | day!=29)
#tempDataWide<-filter(tempDataWide, month!=12 | day!=31)


# calc GDDs
baseT<-10
tempDataWide$tmean[tempDataWide$tmean < baseT] <- 0
cumGDD <- tempDataWide %>% 
  dplyr::group_by(year, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
  dplyr::summarise(value = sum(tmean)) %>%
  dplyr::mutate(csum = cumsum(value))
tempDataWide$GDD<-cumGDD$csum

# CHOOSE Yr to plot
plotYr<-2012
Year<-paste0(plotYr)
currYear<-filter(tempDataWide, year==plotYr) 

# get doy stats
# calculate stats by day
dayQuant<- ddply(tempDataWide,.(doy),summarise,
                 q05 = quantile(GDD,0.05,na.rm='TRUE'),
                 #q25 = quantile(GDD,0.25,na.rm='TRUE'),
                 #q33 = quantile(GDD,0.33,na.rm='TRUE'),
                 q50 = quantile(GDD,0.50,na.rm='TRUE'),
                 #q66 = quantile(GDD,0.66,na.rm='TRUE'),
                 #q75 = quantile(GDD,0.75,na.rm='TRUE'),
                 q95 = quantile(GDD,0.95,na.rm='TRUE'),
                 #min = min(GDD,na.rm='TRUE'),
                 #max = max(GDD,na.rm='TRUE'),
                 avg = mean(GDD,na.rm='TRUE'))
dayQuant$date<-as.POSIXct(strptime(paste(plotYr, dayQuant$doy), format="%Y %j"))
gathQuant<-gather(dayQuant, "stat","values", 2:5)

# plot GDD curves
p<-ggplot(gathQuant,aes(date,values))+
  geom_line(aes(linetype=stat), size=0.1)+
  theme_bw()
p1<-p + geom_line(data=currYear,aes(DateTime,GDD, colour=Year), size=1)+
scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c(paste0(plotYr,"-01-01"),paste0(plotYr,"-07-01"))))+
  labs(x='calendar day', y='GDD(base10C)',title=paste0('Cumulative Growing Degree Days (',coords[1],",",coords[2],")"))+
  geom_hline(yintercept=50)+
  geom_hline(yintercept=500)+
  ylim(0,1000)


# Multi year plots
# CHOOSE Yr to plot
currYear<-filter(tempDataWide, between(year,1948,2006)) 
currYear$altDate<-as.POSIXct(strptime(paste(2016,currYear$month,currYear$day), format="%Y %m %d"))
# plot GDD curves
p<-ggplot(gathQuant,aes(date,values))+
  geom_line(aes(linetype=stat), size=0.1)+
  theme_bw()
p + geom_line(data=currYear,aes(altDate,GDD,color=as.factor(currYear$year)), size=1)+
  scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c("2016-01-01","2016-07-01")))+
  labs(x='calendar day', y='GDD(base10C)',title=paste0('Cumulative Growing Degree Days (',coords[1],",",coords[2],")"))+
  geom_hline(yintercept=50)+
  geom_hline(yintercept=500)+
  ylim(0,1000)


# ---- Daily temp time series
# CHOOSE Yr to plot
tempsData<-tempDataWide
tempsData$tmean<-(tempsData$tmax+tempsData$tmin)/2
plotYr<-2012
Year<-paste0(plotYr)
currYear<-filter(tempsData, year==plotYr) 

# get doy stats
# calculate stats by day
dayTemp<- ddply(tempsData,.(doy),summarise,
                 #q05 = quantile(GDD,0.05,na.rm='TRUE'),
                 #q25 = quantile(GDD,0.25,na.rm='TRUE'),
                 #q33 = quantile(GDD,0.33,na.rm='TRUE'),
                 #q50 = quantile(GDD,0.50,na.rm='TRUE'),
                 #q66 = quantile(GDD,0.66,na.rm='TRUE'),
                 #q75 = quantile(GDD,0.75,na.rm='TRUE'),
                 #q95 = quantile(GDD,0.95,na.rm='TRUE'),
                 min = min(tmean,na.rm='TRUE'),
                 max = max(tmean,na.rm='TRUE'),
                 avg = mean(tmean,na.rm='TRUE'))
dayTemp$date<-as.POSIXct(strptime(paste(plotYr, dayTemp$doy), format="%Y %j"))
currYear<-merge(currYear,dayTemp,by.x="doy",by.y="doy")
currYear$tmeanAnom<-currYear$tmean-currYear$avg
  currYear$pos <- currYear$tmeanAnom >= 0

gathTemp<-gather(dayTemp, "stat","values", 2:4)

# plot temp series
p<-ggplot(gathTemp,aes(date,values))+
  geom_line(aes(linetype=stat), size=0.1)+
  theme_bw()
p2<-p + geom_line(data=currYear,aes(DateTime,tmean, colour=Year), size=1)+
  scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c(paste0(plotYr,"-01-01"),paste0(plotYr,"-07-01"))))+
  labs(x='calendar day', y='deg C',title=paste0('Daily Avg Temp (C) (',coords[1],",",coords[2],")"))+
  geom_hline(yintercept=10)

# plot anomalies
ggplot(currYear, aes(x=DateTime, y=tmeanAnom, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)+
  scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c(paste0(plotYr,"-01-01"),paste0(plotYr,"-07-01"))))+
  labs(x='calendar day', y='deg C',title=paste0(plotYr,' Daily Avg Temp (C) (',coords[1],",",coords[2],")"))+
  theme_bw()

# plot temps and gdd
multiplot(p1, p2, cols=1)




# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

