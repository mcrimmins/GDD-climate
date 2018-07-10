# get TopoWx data with Geoknife
# 11/14/17 MAC

library(dplyr)
library(tidyr)
library(plyr)
library(geoknife)
library(ggplot2)
library(scales)
library(maptools)
library(rgeos)

# save classMap
load("./fixed/cluster15classMap.RData")

# Download States boundaries (might take time)
#states <- getData('GADM', country='United States', level=1)

# create polygon shapefile
# library(rgdal)
#  clusterpoly<-rasterToPolygons(classMap, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
#  proj4string(clusterpoly) <- "+proj=longlat +datum=WGS84"
#  writeOGR(clusterpoly, ".", "./mapFiles/cluster15finalFIXED_ZProd", driver="ESRI Shapefile")

# load in cluster shapefile
clusterpoly <- readShapePoly("./mapFiles/cluster15finalFIXED_ZProd")

# get cluster centroids
clusterCenter<-gCentroid(clusterpoly,byid=TRUE)
centers<-as.data.frame(t(clusterCenter@coords))
# nudge Northeast point into US
centers[1,2]<-centers[1,2]+1# nudge
centers[2,2]<-centers[2,2]+-1# nudge

colnames(centers)<-paste0("cluster",clusterpoly@data$layer)

# # plot map as check
# library(rasterVis)
spdf = SpatialPointsDataFrame(clusterCenter, as.data.frame(clusterpoly@data$layer))
levelplot(classMap, par.settings = RdBuTheme)+
  layer(sp.points(spdf))+
  layer(sp.polygons(clusterpoly))+
  layer(sp.text(coordinates(spdf), txt = spdf@data$`clusterpoly@data$layer`, pos = 1))

# Get topowx data
all_webdata <- query("webdata")

# Setup the maximum air temp fabric using the URL in all_webdata
airtemp_title <- "TopoWx: Topoclimatic Daily Air Temperature Dataset for the Conterminous United States"
airtemp_url <-  url(all_webdata[airtemp_title])
airtemp_fabric <- webdata(list(
  url = airtemp_url,
  variables = c("tmax","tmin"),
  times = as.POSIXct(c("1948-01-01 12:01", "2016-12-31 11:59"), tz = "UTC")
))

# loop through submitting job with each cluster centroid, bind to data file

# Execute the geoknife job
for(i in 1:15){
  # form and submit geoknife job
  location<-simplegeom(centers[,i])
  clusterCenter_job<- geoknife(stencil = location, fabric = airtemp_fabric, wait=TRUE)
  tempData <- result(clusterCenter_job)
  colnames(tempData)[2]<-colnames(centers)[i]
  
  # store tempData 
  if (i==1){
    clusterData <- tempData
  }else{
    clusterData <- cbind(clusterData, tempData[,2]) 
    colnames(clusterData)[i+3]<-colnames(tempData)[2]
  }
  print(paste0("Cluster center iteration # ",i))
}

save(clusterData,centers, file="~/RProjects/TopoWx/ts/TopoWXts_15centers.RData")

#---- load existing data?
load("~/RProjects/TopoWx/ts/TopoWXts_15centers.RData")
clusterN=15

# gather to collapse cluster data cols
tempData<-gather(clusterData, key=statistic, value=paste0("cluster", seq(1,15,1)), -DateTime, -variable)

# change column name to match
colnames(tempData)[3:4]<-c("clusterName","bufferedPoint")
tempData$bufferedPoint<-as.numeric(tempData$bufferedPoint)

# drop the empty 'statistic' rows
tempData <-tempData[ which(tempData$clusterName!='statistic'),]

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
tempDataWide$tmean <- unlist(lapply(tempDataWide$tmean, function(x) ifelse(x>=baseT, x-baseT, 0)))
# need to substract base temp...

cumGDD <- tempDataWide %>% 
  dplyr::group_by(year,clusterName) %>% # still doesn't quite work adjYear kicks off before adjDOY
  #dplyr::summarise(value = sum(tmean)) %>%
  #dplyr::mutate(csum = cumsum(value))
  dplyr::mutate(csum = cumsum(tmean))
tempDataWide$GDD<-cumGDD$csum

# add in factor names
nameLUP<-as.data.frame(cbind(paste0("cluster",as.character(seq(1, clusterN, by=1))),c("TN Valley","N Plains","N Rockies","Pac NW","OH Valley","Florida",
                                                                                      "Texas","S Plains","Gulf Coast","Northeast","Midwest","C Plains",
                                                                                      "Southwest","S Rockies","Southeast")))
# lookup table to add in names
new<-tempDataWide
new[] <- nameLUP$V2[match(unlist(tempDataWide), nameLUP$V1)]
tempDataWide$clusterName<-new$clusterName
# factor order 
tempDataWide$clusterName<-factor(tempDataWide$clusterName, levels=c("Southwest","S Rockies","C Plains","N Rockies","Pac NW","Texas",
                                                                    "TN Valley","S Plains","Midwest","N Plains","Florida","Gulf Coast","Southeast","OH Valley","Northeast"))


# CHOOSE Yr to plot
plotYr<-1993
Year<-paste0(plotYr)
currYear<-filter(tempDataWide, year==plotYr) 

# get doy stats
# calculate stats by day
dayQuant<- ddply(tempDataWide,.(doy, clusterName),summarise,
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
gathQuant<-gather(dayQuant, "stat","values", 3:6)


# plot GDD curves
p<-ggplot(gathQuant,aes(date,values))+
  geom_line(aes(linetype=stat), size=0.5)+
  facet_wrap(~clusterName, nrow = 3)+
  theme_bw()
p1<-p + geom_line(data=currYear,aes(DateTime,GDD, colour=Year), size=1)+
scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c(paste0(plotYr,"-01-01"),paste0(plotYr,"-09-01"))))+
  labs(x='calendar day', y='GDD(base10C)',title=paste0('Cumulative Growing Degree Days'))+
  geom_hline(yintercept=50)+
  geom_hline(yintercept=250)+
  geom_hline(yintercept=450)+
  ylim(0,500)

# anomaly of cumulative GDD ----
avgGDD<-dayQuant[,c(1,2,6)]
gddAnom<-merge(tempDataWide,avgGDD, by.x=c("clusterName","doy"),by.y=c("clusterName","doy"), all.x=TRUE)
gddAnom$avgAnom<-gddAnom$GDD-gddAnom$avg
  # Multi year plots
  # CHOOSE Yr to plot
  currYear<-filter(gddAnom, between(year,2000,2016)) 
  currYear$altDate<-as.POSIXct(strptime(paste(2012,currYear$month,currYear$day), format="%Y %m %d"))
  # plot GDD curves
  ggplot(data=currYear,aes(altDate,avgAnom))+  #color=as.factor(currYear$year)
  geom_point(size=0.1, stat = "identity")+
    facet_wrap(~clusterName, nrow = 3)+
    scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c("2012-01-01","2012-09-01")))+
    geom_hline(yintercept=0)+
    theme_bw()





# Multi year plots
# CHOOSE Yr to plot
currYear<-filter(tempDataWide, between(year,1948,2016)) 
currYear$altDate<-as.POSIXct(strptime(paste(2012,currYear$month,currYear$day), format="%Y %m %d"))
# plot GDD curves
p<-ggplot(gathQuant,aes(date,values))+
  geom_line(aes(linetype=stat), size=0.1)+
  facet_wrap(~clusterName, nrow = 3)+
  theme_bw()
p + geom_path(data=currYear,aes(altDate,GDD,color=as.factor(currYear$year)), size=0.1, stat = "identity")+
  scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c("2012-01-01","2012-09-01")))+
  labs(x='calendar day', y='GDD(base10C)',title="Cumulative Growing Degree Days")+
  geom_hline(yintercept=50)+
  geom_hline(yintercept=250)+
  geom_hline(yintercept=450)+
  ylim(0,2000)

# ggplot(currYear,aes(altDate,GDD))+
# geom_path(size=0.05, stat = "identity")+
#   facet_wrap(~clusterName, nrow = 2)+
#   scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c("2012-01-01","2012-09-01")))+
#   labs(x='calendar day', y='GDD(base10C)',title="Cumulative Growing Degree Days")+
#   geom_hline(yintercept=50)+
#   geom_hline(yintercept=250)+
#   geom_hline(yintercept=450)+
#   ylim(0,2000)+
#   theme_bw()


# ---- Daily temp time series
# CHOOSE Yr to plot
tempsData<-tempDataWide
tempsData$tmean<-(tempsData$tmax+tempsData$tmin)/2
plotYr<-1993
Year<-paste0(plotYr)
currYear<-filter(tempsData, year==plotYr) 

# get doy stats
# calculate stats by day
dayTemp<- ddply(tempsData,.(doy, clusterName),summarise,
                 #q05 = quantile(GDD,0.05,na.rm='TRUE'),
                 q25 = quantile(tmean,0.25,na.rm='TRUE'),
                 #q33 = quantile(GDD,0.33,na.rm='TRUE'),
                 #q50 = quantile(GDD,0.50,na.rm='TRUE'),
                 #q66 = quantile(GDD,0.66,na.rm='TRUE'),
                 q75 = quantile(tmean,0.75,na.rm='TRUE'),
                 #q95 = quantile(GDD,0.95,na.rm='TRUE'),
                 min = min(tmean,na.rm='TRUE'),
                 max = max(tmean,na.rm='TRUE'),
                 avg = mean(tmean,na.rm='TRUE'))
dayTemp$date<-as.POSIXct(strptime(paste(plotYr, dayTemp$doy), format="%Y %j"))
currYear<-merge(currYear,dayTemp,by.x=c("doy","clusterName"),by.y=c("doy","clusterName"))
currYear$tmeanAnom<-currYear$tmean-currYear$avg
  currYear$pos <- currYear$tmeanAnom >= 0

gathTemp<-gather(dayTemp, "stat","values", 3:7)


# plot temp series
p<-ggplot(gathTemp,aes(date,values))+
  geom_line(aes(linetype=stat), size=0.1)+
  facet_wrap(~clusterName, nrow = 3)+
  theme_bw()
p2<-p + geom_line(data=currYear,aes(DateTime,tmean, colour=Year), size=0.5)+
  scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c(paste0(plotYr,"-01-01"),paste0(plotYr,"-07-01"))))+
  labs(x='calendar day', y='deg C',title='Daily Avg Temp (C)')+
  geom_hline(yintercept=10)+
  ylim(10,30)
  #geom_hline(yintercept=5)+
 # geom_hline(yintercept=0)
  

# scratch code to get at crossing over points at 10C
currYear2 <- currYear[ which(currYear$clusterName=='TN Valley'), ]
# is day abv/below 10C
currYear2$tmeanAnom<-currYear2$tmean-10
updn <- c(0, diff(sign(currYear2$tmeanAnom)))
ix <- which(updn != 0)
crossovers<-length(ix)
  

# plot anomalies
ggplot(currYear, aes(x=DateTime, y=tmeanAnom, fill=pos)) +
  geom_bar(stat="identity", position="identity", size=0.5) +
  scale_fill_manual(values=c("#27b2f7", "#f72727"), guide=FALSE)+
  scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c(paste0(plotYr,"-01-01"),paste0(plotYr,"-07-01"))))+
  labs(x='calendar day', y='deg C',title=paste0('Daily Avg Temp Anom (C) - ',plotYr))+
  facet_wrap(~clusterName, nrow = 2)+
  theme_bw()

# plot temps and gdd
multiplot(p1, p2, cols=1)


# differencing
library(xts)
diffGDD<-tempDataWide[which(tempDataWide$month!=2 | tempDataWide$day!=29),]

diffGDD <- diffGDD %>% 
  dplyr::group_by(year,clusterName) %>% # still doesn't quite work adjYear kicks off before adjDOY
  #dplyr::summarise(value = sum(tmean)) %>%
  #dplyr::mutate(csum = cumsum(value))
  #dplyr::mutate(csum = cumsum(tmean))
  #dplyr::mutate(diffcsum = pad(diff((diffGDD$GDD), lag=1), length((diffGDD$GDD)))) 
  #dplyr::mutate(diffcsum = diff(GDD, lag=1)) 
  dplyr::mutate(diffcsum = as.vector(diff(zoo(GDD), na.pad=TRUE))) 
detach("package:xts", unload=TRUE)
detach("package:zoo", unload=TRUE)

# CHOOSE Yr to plot
plotYr<-2005
Year<-paste0(plotYr)
currYear<-filter(diffGDD, year==plotYr) 

# get doy stats
# calculate stats by day
dayQuant<- ddply(diffGDD,.(doy, clusterName),summarise,
                 q05 = quantile(diffcsum,0.05,na.rm='TRUE'),
                 #q25 = quantile(GDD,0.25,na.rm='TRUE'),
                 #q33 = quantile(GDD,0.33,na.rm='TRUE'),
                 q50 = quantile(diffcsum,0.50,na.rm='TRUE'),
                 #q66 = quantile(GDD,0.66,na.rm='TRUE'),
                 #q75 = quantile(GDD,0.75,na.rm='TRUE'),
                 q95 = quantile(diffcsum,0.95,na.rm='TRUE'),
                 #min = min(GDD,na.rm='TRUE'),
                 #max = max(GDD,na.rm='TRUE'),
                 avg = mean(diffcsum,na.rm='TRUE'))
dayQuant$date<-as.POSIXct(strptime(paste(plotYr, dayQuant$doy), format="%Y %j"))
gathQuant<-gather(dayQuant, "stat","values", 6) # or 3:6


# plot GDD curves
p<-ggplot(gathQuant,aes(date,values))+
  geom_line(aes(linetype=stat), size=0.5)+
  facet_wrap(~clusterName, nrow = 2)+
  theme_bw()
p1<-p + geom_line(data=currYear,aes(DateTime,diffcsum, colour=Year), size=1)+
  scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c(paste0(plotYr,"-01-01"),paste0(plotYr,"-09-01"))))+
  labs(x='calendar day', y='GDD(base10C)/day',title=paste0('Rate of GDD Accumulation/day'))
#  geom_hline(yintercept=50)+
#  geom_hline(yintercept=250)+
#  geom_hline(yintercept=450)+
#  ylim(0,2000)

p<-ggplot(gathQuant,aes(date,values))+
  geom_line(aes(linetype=stat), size=0.5)+
  facet_wrap(~clusterName, nrow = 2)+
  theme_bw()


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


# plot of point
test<-t(centers)
text(test[,1], test[,2], rownames(test))