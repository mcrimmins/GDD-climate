# plot trends with cluster boundaries
# MAC 08/15/2019

library(raster)

load("./fixed/bothBaseT/detrendedBaseT10.RData")
cluster12poly <- readShapePoly("./clusterTrends/cluster12final_ZProd")

# stack of trends

allTrends<-stack(gdd50_x4Slope, gdd250_x4Slope,gdd450_x4Slope)
names(allTrends)<-c("GDD50","GDD250","GDD450")
my.at <- seq(-0.5, 0.5, 0.01)
levelplot(allTrends, par.settings = RdBuTheme, layout=c(1, 3),at=my.at, margin=FALSE, main="GDD BaseT10 Linear Trend, TopoWx 1948-2016")+
  layer(sp.polygons(cluster12poly, col = 'black', lwd=0.1))


# trends 
#load("./fixed/bothBaseT/detrendedBaseT10.RData")
my.at <- seq(-0.5, 0.5, 0.01)
levelplot(gdd50_x4Slope, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 BaseT10 Linear Trend, TopoWx 1948-2016")+
  layer(sp.polygons(cluster12poly))
my.at <- seq(-0.5, 0.5, 0.01)
levelplot(gdd450_x4Slope, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450x4 BaseT10 Linear Trend, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <- seq(0.0, 0.05, 0.001)
levelplot(gdd50_x4pval, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50x4 BaseT10 Linear Trend-pval, TopoWx 1948-2016")+
  layer(sp.polygons(states))
my.at <-  seq(0.0, 0.05, 0.001)
levelplot(gdd450_x4pval, par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD450x4 BaseT10 Linear Trend-pval, TopoWx 1948-2016")+
  layer(sp.polygons(states))
