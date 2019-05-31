# plot gdd trends
# MAC 12/20/2018

library(raster)
library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')

# load data
load("./fixed/bothBaseT/detrendedBaseT0.RData")
  rm(gdd50_x4Resid,gdd250_x4Resid,gdd450_x4Resid)
load("./fixed/bothBaseT/detrendedBaseT10.RData")
  rm(gdd50_x4Resid_0,gdd250_x4Resid_0,gdd450_x4Resid_0)
# load mask
maskNA<-raster("./fixed/maskNAalt.grd")  
maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd")

# base 10 trends  
base10trends<-mask(stack(gdd50_x4Slope,gdd250_x4Slope,gdd450_x4Slope),maskNA)*10 
base10pvals<-mask(stack(gdd50_x4pval,gdd250_x4pval,gdd450_x4pval),maskNA)
  names(base10trends)<-c("GDD50_t10","GDD250_t10","GDD450_t10")
  #plot(base10trends, zlim=c(-10,5))
  trend.at <- seq(-2, 2, 0.1)
  pval.at <- seq(0.00, 0.95, 0.5)
  p0 <- levelplot(base10trends, par.settings = BuRdTheme, ylab=NULL, xlab=NULL, 
                  sub=list(label="             Trends",cex=0.75,font = 1),
                  at=trend.at, 
                  layout=c(3,1),  scales=list(alternating=3, cex=0.5), #scales=list(draw=FALSE)
                  colorkey=list(space="bottom", width=0.8, height=1, cex=0.75)) # width=1, height=0.5, row=3, column=1, 
  p0 + levelplot(base10pvals, par.settings =GrTheme, at=pval.at,layout=c(3,1),alpha.regions=0.3)
  
  
# trend diffs
base10trendDiff<-mask((gdd50_x4Slope-gdd450_x4Slope),maskNA)*10 
col.at <- seq(-5, 5, 0.5)
  levelplot(base10trendDiff, par.settings =BuRdTheme, at=col.at, margin=FALSE,
            main="GDD50-450 BaseT10 Trend Differences")
  
# base 0 trends  
base0trends<-mask(stack(gdd50_x4Slope_0,gdd250_x4Slope_0,gdd450_x4Slope_0),maskNA_0)*10 
  names(base0trends)<-c("GDD50_t0","GDD250_t0","GDD450_t0")
  plot(base0trends, zlim=c(-10,5)) 
# trend diffs
base0trendDiff<-mask((gdd50_x4Slope_0-gdd450_x4Slope_0),maskNA_0)*10 
col.at <- seq(-5, 5, 0.5)
  levelplot(base0trendDiff, par.settings =BuRdTheme, at=col.at, margin=FALSE,
            main="GDD50-450 BaseT0 Trend Differences")  
  