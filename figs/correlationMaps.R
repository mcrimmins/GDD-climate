# Correlation Figure
# MAC 06/22/2018


library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)

# map layers
states <- getData('GADM', country='United States', level=1)

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')


# correlations
load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")
# plot all corrs in one stack
allCorr<-stack(corRasterDet_BaseT0[[1]], corRasterDet_BaseT10[[1]],
               corRasterDet_BaseT0_250v50[[1]], corRasterDet_BaseT10_250v50[[1]],
               corRasterDet_BaseT0_450v250[[1]], corRasterDet_BaseT10_450v250[[1]])
names(allCorr)<-c("50v450 BaseT0", "50v450 BaseT10",
                  "50v250 BaseT0", "50v250 BaseT10",
                  "250v450 BaseT0", "250v450 BaseT10")
my.at <- seq(0, 1, 0.1)
corrFig<-levelplot(allCorr, layout=c(2,3), par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Pearson-Corr (TopoWx 48-16)")+
  layer(sp.polygons(states))
# histFig<-histogram(allCorr, layout=c(2,3))
# print(corrFig, split=c(1,1,1,2), more=TRUE)
# print(histFig, split=c(1,2,1,2))


# 50 v 450 
my.at <- seq(0, 1, 0.1)
levelplot(corRaster_BaseT0[[1]], par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT0[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT10[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT10[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
# 50 v 250 
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT0_250v50[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD250x4 BaseT0 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT0_250v50[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD250x4 BaseT0 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT10_250v50[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD250x4 BaseT10 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT10_250v50[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD250x4 BaseT10 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
# 450 v 250 
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT0_450v250[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD450x4/GDD250x4 BaseT0 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT0_450v250[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD450x4/GDD250x4 BaseT0 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRaster_BaseT10_450v250[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD450x4/GDD250x4 BaseT10 Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-1, 1, 0.1)
levelplot(corRasterDet_BaseT10_450v250[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD450x4/GDD250x4 BaseT10 Detrended Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
# pvals
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster_BaseT0[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Pearson-Corr Pval (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRasterDet_BaseT0[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Detrended Pearson-Corr Pval (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRaster_BaseT10[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Pearson-Corr Pval (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(0, 0.1, 0.01)
levelplot(corRasterDet_BaseT10[[2]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Detrended Pearson-Corr Pval (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
# correlation diffs
my.at <- seq(-0.2, 0.2, 0.01)
levelplot(corRaster_BaseT10[[1]]-corRasterDet_BaseT10[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Obs-Det Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
my.at <- seq(-0.2, 0.2, 0.01)
levelplot(corRaster_BaseT0[[1]]-corRasterDet_BaseT0[[1]], par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT0 Obs-Det Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))

# compare raw/detrended corrs
# library(psych)
# pairedCorrs_baseT10 <- overlay(corRaster_BaseT10[[1]],corRasterDet_BaseT10[[1]], fun=function(x,y){ m=paired.r(x,y,NULL, 69); m[[3]]} )
# pairedCorrs_baseT0 <- overlay(corRaster_BaseT0[[1]],corRasterDet_BaseT0[[1]], fun=function(x,y){ m=paired.r(x,y,NULL, 69); m[[3]]} )
# my.at <- seq(0, 0.2, 0.01)
# levelplot(pairedCorrs_baseT10, par.settings = BuRdTheme, at=my.at, margin=FALSE, main="Fisher Z pval BaseT10 Obs-Det Pearson-Corr (TopoWx 48-16)")+ 
#   layer(sp.polygons(states))
# my.at <- seq(0, 0.2, 0.01)
# levelplot(pairedCorrs_baseT0, par.settings = BuRdTheme, at=my.at, margin=FALSE, main="Fisher Z pval BaseT0 Obs-Det Pearson-Corr (TopoWx 48-16)")+ 
#   layer(sp.polygons(states))


# corr map with pvalue overlay

# create polygon shapefile
 library(rgdal)
  pval<-rasterToPolygons(corRasterDet_BaseT10[[2]]>0.05, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
  proj4string(pval) <- "+proj=longlat +datum=WGS84"
  pval <- pval[pval$layer == 1,] 
#  writeOGR(clusterpoly, ".", "./mapFiles/cluster15finalFIXED_ZProd", driver="ESRI Shapefile")
  my.at <- seq(0, 1, 0.05)
  fig1<-levelplot(corRasterDet_BaseT10[[1]], par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD450x4 BaseT10 Detrended Pearson-Corr (TopoWx 48-16)")+ 
    layer(sp.polygons(states))+
    layer(sp.polygons(pval, fill = "grey", col="black", alpha=0.5))
  
# trying out multiple plots
 fig2<- levelplot(corRasterDet_BaseT10_450v250[[1]], par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="GDD250x4/GDD450x4 BaseT10 Detrended Pearson-Corr (TopoWx 48-16)")+ 
    layer(sp.polygons(states))
 hist1<-histogram(corRasterDet_BaseT10[[1]]) 
 hist2<-histogram( corRasterDet_BaseT10_450v250[[1]])
library(gridExtra)
 lay <- rbind(c(1,1,2,2),
              c(1,1,2,2),
              c(1,1,2,2),
              c(3,3,4,4))
 grid.arrange(fig1, fig2, hist1, hist2, layout_matrix = lay)
 #grid.arrange(fig1, fig2, hist1, hist2, ncol=2)
 
  # correlations
  load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
  load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")
  # plot all corrs in one stack
  allCorr<-stack(corRasterDet_BaseT0[[1]], corRasterDet_BaseT10[[1]], 
                 corRasterDet_BaseT0_250v50[[1]], corRasterDet_BaseT10_250v50[[1]],
                 corRasterDet_BaseT0_450v250[[1]], corRasterDet_BaseT10_450v250[[1]])
  names(allCorr)<-c("50v450 BaseT0", "50v450 BaseT10",
                    "50v250 BaseT0", "50v250 BaseT10",
                    "250v450 BaseT0", "250v450 BaseT10")
  my.at <- seq(0, 1, 0.1)
  corrFig<-levelplot(allCorr, layout=c(2,3), par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Pearson-Corr (TopoWx 48-16)")+ 
    layer(sp.polygons(states))
  histFig<-histogram(allCorr, layout=c(2,3))
  print(corrFig, split=c(1,1,1,2), more=TRUE)
  print(histFig, split=c(1,2,1,2))  
    
  
# trying out gplot
  library(ggplot2)
  library(ggthemes)
  # map layers
  states <- getData('GADM', country='United States', level=1)
  states@data$id <- rownames(states@data)
  ggStates <- fortify(states, region = "id")
  ggStatesDF <- merge(ggStates, states@data, by = "id")
  
 
  gplot(allCorr) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable, nrow=3) +
    #scale_fill_gradient(low = 'white', high = 'blue', na.value=NA) +
    scale_fill_distiller(type = "seq", palette = "YlOrRd", direction = 1, na.value=NA)+
    coord_equal(xlim=c(-125, -66), ylim=c(25,50))+
    theme_gdocs()+
    geom_polygon(data=ggStatesDF, aes(x=long, y=lat, group=group),
                 fill=NA, color='grey', size=0.5)
  
  # develop single ggplots
p<-gplot(corRasterDet_BaseT0[[1]]) + geom_tile(aes(fill = value)) +
    #scale_fill_gradient(low = 'white', high = 'blue', na.value=NA) +
    scale_fill_distiller(type = "seq", palette = "YlOrRd", direction = 1, na.value=NA)+
    coord_equal(xlim=c(-125, -66), ylim=c(25,50))+
    theme_gdocs()+
    geom_polygon(data=ggStatesDF, aes(x=long, y=lat, group=group),
                 fill=NA, color='grey', size=0.1)+
  geom_tile(data = pval_data, aes(x, y), fill = "black", alpha = 0.8)
   
  
  
  # multi plot with ggplots
  library(cowplot)
  plot_grid(fig1, fig2, hist1, hist2, 
            labels = c("A", "B", "C","D"),
            ncol = 2, nrow = 2)
  
  
  
  
  
  # gplot script - gets data from raster
  gplot_data <- function(x, maxpixels = 50000)  {
    x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
    coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
    ## Extract values
    dat <- utils::stack(as.data.frame(raster::getValues(x)))
    names(dat) <- c('value', 'variable')
    
    dat <- dplyr::as.tbl(data.frame(coords, dat))
    
    if (!is.null(levels(x))) {
      dat <- dplyr::left_join(dat, levels(x)[[1]],
                              by = c("value" = "ID"))
    }
    dat
  }
  library(sp)
  pval_data <- gplot_data(corRasterDet_BaseT10[[2]]>0.05)
  # thin to only 1's
  pval_data <- pval_data[ which(pval_data$value==1),]