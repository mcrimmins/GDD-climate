# Season length, start variability maps
# 8/14/18 MAC

library(raster)
library(rasterVis)

# map layers
states <- getData('GADM', country='United States', level=1)
USA<-getData('GADM', country='USA', level=0)

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')

# linear model  BASE 10----
# load mask
maskNA<-raster("./fixed/maskNAalt.grd")
# load datasets
load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")

# linear model BASE 0----
# load mask
maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd") 
# load datasets
load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")
load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")

temp<-stack(mask(sdDiff50_450_0Res/sdGDD50_0Res,maskNA_0), 
            mask(sdDiff250_50_0Res/sdGDD50_0Res,maskNA_0),
            mask(sdDiff450_250_0Res/sdGDD250_0Res,maskNA_0),
            mask(sdDiff50_450Res/sdGDD50Res,maskNA),
            mask(sdDiff250_50Res/sdGDD50Res,maskNA),
            mask(sdDiff450_250Res/sdGDD250Res,maskNA)
            )
# CONUS mask
temp<- mask(x = temp, mask = USA)

# add together
col.titles = c('a. Full Season (50GDD-450GDD, 0°C base temp)',
               'c. Early Season (50GDD-250GDD, 0°C base temp)',
               'e. Late Season (250GDD-450GDD, 0°C base temp)',
               'b. Full Season (50GDD-450GDD, 10°C base temp)',
               'd. Early Season (50GDD-250GDD, 10°C base temp)',
               'e. Late Season (250GDD-450GDD, 10°C base temp)')

# custom color ramp https://stackoverflow.com/questions/47459083/levelplot-color-key-range-and-extremes
#max(values(temp), na.rm = TRUE)
#min(values(temp), na.rm = TRUE)

index.at=c(min(values(temp), na.rm = TRUE),0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1,1.05,1.1, max(values(temp), na.rm = TRUE))
my.brks=seq(0.1, 1.5, by=0.1) # needs to match num of index.at
myColorkey <- list(at=my.brks, labels=list(at=my.brks,cex=0.53,labels=c(0, 0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1,1.05,1.1, 10)), 
                   space="bottom",width=0.8, height=0.5)
reds = brewer.pal(5, "YlOrRd")
greens = brewer.pal(4, "Greens")
blues = brewer.pal(5, "Blues")
spectral=(brewer.pal(14, "Spectral")) # only gives you 11 
#mapTheme <- rasterTheme(region=c('gray90', blues, greens, reds, "gray60"))
mapTheme <- rasterTheme(region=c("#72002f", spectral,'#413772'))

# plot maps
#mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))
text2add<-c('a','c','e','b','d','f')
#col.titles = c('50v450 (T0)','50v450 (T10)')

#index.at <- seq(0, 6, 0.05)
p0 <- levelplot(temp, par.settings = mapTheme, ylab=NULL, xlab=NULL, colorkey=myColorkey,
                at=index.at, sub=list(label="             SD(Duration)/SD(T1)",cex=0.6,font = 1), 
                names.attr=col.titles, par.strip.text=list(cex=0.53), 
                layout=c(3,2),  scales=list(alternating=3, cex=0.53))+ #scales=list(draw=FALSE)
  #layer(panel.text(-120, 26, text2add[panel.number()],  cex=0.5))+
  layer(sp.polygons(states))+
  layer(sp.polygons(states, col = 'gray40', lwd=0.1))

# png("/home/crimmins/RProjects/TopoWx/figs/S3_IndexMap.png", width = 7, height = 5, units = "in", res = 300L)
# print(p0, newpage = FALSE)
# dev.off()

pdf("/home/crimmins/RProjects/TopoWx/figs/manuscript/Fig4_IndexMap.pdf", width = 7.48, height = 4.53, pointsize = 8)
print(p0, newpage = FALSE)
dev.off()



# #alternate color ramp
# mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))
# my.at <- seq(0.5, 2, 0.1)
# p0 <- levelplot(temp, par.settings = mapTheme, ylab=NULL, xlab=NULL,
#                 at=my.at, names.attr=col.titles, par.strip.text=list(cex=0.5),
#                 layout=c(3,2),  scales=list(alternating=3))+ #scales=list(draw=FALSE)
#   layer(panel.text(-120, 26, text2add[panel.number()],  cex=1))+
#   layer(sp.polygons(states))
# 
# png("/home/crimmins/RProjects/TopoWx/figs/S2_IndexMap.png", width = 7, height = 5, units = "in", res = 300L)
# print(p0, newpage = FALSE)
# dev.off()
# 
# # ratio vs corr layers - correlation
# corrLayers<-stack(corRaster_BaseT0[[1]], corRaster_BaseT0_250v50[[1]], corRaster_BaseT0_450v250[[1]],
#                   mask(corRaster_BaseT10[[1]],maskNA), mask(corRaster_BaseT10_250v50[[1]], maskNA),
#                   mask(corRaster_BaseT10_450v250[[1]], maskNA))
# 
# tempBrick<-stack(temp,corrLayers)
#   corrStats<-layerStats(tempBrick, 'pearson', na.rm = TRUE)
#   #corrStats<-as.data.frame(corrStats$`pearson correlation coefficient`)
#   corrStats<-diag(corrStats$`pearson correlation coefficient`[1:6,7:12])
  