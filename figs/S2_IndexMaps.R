# Season length, start variability maps
# 8/14/18 MAC

library(raster)
library(rasterVis)

# map layers
states <- getData('GADM', country='United States', level=1)

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
            mask(sdDiff50_450Res/sdGDD50Res,maskNA)
            )
names(temp) <- c("Index-T0", "Index-T10")

# custom color ramp https://stackoverflow.com/questions/47459083/levelplot-color-key-range-and-extremes
#max(values(temp), na.rm = TRUE)
#min(values(temp), na.rm = TRUE)

index.at=c(min(values(temp), na.rm = TRUE),0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1,1.05,1.1, max(values(temp), na.rm = TRUE))
my.brks=seq(0.1, 1.5, by=0.1) # needs to match num of index.at
myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=c(0, 0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1,1.05,1.1, 10)), space="bottom")
reds = brewer.pal(5, "YlOrRd")
greens = brewer.pal(4, "Greens")
blues = brewer.pal(5, "Blues")
mapTheme <- rasterTheme(region=c('white', blues, greens, reds, "gray"))

# plot maps
#mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))
text2add<-c('a','b')
col.titles = c('50v450 (T0)','50v450 (T10)')

#index.at <- seq(0, 6, 0.05)
p0 <- levelplot(temp, par.settings = mapTheme, ylab=NULL, xlab=NULL, colorkey=myColorkey,
                at=index.at, names.attr=col.titles, layout=c(2,1),  scales=list(alternating=3))+ #scales=list(draw=FALSE)
  layer(panel.text(-120, 26, text2add[panel.number()],  cex=1))+
  layer(sp.polygons(states))

png("/home/crimmins/RProjects/TopoWx/figs/S2_IndexMap.png", width = 7, height = 5, units = "in", res = 300L)
print(p0, newpage = FALSE)
dev.off()
