# FIGURE 2
# Correlation Figure - from correlationMaps.R
# MAC 08/12/2018

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

allpval<-stack(corRasterDet_BaseT0[[2]], corRasterDet_BaseT10[[2]],
               corRasterDet_BaseT0_250v50[[2]], corRasterDet_BaseT10_250v50[[2]],
               corRasterDet_BaseT0_450v250[[2]], corRasterDet_BaseT10_450v250[[2]])

# get stats
stats <- data.frame(Layer=character(0),
                    mean=double(0),
                    sdev=double(0),
                    stringsAsFactors=FALSE)
stats<-stats[1:nlayers(allCorr),]
for (i in 1:nlayers(allCorr)){
  stats$Layer[i]<-names(allCorr)[i]
  stats$mean[i]<-cellStats(allCorr[[i]], stat='mean', na.rm=TRUE)
  stats$sdev[i]<-cellStats(allCorr[[i]], stat='sd', na.rm=TRUE)
}
stats$panel<-c('a','b','c','d','e','f')
# labels for panels
text2add<-paste0(stats$panel,'. mean/SD r: ',round(stats$mean,2),'±',round(stats$sdev,2))

# add together
col.titles = c('50v450 (T0)','50v450 (T10)','50v250 (T0)','50v250 (T10)','250v450 (T0)','250v450 (T10)')
#row.titles = c('row1','row2')
#levelplot(s, layout=c(2,2), names.attr=col.titles,ylab=row.titles)
# see https://stat.ethz.ch/pipermail/r-sig-geo/2017-July/025840.html

corr.at <- seq(0, 1, 0.05)
pval.at <- seq(0.05, 0.95, 0.5)
p0 <- levelplot(allCorr, par.settings = YlOrRdTheme, ylab=NULL, xlab=NULL, 
                at=corr.at, names.attr=col.titles, layout=c(2,3),  scales=list(alternating=3), #scales=list(draw=FALSE)
                colorkey=list(space="bottom"))+
                layer(panel.text(-116, 26, text2add[panel.number()],  cex=0.6)) 
p1 <- levelplot(allpval, par.settings =GrTheme, at=pval.at,layout=c(2,3),alpha.regions=0.6)
p<-p0+p1+layer(sp.polygons(states))

# plot one map with overlay
#p0 <- levelplot(allCorr[[1]], par.settings = YlOrRdTheme, at=corr.at, main="Pearson-Corr (TopoWx 48-16)")
#p1 <- levelplot(allpval[[1]], par.settings =GrTheme, at=pval.at,alpha.regions=0.6)
#p0+p1+layer(sp.polygons(states))


png("/home/crimmins/RProjects/TopoWx/figs/fig2corrs.png", width = 7, height = 5, units = "in", res = 300L)
grid.newpage()
print(p, newpage = FALSE)
dev.off()


#png("output.png")
#p
#grid.text('here and there', x=.98, y=.5, rot=-90,
#          gp = gpar(col=4,
#                    fontfamily="HersheyGothicEnglish", cex=2))
#dev.off()

