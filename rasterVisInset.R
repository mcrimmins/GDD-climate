my.at <- seq(0, 1, 0.1)
corrFig<-levelplot(allCorr[[1]],  par.settings = YlOrRdTheme, at=my.at, margin=FALSE, main="Pearson-Corr (TopoWx 48-16)")+ 
  layer(sp.polygons(states))
histFig<-histogram(allCorr[[1]])

## Create viewports (more info in https://stat.ethz.ch/R-manual/R-devel/library/grid/doc/grid.pdf)
## Main viewport
vp1 <- viewport()
pushViewport(vp1)
print(corrFig, newpage = FALSE)
## Inset viewport
xi <- 0.35
yi <- 0.31
wi <- 0.25
hi <- 0.5
vp2 <- viewport(x = xi,
                y = yi,
                height = hi,
                width = wi)
pushViewport(vp2)
print(histFig, newpage=FALSE)