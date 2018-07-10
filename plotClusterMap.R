# plot cluter map
# 04/18/18 MAC

#library(cowplot)
library(ggmap)
library(raster)
library(rasterVis)


# get state boundaries
states <- getData('GADM', country='United States', level=1)
# load data
load("./fixed/cluster12classMap.RData")

# color ramps ----
#darkcols <- brewer.pal(clusterN, "Set1")
#darkcols <- brewer.pal(clusterN, "Set3")
#darkcols <- brewer.pal(clusterN, "Paired")

# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
darkcols<-sample(col_vector, clusterN)

classMap<-as.factor(unC$map)
rat <- levels(classMap)[[1]]
# cluster names
rat[["cluster"]]<-c("N Plains","Gulf Coast","Ohio Valley","Upper Midwest","Southeast","Southwest",
                    "C Plains","S Plains","N Rockies","Pacific NW","S Rockies","Northeast")
# rat[["cluster"]]<-c("S Rockies","N Rockies","Northeast","Pacific NW","S Plains",
#                     "C Plains","N Plains","Southeast","Ohio Valley","Upper Midwest",
#                     "Gulf Coast","Southwest")
#                   
#rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 

# set up background map
conus<-get_map(location = "United States", zoom = 4, source="google", maptype = "terrain")
ggmap(conus)
bbMap<-attr(conus, 'bb')
  height<-with(bbMap, ur.lat-ll.lat)
  width<-with(bbMap, ur.lon-ll.lon)

# plot classified map
clusterMap<-levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
          margin=FALSE, main="50/250/450 GDD Z-score Anom clustering")+
  layer(sp.polygons(states))
  

clusterMap + layer(grid.raster(conus, width=width, height=height, default.units='native'), under = TRUE)



