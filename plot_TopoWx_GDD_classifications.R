# Cluster GDDs, plot
# TopoWx 12/20/17

library(cowplot)
library(raster)
library(rasterVis)
library(sp)
library(maptools)
library(RStoolbox)
library(tidyr)
library(dplyr)


# set rasteroptions
#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')
#rasterOptions(todisk = TRUE)
tmpDir(create=TRUE)

# rsToolbox options
rsOpts(verbose=TRUE)

# Download States boundaries (might take time)
states <- getData('GADM', country='United States', level=1)

# clustering code ----

# # classify with 50/200/500 combo
# gdd50raw_x4<-stack("X4_rawDOY_baseT10_thresh50.grd")
# gdd200raw_x4<-stack("X4_rawDOY_baseT10_thresh200.grd")
# gdd500raw_x4<-stack("X4_rawDOY_baseT10_thresh500.grd")
# # set names
# names(gdd50raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
# names(gdd200raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD200")
# names(gdd500raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD500")

# # # or anoms
gdd50_x4<-stack("X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd200_x4<-stack("X4_anomDOY_baseT10_thresh200_1981-2010.grd")
gdd500_x4<-stack("X4_anomDOY_baseT10_thresh500_1981-2010.grd")
 
  # standardize grids with z-scores
  library(parallel)
  zscore<-function(x){scale(x, center = TRUE, scale = TRUE)}
  ptm <- proc.time()
  beginCluster(6)
    gdd50_x4 <- clusterR(gdd50_x4, calc, args=list(fun=zscore))
    gdd200_x4 <- clusterR(gdd200_x4, calc, args=list(fun=zscore))
    gdd500_x4 <- clusterR(gdd500_x4, calc, args=list(fun=zscore))
  endCluster()
  proc.time() - ptm  

# mask out   
  maskNA<-raster("maskNA.grd") 
  gdd50_x4 <-  mask(gdd50_x4, maskNA)  
  gdd200_x4 <- mask(gdd200_x4, maskNA)
  gdd500_x4 <- mask(gdd500_x4, maskNA)
  
# ---- Plot Mean DOYs
meanDOY50_x4<-raster("X4_meanDOY_baseT10_thresh50_1981-2010.grd")
  meanDOY50_x4 <- mask(meanDOY50_x4, maskNA)
meanDOY200_x4<-raster("X4_meanDOY_baseT10_thresh200_1981-2010.grd")
  meanDOY200_x4 <- mask(meanDOY200_x4, maskNA)
meanDOY500_x4<-raster("X4_meanDOY_baseT10_thresh500_1981-2010.grd")
  meanDOY500_x4 <- mask(meanDOY500_x4, maskNA)
# set names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd200_x4)<-paste0(seq(1948, 2016, by=1),".GDD200")
names(gdd500_x4)<-paste0(seq(1948, 2016, by=1),".GDD500")

# # plot anomaly maps
#  cluster12 <- readShapePoly("./mapFiles/cluster12poly")
# my.at <- seq(-4, 4, 0.1)
#   levelplot(gdd500_x4[[65]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD500 Anomalies - zscore")+ 
#     layer(sp.polygons(states))+
#     layer(sp.polygons(cluster12))
# my.at <- seq(1, 250, 20)
#   levelplot(gdd500raw_x4[[65]],  par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD500 DOY w/12 cluster boundaries")+ 
#     layer(sp.polygons(states))+
#     layer(sp.polygons(cluster12))
  
# CV of datalayers
# gdd50_x4cv<-overlay(gdd50raw_x4, fun=sd)
# my.at <- seq(0, 2, 0.1)
# levelplot( gdd500_x4CV, par.settings = RdBuTheme, margin=FALSE, at=my.at, main="GDD500_x4 Skewness (48-16)")+ 
#   layer(sp.polygons(states))
# levelplot(gdd50_x4sd/gdd50_x4mean, margin=FALSE)

# # kendall corr
# allGdd<-stack(gdd50raw_x4,gdd200raw_x4,gdd500raw_x4) # raw
# allGdd<-stack(gdd50_x4,gdd200_x4,gdd500_x4) # anom
# corRaster<-corLocal(gdd200_x4,gdd500_x4, test=TRUE, method="kendall")
# oranges <- rasterTheme(region=brewer.pal(8, "YlOrRd")) 
# my.at <- seq(-3, 3, 0.1)
#  levelplot(tempGrid, par.settings = oranges, at=my.at, margin=FALSE, main="GDD200x4-Anom/GDD500x4-Anom Kendall-Corr")+ 
#    layer(sp.polygons(states))+
#    layer(sp.polygons(test))
   
# 
# boxplot(corRaster[[1]],classMap)

# lat lon grids
# lonGrid <- init(gdd50_x4, 'x')
# latGrid <- init(gdd50_x4, 'y')

# try anoms vs raw....
#allGdd<-stack(gdd50_x4,gdd200_x4,gdd500_x4, meanDOY50_x4,meanDOY200_x4,meanDOY500_x4)
allGdd<-stack(gdd50_x4,gdd200_x4,gdd500_x4)
rm(gdd50_x4,gdd200_x4,gdd500_x4)
#allGdd<-stack(gdd50_x4,gdd200_x4,gdd500_x4, lonGrid, latGrid)
# allGdd<-stack(gdd50raw_x4,gdd200raw_x4,gdd500raw_x4)

# # pca of data
# pcaGDD<-rasterPCA(allGdd, nSamples = 250000, nComp = 5)
# levelplot(pcaGDD500$map,par.settings = RdBuTheme, main="GDD500 Anom - PCA")+
#        layer(sp.polygons(states))
# # pca by GDD
# pcaGDD50<-rasterPCA(gdd50_x4, nSamples = 250000, nComp = 6)
# pcaGDD200<-rasterPCA(gdd200_x4, nSamples = 250000, nComp = 6)
# pcaGDD500<-rasterPCA(gdd500_x4, nSamples = 250000, nComp = 6)
# allPCA<-stack(pcaGDD50$map,pcaGDD200$map,pcaGDD500$map)
# # get % var explained
# eigs <- pcaGDD50$model$sdev^2
# plot(eigs / sum(eigs))
# plot(pcaGDD50$model)

# cluster data
ptm <- proc.time()
  set.seed(1234)
  clusterN<-12
  unC <- unsuperClass(allGdd, nSamples = 250000, nClasses = clusterN, nStarts = 25, nIter = 1000)
proc.time() - ptm

# END CLUSTERING -----

# save/load cluster
# save.image("~/RProjects/TopoWx/cluster12classMap.RData")
# load("~/RProjects/TopoWx/fixed/cluster12classMap.RData")

# cluster diagnostics ----
# plot withinss
# withinss<-as.data.frame(cbind(seq(1,12,1),unC$model$withinss))
# colnames(withinss)<-c("cluster","error")
# p <-ggplot(withinss, aes(cluster, error))
# p +geom_bar(stat = "identity")+
#   scale_x_discrete(limits=as.character(seq(1,12,1)))+
#   labs(title="Within Sum of Squares Error by Cluster")

# # parallel version
# library(parallel)
# beginCluster(7)
#   unC <- clusterR(unsuperClass(allGdd, nSamples = 200000, nClasses = clusterN,nStarts = 10))
# endCluster()

# load saved cluster 
#load("~/RProjects/TopoWx/8cluster_allGdd_nsamples200000_nstarts10.RData")

# # 5 colors
# colors <- c("#A7A7A7",
#             "dodgerblue",
#             "firebrick",
#             "forestgreen",
#             "gold")

#darkcols <- brewer.pal(clusterN, "Set1")
darkcols <- brewer.pal(clusterN, "Set3")

# more colors
# https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
darkcols<-sample(col_vector, clusterN)

# smooth or clump?
#test <- focal(classMap, w=matrix(1, 7, 7), fun=min)
# test<-clump(classMap)

#cluster12 <- readShapePoly("./mapFiles/cluster12poly")

classMap<-as.factor(unC$map)
rat <- levels(classMap)[[1]]
# cluster names
rat[["cluster"]]<-c("N Rockies","Northeast","S Plains","S Rockies","N Plains",
                "Midwest","Southeast","Mid Atlantic","Upper Midwest","Southwest",
                "Gulf Coast","Pacific NW")
#rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 

# create polygon shapefile
#library(rgdal)
#library(rgeos)
#library(maptools)
# clusterpoly<-rasterToPolygons(classMap, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
# proj4string(clusterpoly) <- "+proj=longlat +datum=WGS84"
# writeOGR(clusterpoly, ".", "./mapFiles/cluster12final", driver="ESRI Shapefile")

# plot classified map
levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
          margin=FALSE, main="50/250/450 GDD Z-score Anom clustering")+
  layer(sp.polygons(states))+
  layer(sp.points(clusterCenter, phc=3, col="black"))
#  layer(sp.polygons(cluster12))

# ## plot with cluster boundaries
# my.at <- seq(-50, 50, 5)
# #levelplot(gdd50, par.settings = RdBuTheme, at=my.at, main="GDD50-BaseT10")
# levelplot(gdd500_x4[[59:69]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD500x4 Anoms (BaseT10-TopoWx,81-00 normal)")+
#   layer(sp.polygons(states))+
#   layer(sp.polygons(test))

# cluster correlations
# library(corrplot)
# centers<-as.data.frame(t(unC$model$centers))
# res <- cor(centers, method = "kendall")
# corrplot(res, type = "upper", order = "original", 
#          tl.col = "black", tl.srt = 45, title="Intercluster Kendall Corrs")

# # cluster histograms
# # approach 3
# z <- layerize(classMap, falseNA=TRUE)
# sz <- z[[9]] * gdd50_x4
# x <- hist(sz) #hist(sz, breaks=c(0,5,15,30,45,65))
# f3 <- sapply(x, function(i) i$counts)
# 
# boxplot(gdd50_x4,classMap)

# time series plots
# centers<-as.data.frame(t(unC$model$centers))
# centers$yearLayer<-rownames(centers)
# centers<-centers %>% gather(yearLayer, 1:clusterN)
# colnames(centers)<-c("code","cluster","GDDValue")
# centers<-separate(centers,code,c("X","code"), sep ="X")
# centers<-separate(centers,code,c("year","threshold"))
# centers$year<-as.numeric(centers$year)

# correlate centers with each pixel in each cluster? 

# get summary stats for plots
# df.sd.GDD <- centers %>%
#   group_by(cluster,threshold) %>% # add in threshold
#   summarise(sdGDD = round(sd(GDDValue), 2))
# # add positions
# posLabel<-as.data.frame(do.call(rbind, replicate(clusterN, cbind(c(1960,1980,2000),c(200,200,200)), simplify=FALSE)))
# df.sd.GDD<-cbind(as.data.frame(df.sd.GDD),posLabel)

#library(ggrepel) Z-SCORE PLOT GGPLOT
# ggplot(centers, aes(x=year,y=GDDValue, color=factor(threshold))) +
#   facet_wrap(~cluster, nrow = 1)+
#   geom_line() +
#   geom_smooth(method = "lm")+
#   ylim(c(-3,3))+
#   background_grid(major = "xy", minor = "xy")+
#   scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
#   labs(title=paste0("z-score Anom for GDD Thresholds"),
#        x ="Year", y = "z-score")+
#   geom_text(x = 2000, y = 3, 
#             aes(label = paste0("SD: ", sdGDD)), 
#             data = df.sd.GDD, check_overlap = TRUE)
  
# # # # find optimal cluster number
# clusterN=47
# wss<-allGdd[[1]]@nrows*allGdd[[1]]@ncols
#  minwss<-allGdd[[1]]@nrows*allGdd[[1]]@ncols
#  totss<-allGdd[[1]]@nrows*allGdd[[1]]@ncols
#  btwss<-allGdd[[1]]@nrows*allGdd[[1]]@ncols
# for (i in 2:clusterN){
#   set.seed(1234)
#   unC <- unsuperClass(allGdd, nSamples = 250000, nClasses = i, nStarts = 25, nIter = 1000)
#   wss[i]<-unC$model$tot.withinss
#    minwss[i]<-min(unC$model$withinss)
#    totss[i]<-unC$model$totss
#    btwss[i]<-unC$model$betweenss
#    print(i)
# }
#   plot(2:clusterN, wss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms z-score",
#          ylab="Within groups sum of squares", ylim=c(min(wss[2:clusterN]),max(wss[2:clusterN])))
#   plot(3:clusterN, diff(wss[2:clusterN])*-1, type="b", xlab="Number of Clusters - GDD Anoms z-score",
#       ylab="Diff Within groups sum of squares")#, ylim=c(min(diff(wss[2:clusterN])),max(diff(wss[2:clusterN]))))
#  plot(2:clusterN, btwss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms z-score",
#       ylab="Between groups sum of squares",  ylim=c(min(btwss[2:clusterN]),max(btwss[2:clusterN])))
#  plot(2:clusterN, minwss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms z-score",
#       ylab="Min groups sum of squares",  ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
# plot(2:clusterN, totss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms z-score",
#      ylab="Diff Within groups sum of squares",  ylim=c(min(totss[2:clusterN]),max(totss[2:clusterN])))
 
# # polygon
  # clusterpoly<-rasterToPolygons(classMap, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
  # writePolyShape(clusterpoly, "./mapFiles/cluster12poly")
  # cluster12 <- readShapePoly("./mapFiles/cluster12poly")
# 
# levelplot(classMap, par.settings = BuRdTheme, at=my.at, margin=FALSE, main="GDD50x4/GDD500x4 Kendall-Corr")+ 
#   layer(sp.polygons(test))

# save classMap
#save(classMap, file="cluster12classMap.RData")
#load("cluster12classMap.RData")

# end cluster diagnostics ----


# try zonal stats by cluster map
gdd50raw_x4<-stack("X4_rawDOY_baseT10_thresh50.grd")
  gdd50raw_x4 <- mask(gdd50raw_x4, maskNA)
gdd200raw_x4<-stack("X4_rawDOY_baseT10_thresh200.grd")
  gdd200raw_x4 <- mask(gdd200raw_x4, maskNA)
gdd500raw_x4<-stack("X4_rawDOY_baseT10_thresh500.grd")
  gdd500raw_x4 <- mask(gdd500raw_x4, maskNA)
# set names
names(gdd50raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd200raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD200")
names(gdd500raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD500")
allGddraw<-stack(gdd50raw_x4,gdd200raw_x4,gdd500raw_x4)
  rm(gdd50raw_x4,gdd200raw_x4,gdd500raw_x4)

# gridded corrs, mean, stdev
  # gdd50_500pearson<-corLocal(gdd50raw_x4,gdd500raw_x4, test=TRUE, method="pearson")
  # # stdev from raw diffs
  # beginCluster(3)
  # gdd50raw_x4SD <- clusterR(gdd50raw_x4, overlay, args=list(fun=sd))
  # gdd50raw_x4mean <- clusterR(gdd50raw_x4, overlay, args=list(fun=mean))
  # endCluster()
  # s <- stack(gdd50raw_x4SD, gdd50raw_x4mean, gdd50_500pearson[[1]])
  # names(s) <- c('GDD50StDev', 'GDD50Mean', 'GDDcorr')
  # #xyplot(GDD50StDev~GDD50Mean, data=s)
  # hexbinplot(GDD50Mean~GDD50StDev, data=s, aspect=1)
  # hexbinplot(GDDcorr~GDD50Mean, data=s, aspect=1)
  
# # get zonal mean of each cluster
# zStats<-as.data.frame(t(zonal(allGddraw, classMap, 'mean')))
# zStats<-zStats[-1,]
# zStats$yearLayer<-rownames(zStats)
# zStats<-zStats %>% gather(yearLayer, 1:clusterN)
# colnames(zStats)<-c("code","cluster","GDDValue")
# zStats<-separate(zStats,code,c("X","code"), sep ="X")
# zStats<-separate(zStats,code,c("year","threshold"))
# zStats$year<-as.numeric(zStats$year)
# 
  # ggplot(zStats, aes(x=year,y=GDDValue, color=factor(threshold))) +
  #   facet_wrap(~cluster, nrow = 1)+
  #   geom_line() +
  #   geom_smooth(method = "lm")+
  #   ylim(c(0,200))+
  #   background_grid(major = "xy", minor = "xy")+
  #   scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  #   labs(title=paste0("DOY for GDD Thresholds by Cluster"),
  #        x ="Year", y = "Day of Year")
  # 
# detrended plots
  library(pracma)
  DTzStats <- zStats %>%
    group_by(threshold,cluster) %>% # add in threshold
    #summarise(corrs = cor(GDDValue, GDDValue))
    do(data.frame(GDDValue=detrend(.$GDDValue, 'linear')))
  DTzStats$year<- zStats$year

ggplot(DTzStats, aes(x=year,y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line() +
  geom_smooth(method = "lm")+
  ylim(c(-40,40))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY for GDD Thresholds by Cluster"),
       x ="Year", y = "Day of Year")

# --- plot with sd intervals
# get zonal mean of each cluster
zStats<-as.data.frame(t(zonal(allGddraw, classMap, 'mean'))) # mean or median
zStats<-zStats[-1,]
zStats$yearLayer<-rownames(zStats)
zStats<-zStats %>% gather(yearLayer, 1:clusterN)
colnames(zStats)<-c("code","cluster","GDDValue")
zStats<-separate(zStats,code,c("X","code"), sep ="X")
zStats<-separate(zStats,code,c("year","threshold"))
zStats$year<-as.numeric(zStats$year)

clusterNames<-c("N Rockies","Northeast","S Plains","S Rockies","N Plains",
  "Midwest","Southeast","Mid Atlantic","Upper Midwest","Southwest",
  "Gulf Coast","Pacific NW")
nameLUP<-as.data.frame(cbind(paste0("V",as.character(seq(1, clusterN, by=1))),c("N Rockies","Northeast","S Plains","S Rockies","N Plains",
                                                      "Midwest","Southeast","Mid Atlantic","Upper Midwest","Southwest",
                                                      "Gulf Coast","Pacific NW"))) 
# lookup table to add in names
new<-zStats
new[] <- nameLUP$V2[match(unlist(zStats), nameLUP$V1)]
zStats$cluster<-new$cluster
# factor order 
zStats$cluster<-factor(zStats$cluster, levels=c("Southwest","S Plains","S Rockies","N Plains","Pacific NW","N Rockies",
                                                  "Gulf Coast","Southeast","Mid Atlantic","Midwest","Northeast","Upper Midwest"))
# early/late factor
zStats$period <- ifelse(zStats$year > 1980,"late", "early")

# climate indices ----
pna<-read.table("https://www.esrl.noaa.gov/psd/data/correlation/pna.data", skip=1, nrows = 71)
colnames(pna)<-c("year",month.abb[seq(1,12,1)])
pna[pna == -99.9] <- NA
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]
meanGDDsPNA<-merge(pna, meanGDDs, by=c("year"))
#meanGDDsPNA<-meanGDDsPNA[which(meanGDDsPNA$year>1980),]
corrPNA <- meanGDDsPNA %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(meanGDDsPNA$Feb, meanGDDsPNA$GDD50,use="na.or.complete"))
  do(data.frame(Cor=t(cor((.[,2]+.[,3]+.[,4])/3, .[,16], method="pearson",use="na.or.complete"))))

ggplot(meanGDDsPNA, aes(x=(Jan+Feb+Mar)/3, y=GDD50, color=year))+
  geom_point(size=0.75)+
  facet_wrap(~cluster, nrow=2)+
  #xlim(0,200)+
  #ylim(0,200)+
  geom_smooth(method = "lm")+
  #coord_fixed()+
  labs(title="Pacific-North America Index vs GDD50",
       x ="PNA Index", y = "Day of Year")+
  #geom_abline(intercept =0, slope =1)+
  scale_color_gradient2(low="blue", mid="yellow",high="red", midpoint = 1983,
                       guide = guide_legend(title="Year"))+
  theme_bw()

# NAO
nao<-read.table("https://www.esrl.noaa.gov/psd/data/correlation/nao.data", skip=1, nrows = 71)
colnames(nao)<-c("year",month.abb[seq(1,12,1)])
nao[nao == -99.9] <- NA
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]
meanGDDsNAO<-merge(nao, meanGDDs, by=c("year"))
#meanGDDsNAO<-meanGDDsNAO[which(meanGDDsNAO$year>1980),]
corrNAO <- meanGDDsNAO %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(meanGDDsPNA$Feb, meanGDDsPNA$GDD50,use="na.or.complete"))
  do(data.frame(Cor=t(cor((.[,2]+.[,3])/2, .[,16], method="pearson",use="na.or.complete"))))

ggplot(meanGDDsNAO, aes(x=(Jan+Feb)/2, y=GDD50, color=year))+
  geom_point(size=0.75)+
  facet_wrap(~cluster, nrow=2)+
  #xlim(0,200)+
  #ylim(0,200)+
  geom_smooth(method = "lm")+
  #coord_fixed()+
  labs(title="North Atlantic Oscillation Index vs GDD50",
       x ="PNA Index", y = "Day of Year")+
  #geom_abline(intercept =0, slope =1)+
  scale_color_gradient2(low="blue", mid="yellow",high="red", midpoint = 1983,
                        guide = guide_legend(title="Year"))+
  theme_bw()

# EPO
epo<-read.table("https://www.esrl.noaa.gov/psd/data/correlation/epo.data", skip=1, nrows = 71)
colnames(epo)<-c("year",month.abb[seq(1,12,1)])
epo[epo == -99.9] <- NA
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]
meanGDDsEPO<-merge(epo, meanGDDs, by=c("year"))
#meanGDDsEPO<-meanGDDsEPO[which(meanGDDsEPO$year>1980),]
corrEPO <- meanGDDsEPO %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(meanGDDsPNA$Feb, meanGDDsPNA$GDD50,use="na.or.complete"))
  do(data.frame(Cor=t(cor((.[,2]+.[,3])/2, .[,16], method="pearson",use="na.or.complete"))))

ggplot(meanGDDsEPO, aes(x=(Feb+Mar)/2, y=GDD50, color=year))+
  geom_point(size=0.75)+
  facet_wrap(~cluster, nrow=2)+
  #xlim(0,200)+
  #ylim(0,200)+
  geom_smooth(method = "lm")+
  #coord_fixed()+
  labs(title="East Pacific Oscillation Index vs GDD50",
       x ="PNA Index", y = "Day of Year")+
  #geom_abline(intercept =0, slope =1)+
  scale_color_gradient2(low="blue", mid="yellow",high="red", midpoint = 1983,
                        guide = guide_legend(title="Year"))+
  theme_bw()

# SOI
soi<-read.table("https://www.esrl.noaa.gov/psd/data/correlation/soi.data", skip=1, nrows = 71)
colnames(soi)<-c("year",month.abb[seq(1,12,1)])
soi[soi == -99.99] <- NA
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]
meanGDDsSOI<-merge(soi, meanGDDs, by=c("year"))
#meanGDDsSOI<-meanGDDsSOI[which(meanGDDsSOI$year>1980),]
corrSOI <- meanGDDsSOI %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(meanGDDsPNA$Feb, meanGDDsPNA$GDD50,use="na.or.complete"))
  do(data.frame(Cor=t(cor((.[,2]+.[,3])/2, .[,16], method="pearson",use="na.or.complete"))))

ggplot(meanGDDsSOI, aes(x=(Feb+Mar)/2, y=GDD50, color=year))+
  geom_point(size=0.75)+
  facet_wrap(~cluster, nrow=2)+
  #xlim(0,200)+
  #ylim(0,200)+
  geom_smooth(method = "lm")+
  #coord_fixed()+
  labs(title="Southern Oscillation Index vs GDD50",
       x ="SOI Index", y = "Day of Year")+
  #geom_abline(intercept =0, slope =1)+
  scale_color_gradient2(low="blue", mid="yellow",high="red", midpoint = 1983,
                        guide = guide_legend(title="Year"))+
  theme_bw()

# -----

# trends and stdev ----
# # linear trends
# # get sdevs for each cluster
 # library("broom")
 #  groupsLM <- zStats %>%
 #    group_by(cluster, threshold) 
 #  GDDTrends<-do(groupsLM,glance(lm(year ~ GDDValue, data = .)))
 #  GDDSlopes<-do(groupsLM,tidy(lm(year ~ GDDValue, data = .)))
 #  GDDSlopes<-GDDSlopes[which(GDDSlopes$term=="GDDValue"),]
 #  GDDTrends<-merge(GDDTrends, GDDSlopes, by=c("cluster","threshold"))
  
# var.test for diffs in sdevs
# zStatsEarly<-zStats[which(zStats$year<=1980),]
# zStatsLate<-zStats[which(zStats$year>1980),]
# test<-zStats[c(-1,-2)]
# test<-spread(test, period, GDDValue)
#   groupsLM <- zStats %>%
#     group_by(cluster, threshold) 
#   GDDTrends<-do(glance(var.test(groupsLM$year,groupsLM$GDDValue)))
# end trend/sdevs ----

# correlations
# get summary stats for plots
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]

# model GDD500 day based on GDD50 and 200
 library("broom")
 groupsLM <- meanGDDs %>%
   group_by(cluster)
 GDDModel<-do(groupsLM,glance(lm(GDD500 ~ GDD50, data = .)))
 #GDDModelTidy<-do(groupsLM,tidy(lm(GDD500 ~ GDD50, data = .)))
 GDDModelresids<-do(groupsLM,augment(lm(GDD500 ~ GDD50, data = .)))
 # add years
 years<-as.data.frame(rep(seq(1948,2016,1), 12))
 colnames(years)<-"year"
 GDDModelresids<-cbind(as.data.frame(GDDModelresids),years)
 #GDDSlopes<-GDDSlopes[which(GDDSlopes$term=="GDDValue"),]
 #GDDTrends<-merge(GDDTrends, GDDSlopes, by=c("cluster","threshold"))
 ggplot(GDDModelresids, aes(x=year, y=.resid))+
   #geom_point(size=0.75)+
   geom_line()+
   facet_wrap(~cluster, nrow=2)+
   #xlim(0,200)+
   #ylim(0,200)+
   #geom_smooth(method = "lm")+
   #coord_fixed()+
   labs(title=paste0("Resid of predicted GDD500 based on GDD50"),
        x ="Year", y = "Residual (Days)")+
   theme_bw()
 # fit plots
 ggplot(GDDModelresids, aes(x=GDD500+.resid, y=GDD500))+
   geom_point(size=0.75)+
   facet_wrap(~cluster, nrow=2)+
   xlim(0,200)+
   ylim(0,200)+
   geom_smooth(method = "lm")+
   coord_fixed()+
   labs(title="Resid of predicted GDD500 based on GDD50",
        x ="Predicted", y = "Observed")+
   geom_abline(intercept =0, slope =1)+
   theme_bw()
 
 
# ggplot correlation heatmaps ----
# detrend before correlating
 library(pracma)
 DTzStats <- zStats %>%
   group_by(threshold,cluster) %>% # add in threshold
   #summarise(corrs = cor(GDDValue, GDDValue))
   do(data.frame(GDDValue=detrend(.$GDDValue, 'linear')))
 DTzStats$year<- zStats$year
 meanGDDs<-spread(DTzStats,threshold,GDDValue)
 meanGDDs<-meanGDDs[,c(1,2,4,3,5)]
 
 # or just regular data
 #meanGDDs<-spread(zStats,threshold,GDDValue)
 #meanGDDs <- meanGDDs[-1]
 #meanGDDs<-meanGDDs[,c(1,2,4,3,5)]
 
 
library(reshape2)
corrGDD <- meanGDDs %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(GDDValue, GDDValue))
  do(data.frame(Cor=t(cor(.[,3:5], .[,3:5], method="pearson")))) # [4:6] or [3:5]
labels<-as.data.frame(rep(c("Cor.GDD50y","Cor.GDD200y", "Cor.GDD500y"), 12))
colnames(labels)<-"corrLabels"
corrGDD<-cbind(labels,as.data.frame(corrGDD))
corrGDD<-melt(corrGDD)
corrGDDall<-corrGDD

# # pval
# library(plyr)
# library(dplyr)
# # cor.test function
# corfun<-function(x, y) {
#   corr=(cor.test(x, y))
# }
# GDD50_500<-ddply(meanGDDs, .(cluster), summarise,
#       pval=corfun(GDD50,GDD500)$p.value,
#       cor=corfun(GDD50,GDD500)$estimate,
#       alt=corfun(GDD50,GDD500)$alternative
#       )

# set factors
corrGDD$corrLabels<-factor(corrGDD$corrLabels, levels=c("Cor.GDD50y","Cor.GDD200y","Cor.GDD500y"))

corrGDD$cluster<-factor(corrGDD$cluster, levels=c("Southwest","S Plains","S Rockies","N Plains","Pacific NW","N Rockies",
                                                  "Gulf Coast","Southeast","Mid Atlantic","Midwest","Northeast","Upper Midwest"))
# ggplot heat map
p<- ggplot(corrGDD, aes(variable, corrLabels))+
  geom_tile(aes(fill = value),colour = "white")+
  geom_text(aes(x=variable, y=corrLabels, label = round(value, 2)), size=4)+
  scale_fill_gradient2(low="white", mid="yellow",high="red", midpoint = 0.5,
                       guide = guide_legend(title="Correlation"))+
  facet_wrap(~ cluster,nrow=2)+
  ggtitle("1948-2016 GDD Correlations (mean GDDs, Pearson r)")
p+  theme(axis.text.x = element_text(angle = 330, hjust = 0))

# late period correlation 1981-2016
meanGDDs <- meanGDDs[ which(meanGDDs$year >= 1981),] # meanGDDs$year >= 1981
corrGDD <- meanGDDs %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(GDDValue, GDDValue))
  do(data.frame(Cor=t(cor(.[,4:6], .[,4:6], method="pearson"))))
labels<-as.data.frame(rep(c("Cor.GDD50y","Cor.GDD200y", "Cor.GDD500y"), 12))
colnames(labels)<-"corrLabels"
corrGDD<-cbind(labels,as.data.frame(corrGDD))
corrGDD<-melt(corrGDD)
corrGDDlate<-corrGDD
#corrGDDearly<-corrGDD

corrGDD$corrLabels<-factor(corrGDD$corrLabels, levels=c("Cor.GDD50y","Cor.GDD200y","Cor.GDD500y"))

corrGDD$cluster<-factor(corrGDD$cluster, levels=c("Southwest","S Plains","S Rockies","N Plains","Pacific NW","N Rockies",
                                                  "Gulf Coast","Southeast","Mid Atlantic","Midwest","Northeast","Upper Midwest"))

p<- ggplot(corrGDD, aes(variable, corrLabels))+
  geom_tile(aes(fill = value),colour = "white")+
  geom_text(aes(x=variable, y=corrLabels, label = round(value, 2)), size=4)+
  scale_fill_gradient2(low="white", mid="yellow",high="red", midpoint = 0.5,
                       guide = guide_legend(title="Correlation"))+
  facet_wrap(~ cluster,nrow=2)+
  ggtitle("1948-1980 GDD Correlations (mean GDDs, Pearson r)")
p+  theme(axis.text.x = element_text(angle = 330, hjust = 0))

# corr diff heatmap
corrGDD$value<-corrGDDlate$value-corrGDDearly$value
p<- ggplot(corrGDD, aes(variable, corrLabels))+
  geom_tile(aes(fill = value),colour = "white")+
  geom_text(aes(x=variable, y=corrLabels, label = round(value, 2)), size=4)+
  scale_fill_gradient2(low="blue", mid="white",high="red", midpoint = 0,
                       guide = guide_legend(title="Correlation"))+
  facet_wrap(~ cluster,nrow=2)+
  ggtitle("Late(81-16)-early(48-80) GDD Correlations (mean GDDs, Pearson r)")
p+  theme(axis.text.x = element_text(angle = 330, hjust = 0))
# end heatmaps ----


# diff in corrs ----
library(psych)
tempX<-corrGDDall[which(corrGDDall$corrLabels=="Cor.GDD500y" & corrGDDall$cluster=="Mid Atlantic" & corrGDDall$variable=="Cor.GDD50"),]
tempY<-corrGDDlate[which(corrGDDlate$corrLabels=="Cor.GDD500y" & corrGDDlate$cluster=="Mid Atlantic" & corrGDDlate$variable=="Cor.GDD50"),]
paired.r(tempX[1,4],tempY[1,4],NULL, 69, 36) # independent correlations, different sample sizes
# early vs late corrs
tempX<-corrGDDearly[which(corrGDDearly$corrLabels=="Cor.GDD500y" & corrGDDearly$cluster=="Northeast" & corrGDDearly$variable=="Cor.GDD50"),]
tempY<-corrGDDlate[which(corrGDDlate$corrLabels=="Cor.GDD500y" & corrGDDlate$cluster=="Northeast" & corrGDDlate$variable=="Cor.GDD50"),]
paired.r(tempX[1,4],tempY[1,4],NULL, 33, 36) # independent correlations, different sample sizes
# ----

# comparing corrs, means and sdevs in scatterplots ----
# get sdevs of GDDs by threshold and region
df.sd.GDD <- zStats %>%
  group_by(cluster, threshold) %>% # add in threshold
  summarise(sdGDD = round(sd(GDDValue), 2), meanGDD=mean(GDDValue))
ggplot(df.sd.GDD, aes(x=sdGDD, y=meanGDD, color=cluster, shape=factor(threshold)))+
  geom_point(size=5)+
  ggtitle("Mean GDD vs GDD Stdev by threshold and region")

ggplot(df.sd.GDD, aes(x=sdGDD, y=meanGDD, color=factor(threshold)))+
  geom_point(size=5)+
  geom_smooth(method="lm")+
  ggtitle("Mean GDD vs GDD Stdev by threshold")

ggplot(df.sd.GDD, aes(x=sdGDD, y=meanGDD))+
  geom_point(size=5)+
  geom_smooth(method="lm")+
  ggtitle("Mean GDD vs GDD Stdev")+
  annotate("text", x = 14, y = 150, label =as.character(round(cor(df.sd.GDD$sdGDD,df.sd.GDD$meanGDD),2)))
#cor.test(df.sd.GDD$sdGDD,df.sd.GDD$meanGDD)
# join corrs to df.sd table
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]
#library(reshape2)
corrGDD <- meanGDDs %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(GDDValue, GDDValue))
  do(data.frame(Cor=t(cor(.[,4:6], .[,4:6], method="pearson"))))
labels<-as.data.frame(rep(c("GDD50","GDD200", "GDD500"), 12))
colnames(labels)<-"threshold"
corrGDD<-cbind(labels,as.data.frame(corrGDD))
statsGDD<-merge(df.sd.GDD, corrGDD, by=c("cluster","threshold"))
statsGDD<-statsGDD[which(statsGDD$threshold=="GDD50"),]
ggplot(statsGDD, aes(x=Cor.GDD500, y=meanGDD, color=sdGDD))+
  geom_point(size=4)+
  ggtitle("Corr50v500 vs Mean GDD50, region::GDD50 stdevs")+
    scale_color_gradient2(low="blue", mid="yellow",high="red", midpoint = 11,
                         guide = guide_legend(title="stdev"))

# scatterplot by region
meanGDDs$cluster<-factor(meanGDDs$cluster, levels=c("Southwest","S Plains","S Rockies","N Plains","Pacific NW","N Rockies",
                                                  "Gulf Coast","Southeast","Mid Atlantic","Midwest","Northeast","Upper Midwest"))

ggplot(meanGDDs, aes(x=GDD50, y=GDD500, color=year))+
  geom_point(size=0.75)+
  facet_wrap(~cluster, nrow=2)+
  xlim(-40,40)+
  ylim(-40,40)+
  #geom_smooth(method = "lm")+
  #coord_fixed()+
  ggtitle("Detrended GDD50 by GDD500 - regions")+
  scale_color_gradient2(low="blue", mid="yellow",high="red", midpoint = 1983,
                        guide = guide_legend(title="Correlation"))+
  theme_bw()
# ----

# get zonal sdev
for (i in seq(from=1, to=207, by=3)){
  j=i+2
  temp<-zonal(allGddraw[[i:j]], classMap, 'sd')
  if (i==1){
    zStdev <- temp
  }else{
    zStdev <- cbind(zStdev, temp[,2:4]) # bind
  }
}

zStdev<-as.data.frame(t(zStdev))
zStdev<-zStdev[-1,]
zStdev$yearLayer<-rownames(zStdev)
zStdev<-zStdev %>% gather(yearLayer, 1:clusterN)
colnames(zStdev)<-c("code","cluster","GDDValue")
zStdev<-separate(zStdev,code,c("X","code"), sep ="X")
zStdev<-separate(zStdev,code,c("year","threshold"))
zStdev$year<-as.numeric(zStdev$year)

# get sdev intervals
zStats$sdevPos<-zStats$GDDValue+zStdev$GDDValue
zStats$sdevNeg<-zStats$GDDValue-zStdev$GDDValue

# get sdevs for each cluster
df.sd.GDD <- zStdev %>%
  group_by(cluster, threshold) %>% # add in threshold
  summarise(sdGDD = round(sd(GDDValue), 2))

# arrange facet wrap order
# facetOrder <- zStats %>%
#   group_by(cluster, threshold) %>% # add in threshold
#   summarise(meanGDD = round(mean(GDDValue), 2))
# facetOrder<-facetOrder[which(facetOrder$threshold=="GDD200"),]
# facetOrder<-facetOrder[order(facetOrder$meanGDD),]
# zStats$cluster<-factor(zStats$cluster, levels = facetOrder$cluster)
  
# rolling mean option
#library(zoo)

ggplot(zStats, aes(x=year,y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line()+
  #geom_line(aes(y=rollmean(GDDValue, 5, na.pad=TRUE)))+
  geom_smooth(method = "lm")+
  #geom_ribbon(aes(ymax = sdevPos, ymin = sdevNeg,linetype=NA), alpha = 0.3)+
  ylim(c(-5,205))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY for GDD Thresholds by Cluster"),
       x ="Year", y = "Day of Year")
  # geom_text(x = 1980, y = 200, 
  #           aes(label = paste0("200 GDD SD: ", sdGDD)), 
  #           data = df.sd.GDD, check_overlap = TRUE)

# early/late boxplots
ggplot(zStats, aes(x=as.factor(period),y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 2)+
  geom_boxplot()+
  theme_bw()+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("Early (48-80) vs Late (81-16) GDD stats"),
       x ="Period", y = "Day of Year")

# diff in GDD500-GDD50
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]
meanGDDs$diff500_50<-meanGDDs$GDD500-meanGDDs$GDD50
ggplot(meanGDDs, aes(x=year,y=diff500_50)) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line()+
  geom_smooth(method="lm")+
  labs(title=paste0("Mean GDD500-GDD50 DOY by year"),
       x ="Year", y = "Days")+
  theme_bw()

# get sdevs for each cluster
df.sd.GDD <- zStdev %>%
  group_by(cluster) %>% # add in threshold
  summarise(sdGDD = round(sd(GDDValue), 2))

#showTmpFiles()
#removeTmpFiles(h=0) # in hours

# zonal stats for clusters ----
# get some data 
elev<-raster("./mapFiles/X4_elev.grd")
# need to resample to X4
#normTmin<-stack("./mapFiles/normals_tmin.nc", varname="tmin_normal")
#normTmax<-stack("./mapFiles/normals_tmax.nc", varname="tmax_normal")

clusterStats<-as.data.frame(cbind(classMap@data@attributes[[1]][2],freq(classMap, useNA="no"),zonal(elev, classMap, 'mean'),zonal(elev, classMap, 'sd'),
                          zonal(meanDOY50_x4, classMap, 'mean'),zonal(meanDOY50_x4, classMap, 'sd'),
                          zonal(meanDOY200_x4, classMap, 'mean'),zonal(meanDOY200_x4, classMap, 'sd'),
                          zonal(meanDOY500_x4, classMap, 'mean'),zonal(meanDOY500_x4, classMap, 'sd')))

clusterStats <- clusterStats[c(-2,-4,-6,-8,-10,-12,-14,-16,-18)]
colnames(clusterStats)<-c("cluster","#pixels","meanElev","sdevElev","doy50mean","doy50sdev","doy200mean","doy200sdev","doy500mean","doy500sdev")
# ----

# sliding correlation ----
# 2011-03-04, https://www.r-bloggers.com/new-r-code-for-moving-correlations/
# v0.01

MovingCor <- function() {
  # Computes moving correlations between two vectors with symmetrical windows.
  #
  # Args:
  #   x: One of the two vectors whose correlation is to be calculated.
  #   y: The other vector. Note that it must be of the same length as x.
  #   window.size: The size of windows to be used for each calculated
  #                correlation. Note that if even numbers are chosen, the
  #                window will not be skewed as there will be one extra value
  #                on the upper-side of the window. Default size is 21.
  #   method: The method of correlation. May be: "pearson", "kendall", or
  #           "spearman". Default is "pearson".
  #
  # Returns:
  #   A vector of the moving correlations.
  n <- length(x)
  # Setup a few catches for error handling.
  if (TRUE %in% is.na(y) || TRUE %in% is.na(x)) {
    stop("Arguments x and y cannot have missing values.")
  }
  if (n <= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  out <- rep(NA, round(window.size/2))  # Stuffing the returned vector.
  for (value in seq(from = 1, to = n - (window.size - 1))) {
    value.end <- value + (window.size - 1)
    out <- append(out, cor(x[value:value.end],
                           y[value:value.end],
                           method = method))
  }
  out <- append(out, rep(NA, n - length(out)))  # Finish stuffing.
  return(out)
}

slideCorr<-meanGDDs[which(meanGDDs$cluster=="Northeast"),]
MACorrs<-MovingCor(slideCorr$GDD50, slideCorr$GDD500, window.size=33, method="kendall")

plot(seq(1948,2016,1),MACorrs, type="l", ylim=c(0,1))

plot(slideCorr$year, slideCorr$GDD50, ylim=c(0,200), type="l")
lines(slideCorr$year, slideCorr$GDD500)
plot(slideCorr$GDD50, slideCorr$GDD500)
# ----