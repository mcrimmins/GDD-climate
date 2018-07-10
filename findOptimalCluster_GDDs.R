# find optimal clusters from GDD anom z-scores
# code taken from plot_TopoWx_GDD_classifications.R
# MAC 03/23/18

#library(cowplot)
library(raster)
library(rasterVis)
#library(sp)
#library(maptools)
library(RStoolbox)
#library(tidyr)
#library(dplyr)
library(parallel)

# set rasteroptions
rasterOptions(progress = 'text')
tmpDir(create=TRUE)
# rsToolbox options
rsOpts(verbose=TRUE)

# get state boundaries
states <- getData('GADM', country='United States', level=1)

# clustering code ----
# ---- Plot Mean DOYs
meanDOY50_x4<-raster("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
meanDOY50_x4 <- mask(meanDOY50_x4, maskNA)

# # # or anoms
gdd50_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh50_1981-2010.grd")
gdd250_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh250_1981-2010.grd")
gdd450_x4<-stack("./fixed/X4_anomDOY_baseT10_thresh450_1981-2010.grd")

# standardize grids with z-scores
zscore<-function(x){scale(x, center = TRUE, scale = TRUE)}
ptm <- proc.time()
beginCluster(6)
  gdd50_x4 <- clusterR(gdd50_x4, calc, args=list(fun=zscore))
  gdd250_x4 <- clusterR(gdd250_x4, calc, args=list(fun=zscore))
  gdd450_x4 <- clusterR(gdd450_x4, calc, args=list(fun=zscore))
endCluster()
proc.time() - ptm  

# mask out   
maskNA<-raster("./fixed/maskNAalt.grd") 
gdd50_x4 <-  mask(gdd50_x4, maskNA)  
gdd250_x4 <- mask(gdd250_x4, maskNA)
gdd450_x4 <- mask(gdd450_x4, maskNA)

# set names
names(gdd50_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")

# check standardized anom plots
#my.at <- seq(-4, 4, 0.1)
#levelplot(gdd50_x4[[60:69]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="GDD50 Anomalies - zscore")+ 
#  layer(sp.polygons(states))

# combine into stack
allGdd<-stack(gdd50_x4,gdd250_x4,gdd450_x4)
#allGdd<-stack(gdd50_x4,gdd450_x4)
rm(gdd50_x4,gdd250_x4,gdd450_x4)

# # RAW DATA for clustering
# maskNA<-raster("./fixed/maskNAalt.grd") 
# gdd50raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh50.grd")
# gdd50raw_x4 <- mask(gdd50raw_x4, maskNA)
# gdd250raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh250.grd")
# gdd250raw_x4 <- mask(gdd250raw_x4, maskNA)
# gdd450raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh450.grd")
# gdd450raw_x4 <- mask(gdd450raw_x4, maskNA)
# # set names
# names(gdd50raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
# names(gdd250raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
# names(gdd450raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")
# allGddraw<-stack(gdd50raw_x4,gdd250raw_x4,gdd450raw_x4)
# rm(gdd50raw_x4,gdd250raw_x4,gdd450raw_x4)


# load zDiff grids
#load("./fixed/zscore50_450gddStats.RData")
#allGdd<-stack(diff450_50,gdd50_x4)

# load zProd grids
load("./fixed/Prodzscore50_450gddStats.RData")
# lat lon grids
 lonGrid <- normImage(init(prod450_50, 'x'),norm = TRUE)
 latGrid <- normImage(init(prod450_50, 'y'),norm = TRUE)
#diff450_50<-stack(diff450_50,lonGrid,latGrid)
 # set names
 names(prod450_50)<-paste0(seq(1948, 2016, by=1),".PRODz50_450")
allGdd<-stack(prod450_50,3*lonGrid,3*latGrid)
rm(prod450_50)
 
# # # # find optimal cluster number
# clusterN=20
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
# # save cluster diag vars
#  save(clusterN,wss,minwss,totss,btwss,
#       file="./fixed/zProd_FindClusterDiagnostics.RData")
# #load("./fixed/zProd_FindClusterDiagnostics.RData")
# #  
  plot(2:clusterN, wss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms z-score",
         ylab="Within groups sum of squares", ylim=c(min(wss[2:clusterN]),max(wss[2:clusterN])))
  plot(3:clusterN, diff(wss[2:clusterN])*-1, type="b", xlab="Number of Clusters - GDD Anoms z-score",
      ylab="Diff Within groups sum of squares")#, ylim=c(min(diff(wss[2:clusterN])),max(diff(wss[2:clusterN]))))
 plot(2:clusterN, btwss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms z-score",
      ylab="Between groups sum of squares",  ylim=c(min(btwss[2:clusterN]),max(btwss[2:clusterN])))
 plot(2:clusterN, minwss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms z-score",
      ylab="Min groups sum of squares",  ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
 plot(3:clusterN, diff(minwss[2:clusterN])*-1, type="b", xlab="Number of Clusters - GDD Anoms z-score",
      ylab="Diff Min groups sum of squares")#,ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
 plot(2:clusterN, btwss[2:clusterN]/totss[2:clusterN], type="b", xlab="Number of Clusters - GDD Anoms z-score",
     ylab="BSS/TSS Ratio")#,  ylim=c(min(totss[2:clusterN]),max(totss[2:clusterN])))

# save cluster results for plotting later...
#load("./fixed/zscore50_450gddStats.RData")
# lat lon grids
# lonGrid <- init(diff450_50, 'x')
# latGrid <- init(diff450_50, 'y')
#diff450_50<-stack(diff450_50,lonGrid,latGrid)
 
# cluster data
ptm <- proc.time()
set.seed(1234)
  clusterN<-15
  unC <- unsuperClass(allGdd, nSamples = 250000, nClasses = clusterN, nStarts = 25, nIter = 1000, norm = FALSE) # or allGDD
proc.time() - ptm


# # trying out CLARA
# library(cluster)
# v <- getValues(allGdd)
# i <- which(!is.na(v))
# v <- na.omit(v)
# set.seed(1234)
# clusterN<-15
# clus <- clara(v,clusterN,samples=50,metric="euclidean",pamLike=T, rngR = FALSE, trace = 1)
# clara_raster <- allGdd[[1]]
# clara_raster[i] <- clus$clustering
# classMap<-as.factor(clara_raster)

# PLOT CLUSTER MAP ----
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
rat[["cluster"]]<-c("TN Valley","N Plains","N Rockies","Pac NW","OH Valley","Florida",
                    "Texas","S Plains","Gulf Coast","Northeast","Midwest","C Plains",
                    "Southwest","S Rockies","Southeast")

#rat[["cluster"]]<-c("N Plains","Gulf Coast","Ohio Valley","Upper Midwest","Southeast","Southwest",
#                    "C Plains","S Plains","N Rockies","Pacific NW","S Rockies","Northeast")
# rat[["cluster"]]<-c("S Rockies","N Rockies","Northeast","Pacific NW","S Plains",
#                     "C Plains","N Plains","Southeast","Ohio Valley","Upper Midwest",
#                     "Gulf Coast","Southwest")
#                   
#rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 

# plot classified map
levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
          margin=FALSE, main="50*450 GDD Product Z-score Anom clustering (3*lat/lon)")+
  layer(sp.polygons(states))+
  layer(sp.polygons(clusterpoly)) 

# save classMap
#save(classMap, file="./fixed/cluster8classMap.RData")
#save(classMap,unC,clusterN, file="./fixed/cluster15classMap.RData")
#load("./fixed/cluster15classMap.RData")

# CLUSTER Diagnostics
#cluster correlations
library(corrplot)
centers<-as.data.frame(t(unC$model$centers))
res <- cor(centers, method = "pearson")
corrplot(res, type = "upper", order = "original",
         tl.col = "black", tl.srt = 45, title="Intercluster Pearson Corrs")

# # cluster histograms
# # approach 3
# z <- layerize(classMap, falseNA=TRUE)
# sz <- z[[9]] * gdd50_x4
# x <- hist(sz) #hist(sz, breaks=c(0,5,15,30,45,65))
# f3 <- sapply(x, function(i) i$counts)
# 
# boxplot(gdd50_x4,classMap)

# # plot withinss
#  library(ggplot2)
# clusterNames<-c("N Plains","Gulf Coast","Ohio Valley","Upper Midwest","Southeast","Southwest",
#                 "C Plains","S Plains","N Rockies","Pacific NW","S Rockies","Northeast")
# withinss<-as.data.frame(cbind(seq(1,nrow(unC$model$centers),1),unC$model$withinss))
# colnames(withinss)<-c("cluster","error")
# withinss$names<-clusterNames
# p <-ggplot(withinss, aes(cluster, error))
# p +geom_bar(stat = "identity")+
#   scale_x_discrete(limits=as.character(seq(1,nrow(unC$model$centers),1)),labels=clusterNames)+
#   labs(title="Within Sum of Squares Error by Cluster")


# ---- plot 
library(tidyr)
library(dplyr)
clusterN=12
# get zonal sdev
for (i in seq(from=1, to=207, by=3)){
  j=i+2
  temp<-zonal(allGdd[[i:j]], classMap, 'sd')
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

# # get zonal mean of each cluster
zStats<-as.data.frame(t(zonal(allGdd, classMap, 'mean')))
zStats<-zStats[-1,]
zStats$yearLayer<-rownames(zStats)
zStats<-zStats %>% gather(yearLayer, 1:clusterN)
colnames(zStats)<-c("code","cluster","GDDValue")
zStats<-separate(zStats,code,c("X","code"), sep ="X")
zStats<-separate(zStats,code,c("year","threshold"))
zStats$year<-as.numeric(zStats$year)

# get sdev intervals
zStats$sdevPos<-zStats$GDDValue+zStdev$GDDValue
zStats$sdevNeg<-zStats$GDDValue-zStdev$GDDValue

# add ClusterNames
clusterNames<-c("N Plains","Gulf Coast","Ohio Valley","Upper Midwest","Southeast","Southwest",
                "C Plains","S Plains","N Rockies","Pacific NW","S Rockies","Northeast")
nameLUP<-as.data.frame(cbind(paste0("V",as.character(seq(1, clusterN, by=1))),c("N Plains","Gulf Coast","Ohio Valley","Upper Midwest","Southeast","Southwest",
                                                                                "C Plains","S Plains","N Rockies","Pacific NW","S Rockies","Northeast"))) 
# lookup table to add in names
new<-zStats
new[] <- nameLUP$V2[match(unlist(zStats), nameLUP$V1)]
zStats$cluster<-new$cluster
# factor order 
zStats$cluster<-factor(zStats$cluster, levels=c("Southwest","S Rockies","Pacific NW","N Rockies","S Plains","C Plains",
                                                "N Plains","Upper Midwest","Gulf Coast","Southeast","Ohio Valley","Northeast"))
#zStats$cluster<-factor(zStats$cluster, levels=c("N Rockies","Upper Midwest","Northeast","Pacific NW","N Plains","Ohio Valley",
#                                                "S Rockies","C Plains","Southeast","Southwest","S Plains","Gulf Coast"))


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
library(cowplot)
ggplot(zStats, aes(x=year,y=GDDValue)) +
  facet_wrap(~factor(threshold)+cluster, nrow = 3)+
  geom_line(color='red')+
  #geom_line(aes(y=rollmean(GDDValue, 5, na.pad=TRUE)))+
  #geom_smooth(method = "lm")+
  geom_ribbon(aes(ymax = sdevPos, ymin = sdevNeg,linetype=NA), alpha = 0.3)+
  #ylim(c(-5,205))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY zscore for GDD Thresholds by Cluster"),
       x ="Year", y = "z-score")
# geom_text(x = 1980, y = 200, 
#           aes(label = paste0("200 GDD SD: ", sdGDD)), 
#           data = df.sd.GDD, check_overlap = TRUE)


# ---- zProduct time series by cluster
# load zProd grids
load("./fixed/Prodzscore50_450gddStats.RData")
load("./fixed/cluster15classMap.RData")
names(prod450_50)<-paste0(seq(1948, 2016, by=1),".PRODz50_450")
allGdd<-prod450_50
rm(prod450_50)

# ---- plot 
library(tidyr)
library(dplyr)
clusterN=15
# get zonal sdev
# for (i in seq(from=1, to=207, by=3)){
#   j=i+2
#   temp<-zonal(allGdd[[i:j]], classMap, 'sd')
#   if (i==1){
#     zStdev <- temp
#   }else{
#     zStdev <- cbind(zStdev, temp[,2:4]) # bind
#   }
# }

zStdev<-zonal(allGdd, classMap, 'sd')

zStdev<-as.data.frame(t(zStdev))
zStdev<-zStdev[-1,]
zStdev$yearLayer<-rownames(zStdev)
zStdev<-zStdev %>% gather(yearLayer, 1:clusterN)
colnames(zStdev)<-c("code","cluster","GDDValue")
zStdev<-separate(zStdev,code,c("X","code"), sep ="X")
zStdev<-separate(zStdev,code,c("year","threshold"))
zStdev$year<-as.numeric(zStdev$year)

# # get zonal mean of each cluster
zStats<-as.data.frame(t(zonal(allGdd, classMap, 'mean')))
zStats<-zStats[-1,]
zStats$yearLayer<-rownames(zStats)
zStats<-zStats %>% gather(yearLayer, 1:clusterN)
colnames(zStats)<-c("code","cluster","GDDValue")
zStats<-separate(zStats,code,c("X","code"), sep ="X")
zStats<-separate(zStats,code,c("year","threshold"))
zStats$year<-as.numeric(zStats$year)

# get sdev intervals
zStats$sdevPos<-zStats$GDDValue+zStdev$GDDValue
zStats$sdevNeg<-zStats$GDDValue-zStdev$GDDValue

# add ClusterNames
clusterNames<-c("TN Valley","N Plains","N Rockies","Pac NW","OH Valley","Florida",
                "Texas","S Plains","Gulf Coast","Northeast","Midwest","C Plains",
                "Southwest","S Rockies","Southeast")
nameLUP<-as.data.frame(cbind(paste0("V",as.character(seq(1, clusterN, by=1))),c("TN Valley","N Plains","N Rockies","Pac NW","OH Valley","Florida",
                                                                                "Texas","S Plains","Gulf Coast","Northeast","Midwest","C Plains",
                                                                                "Southwest","S Rockies","Southeast"))) 
# lookup table to add in names
new<-zStats
new[] <- nameLUP$V2[match(unlist(zStats), nameLUP$V1)]
zStats$cluster<-new$cluster
# factor order 
zStats$cluster<-factor(zStats$cluster, levels=c("Pac NW","N Plains","Northeast","N Rockies","Midwest","OH Valley","C Plains","S Plains","Southeast",
                                                "S Rockies","TN Valley","Gulf Coast","Southwest","Texas","Florida"))
#zStats$cluster<-factor(zStats$cluster, levels=c("N Rockies","Upper Midwest","Northeast","Pacific NW","N Plains","Ohio Valley",
#                                                "S Rockies","C Plains","Southeast","Southwest","S Plains","Gulf Coast"))

library("broom")
groupsLM <- zStats %>%
  group_by(cluster)
GDDTrends<-do(groupsLM,glance(lm(GDDValue ~ year, data = .)))
GDDSlopes<-do(groupsLM,tidy(lm(GDDValue ~ year, data = .)))
GDDSlopes<-GDDSlopes[which(GDDSlopes$term=="year"),]
GDDTrends<-merge(GDDTrends, GDDSlopes, by=c("cluster","threshold"))

# rolling mean option
#library(zoo)
library(cowplot)
ggplot(zStats, aes(x=year,y=GDDValue)) +
  facet_wrap(~cluster, nrow = 5)+
  geom_line(color='red')+
  #geom_line(aes(y=rollmean(GDDValue, 5, na.pad=TRUE)))+
  geom_smooth(method = "lm")+
  geom_ribbon(aes(ymax = sdevPos, ymin = sdevNeg,linetype=NA), alpha = 0.3)+
  ylim(c(-1,8))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  geom_hline(yintercept=0)+
  labs(title=paste0("Yearly Z-score 450*50 product by Cluster"),
       x ="Year", y = "z-score product")
# geom_text(x = 1980, y = 200, 
#           aes(label = paste0("200 GDD SD: ", sdGDD)), 
#           data = df.sd.GDD, check_overlap = TRUE)

