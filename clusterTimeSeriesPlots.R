# Cluster time series from zonal stats
# code taken from plot_TopoWx_GDD_classifications
# MAC 03/23/2018

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

# save classMap
load("./fixed/cluster15classMap.RData")
clusterN<-15
# load mask
maskNA<-raster("./fixed/maskNAalt.grd") 

# try zonal stats by cluster map
gdd50raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh50.grd")
  gdd50raw_x4 <- mask(gdd50raw_x4, maskNA)
gdd250raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh250.grd")
  gdd250raw_x4 <- mask(gdd250raw_x4, maskNA)
gdd450raw_x4<-stack("./fixed/X4_rawDOY_baseT10_thresh450.grd")
  gdd450raw_x4 <- mask(gdd450raw_x4, maskNA)
# set names
names(gdd50raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD50")
names(gdd250raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD250")
names(gdd450raw_x4)<-paste0(seq(1948, 2016, by=1),".GDD450")
allGddraw<-stack(gdd50raw_x4,gdd250raw_x4,gdd450raw_x4)
rm(gdd50raw_x4,gdd250raw_x4,gdd450raw_x4)

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
zStats<-as.data.frame(t(zonal(allGddraw, classMap, 'mean')))
zStats<-zStats[-1,]
zStats$yearLayer<-rownames(zStats)
zStats<-zStats %>% gather(yearLayer, 1:clusterN)
colnames(zStats)<-c("code","cluster","GDDValue")
zStats<-separate(zStats,code,c("X","code"), sep ="X")
zStats<-separate(zStats,code,c("year","threshold"))
zStats$year<-as.numeric(zStats$year)

ggplot(zStats, aes(x=year,y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line() +
  geom_smooth(method = "lm")+
  ylim(c(0,225))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY for GDD Thresholds by Cluster"),
       x ="Year", y = "Day of Year")

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
zStats$cluster<-factor(zStats$cluster, levels=c("Southwest","S Rockies","C Plains","N Rockies","Pac NW","Texas",
                                                  "TN Valley","S Plains","Midwest","N Plains","Florida","Gulf Coast","Southeast","OH Valley","Northeast"))
#zStats$cluster<-factor(zStats$cluster, levels=c("N Rockies","Upper Midwest","Northeast","Pacific NW","N Plains","Ohio Valley",
#                                                "S Rockies","C Plains","Southeast","Southwest","S Plains","Gulf Coast"))

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
library("broom")
 groupsLM <- zStats %>%
   group_by(cluster, threshold)
 GDDTrends<-do(groupsLM,glance(lm(GDDValue ~ year, data = .)))
 GDDSlopes<-do(groupsLM,tidy(lm(GDDValue ~ year, data = .)))
 GDDSlopes<-GDDSlopes[which(GDDSlopes$term=="year"),]
 GDDTrends<-merge(GDDTrends, GDDSlopes, by=c("cluster","threshold"))

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
GDDModel<-do(groupsLM,glance(lm(GDD450 ~ GDD50, data = .)))
#GDDModelTidy<-do(groupsLM,tidy(lm(GDD500 ~ GDD50, data = .)))
GDDModelresids<-do(groupsLM,augment(lm(GDD450 ~ GDD50, data = .)))
# add years
years<-as.data.frame(rep(seq(1948,2016,1), clusterN))
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
  labs(title=paste0("Resid of predicted GDD450 based on GDD50"),
       x ="Year", y = "Residual (Days)")+
  theme_bw()
# fit plots
ggplot(GDDModelresids, aes(x=GDD450+.resid, y=GDD450))+
  geom_point(size=0.75)+
  facet_wrap(~cluster, nrow=2)+
  xlim(50,250)+
  ylim(50,250)+
  geom_smooth(method = "lm")+
  coord_fixed()+
  labs(title="Resid of predicted GDD450 based on GDD50",
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
meanGDDs<-meanGDDs[,c(1,2,5,3,4)] #c(1,2,4,3,5)

# or just regular data
#meanGDDs<-spread(zStats,threshold,GDDValue)
#meanGDDs <- meanGDDs[-1]
#meanGDDs<-meanGDDs[,c(1,2,4,3,5)]


library(reshape2)
corrGDD <- meanGDDs %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(GDDValue, GDDValue))
  do(data.frame(Cor=t(cor(.[,3:5], .[,3:5], method="pearson")))) # [4:6] or [3:5]
labels<-as.data.frame(rep(c("Cor.GDD50y","Cor.GDD250y", "Cor.GDD450y"), clusterN)) #set number of clusters
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
corrGDD$corrLabels<-factor(corrGDD$corrLabels, levels=c("Cor.GDD50y","Cor.GDD250y","Cor.GDD450y"))

#corrGDD$cluster<-factor(corrGDD$cluster, levels=c("Southwest","S Rockies","Pacific NW","N Rockies","S Plains","C Plains",
#                                                  "N Plains","Upper Midwest","Gulf Coast","Southeast","Ohio Valley","Northeast"))
corrGDD$cluster<-factor(corrGDD$cluster, levels=c("Pac NW","N Plains","Northeast","N Rockies","Midwest","OH Valley","C Plains","S Plains","Southeast",
                                                  "S Rockies","TN Valley","Gulf Coast","Southwest","Texas","Florida"))
# ggplot heat map
p<- ggplot(corrGDD, aes(variable, corrLabels))+
  geom_tile(aes(fill = value),colour = "white")+
  geom_text(aes(x=variable, y=corrLabels, label = round(value, 2)), size=4)+
  scale_fill_gradient2(low="white", mid="yellow",high="red", midpoint = 0.5,
                       guide = guide_legend(title="Correlation"))+
  facet_wrap(~cluster,nrow=5)+
  ggtitle("1948-2016 GDD Correlations (mean GDDs, Pearson r)")
p+  theme(axis.text.x = element_text(angle = 330, hjust = 0))

# late period correlation 1981-2016
meanGDDs <- meanGDDs[ which(meanGDDs$year >= 1981),] # meanGDDs$year >= 1981
corrGDD <- meanGDDs %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(GDDValue, GDDValue))
  do(data.frame(Cor=t(cor(.[,3:5], .[,3:5], method="pearson"))))
labels<-as.data.frame(rep(c("Cor.GDD50y","Cor.GDD200y", "Cor.GDD500y"), clusterN))
colnames(labels)<-"corrLabels"
corrGDD<-cbind(labels,as.data.frame(corrGDD))
corrGDD<-melt(corrGDD)
corrGDDlate<-corrGDD
#corrGDDearly<-corrGDD

corrGDD$corrLabels<-factor(corrGDD$corrLabels, levels=c("Cor.GDD50y","Cor.GDD200y","Cor.GDD500y"))

corrGDD$cluster<-factor(corrGDD$cluster, levels=c("Pac NW","N Plains","Northeast","N Rockies","Midwest","OH Valley","C Plains","S Plains","Southeast",
                                                  "S Rockies","TN Valley","Gulf Coast","Southwest","Texas","Florida"))

p<- ggplot(corrGDD, aes(variable, corrLabels))+
  geom_tile(aes(fill = value),colour = "white")+
  geom_text(aes(x=variable, y=corrLabels, label = round(value, 2)), size=4)+
  scale_fill_gradient2(low="white", mid="yellow",high="red", midpoint = 0.5,
                       guide = guide_legend(title="Correlation"))+
  facet_wrap(~ cluster,nrow=5)+
  ggtitle("1981-2016 GDD Correlations (mean GDDs, Pearson r)")
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
labels<-as.data.frame(rep(c("GDD50","GDD250", "GDD450"), clusterN))
colnames(labels)<-"threshold"
corrGDD<-cbind(labels,as.data.frame(corrGDD))
statsGDD<-merge(df.sd.GDD, corrGDD, by=c("cluster","threshold"))
statsGDD<-statsGDD[which(statsGDD$threshold=="GDD50"),]
ggplot(statsGDD, aes(x=Cor.GDD450, y=meanGDD, color=sdGDD))+
  geom_point(size=4)+
  ggtitle("Corr50v450 vs Mean GDD50, region::GDD50 stdevs")+
  scale_color_gradient2(low="blue", mid="yellow",high="red", midpoint = 11,
                        guide = guide_legend(title="stdev"))

# scatterplot by region
meanGDDs$cluster<-factor(meanGDDs$cluster, levels=c("Pac NW","N Plains","Northeast","N Rockies","Midwest","OH Valley","C Plains","S Plains","Southeast",
                                                    "S Rockies","TN Valley","Gulf Coast","Southwest","Texas","Florida"))

ggplot(meanGDDs, aes(x=GDD50, y=GDD450, color=year))+
  geom_point(size=0.75)+
  facet_wrap(~cluster, nrow=2)+
  xlim(-40,40)+
  ylim(-40,40)+
  #geom_smooth(method = "lm")+
  #coord_fixed()+
  ggtitle("Detrended GDD50 by GDD450 - regions")+
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

# plot sDevs
ggplot(zStdev, aes(x=year,y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line()

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
  ylim(c(0,220))+
  background_grid(major = "xy", minor = "xy")+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("DOY for GDD Thresholds by Cluster"),
       x ="Year", y = "Day of Year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# geom_text(x = 1980, y = 200, 
#           aes(label = paste0("200 GDD SD: ", sdGDD)), 
#           data = df.sd.GDD, check_overlap = TRUE)

# early/late boxplots
ggplot(zStats, aes(x=as.factor(period),y=GDDValue, color=factor(threshold))) +
  facet_wrap(~cluster, nrow = 1)+
  geom_boxplot()+
  theme_bw()+
  scale_color_brewer(name ="GDD Threshold",palette = "Set1")+
  labs(title=paste0("Early (48-80) vs Late (81-16) GDD stats"),
       x ="Period", y = "Day of Year")

# diff in GDD500-GDD50
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]
meanGDDs$diff500_50<-meanGDDs$GDD450-meanGDDs$GDD50
ggplot(meanGDDs, aes(x=year,y=diff500_50)) +
  facet_wrap(~cluster, nrow = 1)+
  geom_line()+
  geom_smooth(method="lm")+
  labs(title=paste0("Mean GDD450-GDD50 DOY by year"),
       x ="Year", y = "Days")+
  theme_bw()
# check slope of diffs
 library("broom")
  groupsLM <- meanGDDs %>%
    group_by(cluster)
  GDDTrends<-do(groupsLM,glance(lm(year ~ diff500_50, data = .)))
  GDDSlopes<-do(groupsLM,tidy(lm(year ~ diff500_50, data = .)))
  GDDSlopes<-GDDSlopes[which(GDDSlopes$term=="diff500_50"),]
  GDDTrends<-merge(GDDTrends, GDDSlopes, by=c("cluster","threshold"))


# get sdevs for each cluster
df.sd.GDD <- zStdev %>%
  group_by(cluster) %>% # add in threshold
  summarise(sdGDD = round(sd(GDDValue), 2))

#showTmpFiles()
#removeTmpFiles(h=0) # in hours

# zonal stats for clusters ----
# get some data 
elev<-raster("./mapFiles/X4_elev.grd")
meanDOY50_x4<-stack("./fixed/X4_meanDOY_baseT10_thresh50_1981-2010.grd")
  meanDOY50_x4 <- mask(meanDOY50_x4, maskNA)
meanDOY250_x4<-stack("./fixed/X4_meanDOY_baseT10_thresh250_1981-2010.grd")
  meanDOY250_x4 <- mask(meanDOY250_x4, maskNA)
meanDOY450_x4<-stack("./fixed/X4_meanDOY_baseT10_thresh450_1981-2010.grd")
  meanDOY450_x4 <- mask(meanDOY450_x4, maskNA)
# need to resample to X4
#normTmin<-stack("./mapFiles/normals_tmin.nc", varname="tmin_normal")
#normTmax<-stack("./mapFiles/normals_tmax.nc", varname="tmax_normal")

clusterStats<-as.data.frame(cbind(classMap@data@attributes[[1]][2],freq(classMap, useNA="no"),zonal(elev, classMap, 'mean'),zonal(elev, classMap, 'sd'),
                                  zonal(meanDOY50_x4, classMap, 'mean'),zonal(meanDOY50_x4, classMap, 'sd'),
                                  zonal(meanDOY250_x4, classMap, 'mean'),zonal(meanDOY250_x4, classMap, 'sd'),
                                  zonal(meanDOY450_x4, classMap, 'mean'),zonal(meanDOY450_x4, classMap, 'sd')))

clusterStats <- clusterStats[c(-2,-4,-6,-8,-10,-12,-14,-16,-18)]
colnames(clusterStats)<-c("cluster","#pixels","meanElev","sdevElev","doy50mean","doy50sdev","doy250mean","doy250sdev","doy450mean","doy450sdev")
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


# plot correlation choropleth map
# 50 vs 450
library(colorRamps)
clusterpoly <- readShapePoly("./mapFiles/cluster15finalFIXED_ZProd")
clusterpoly@data$names<-(c("TN Valley","Northeast","Midwest","C Plains",
                          "Southwest","S Rockies","Southeast","N Plains",
                          "N Rockies","Pac NW","OH Valley","Florida",
                          "Texas","S Plains","Gulf Coast"))
corrGDDSubset<-corrGDD[which(corrGDD$corrLabels=='Cor.GDD50y' 
                       & corrGDD$variable=='Cor.GDD450'),]
corrGDDSubset$cluster<-as.character(corrGDDSubset$cluster)
corrGDDSubset<-corrGDDSubset[c(-1,-3)]
clusterpoly@data<-merge(clusterpoly@data,corrGDDSubset, by.x = "names", by.y = "cluster")
target <- c("TN Valley","Northeast","Midwest","C Plains",
            "Southwest","S Rockies","Southeast","N Plains",
            "N Rockies","Pac NW","OH Valley","Florida",
            "Texas","S Plains","Gulf Coast")
clusterpoly@data<-clusterpoly@data[match(target, clusterpoly@data$names),]

spplot(clusterpoly,"value",col.regions=colorRampPalette(c("yellow","orange", "red"))(13),  at=seq(0.4, 1, 0.05),main=list(label="GDD50v450 Correlation (r)",cex=1))


# labels <- as.character(clusterpoly@data$names)
# locs.labels <- getSpPPolygonsLabptSlots(clusterpoly)
# nc.labels <- maptools::pointLabel(x=locs.labels[,1],y=locs.labels[,2],labels=labels,doPlot=FALSE)
# 
# nc.labels.white.panel <- list("panel.text",nc.labels$x-0.005,nc.labels$y-0.005,labels=labels,col="white",cex=0.5)
# nc.labels.panel <- list("panel.text",nc.labels$x,nc.labels$y,labels=labels,col="black",cex=0.5)
# #spplot(nc, 'AREA', sp.layout=list(nc.labels.white.panel,nc.labels.panel), col.regions=brewer.pal(7, "Reds"), cuts=6)
# spplot(clusterpoly,"value",sp.layout=list(nc.labels.white.panel,nc.labels.panel),
#        col.regions=colorRampPalette(c("yellow","orange", "red"))(13),
#        at=seq(0.4, 1, 0.05),main=list(label="GDD50v450 Correlation (r)",cex=1))

# 250 vs 450
clusterpoly <- readShapePoly("./mapFiles/cluster15finalFIXED_ZProd")
clusterpoly@data$names<-(c("TN Valley","Northeast","Midwest","C Plains",
                           "Southwest","S Rockies","Southeast","N Plains",
                           "N Rockies","Pac NW","OH Valley","Florida",
                           "Texas","S Plains","Gulf Coast"))
corrGDDSubset<-corrGDD[which(corrGDD$corrLabels=='Cor.GDD250y' 
                             & corrGDD$variable=='Cor.GDD450'),]
corrGDDSubset$cluster<-as.character(corrGDDSubset$cluster)
corrGDDSubset<-corrGDDSubset[c(-1,-3)]
clusterpoly@data<-merge(clusterpoly@data,corrGDDSubset, by.x = "names", by.y = "cluster")
target <- c("TN Valley","Northeast","Midwest","C Plains",
            "Southwest","S Rockies","Southeast","N Plains",
            "N Rockies","Pac NW","OH Valley","Florida",
            "Texas","S Plains","Gulf Coast")
clusterpoly@data<-clusterpoly@data[match(target, clusterpoly@data$names),]

spplot(clusterpoly,"value",col.regions=colorRampPalette(c("yellow","orange", "red"))(13),  at=seq(0.4, 1, 0.05),main=list(label="GDD250v450 Correlation (r)",cex=1))

# 50 vs 250
clusterpoly <- readShapePoly("./mapFiles/cluster15finalFIXED_ZProd")
clusterpoly@data$names<-(c("TN Valley","Northeast","Midwest","C Plains",
                           "Southwest","S Rockies","Southeast","N Plains",
                           "N Rockies","Pac NW","OH Valley","Florida",
                           "Texas","S Plains","Gulf Coast"))
corrGDDSubset<-corrGDD[which(corrGDD$corrLabels=='Cor.GDD50y' 
                             & corrGDD$variable=='Cor.GDD250'),]
corrGDDSubset$cluster<-as.character(corrGDDSubset$cluster)
corrGDDSubset<-corrGDDSubset[c(-1,-3)]
clusterpoly@data<-merge(clusterpoly@data,corrGDDSubset, by.x = "names", by.y = "cluster")
target <- c("TN Valley","Northeast","Midwest","C Plains",
            "Southwest","S Rockies","Southeast","N Plains",
            "N Rockies","Pac NW","OH Valley","Florida",
            "Texas","S Plains","Gulf Coast")
clusterpoly@data<-clusterpoly@data[match(target, clusterpoly@data$names),]

spplot(clusterpoly,"value",col.regions=colorRampPalette(c("yellow","orange", "red"))(13),  at=seq(0.4, 1, 0.05),main=list(label="GDD50v250 Correlation (r)",cex=1))


