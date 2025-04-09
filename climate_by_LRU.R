# assess SPI/SPEI climate time series relative to RPMS/smNDVI
# code borrowed from analyzeTimeSeries.R and analyze_by_LRU.R
# use results from analyze_by_LRU.R
# MAC 04/20/2020

library(raster)
library(tidyr)
library(SPEI)
library(cowplot)
library(reshape2)

# set rasteroptions
rasterOptions(progress = 'text')

# load results from analyze_by_LRU.R
load(file="./results/SW_CRA_rpms_gridmet_SPI_SPEI_results.Rdata")

# load grids - all diff resolutions and extents
rpmsSW<-stack("/scratch/crimmins/USDA_NPP/v2019/AZNM_RPMS_8419_WGS84.grd")
# monthly gridmet, orig res
precip<-stack("/scratch/crimmins/gridmet/update_Aug2019/processed/SWUS_gridmet_monthly_sum_precip_1979_2019.grd")
pet<-stack("/scratch/crimmins/gridmet/update_Aug2019/processed/SWUS_gridmet_monthly_sum_pet_1979_2019.grd")
# max smNVDI
smNDVImax<-stack("/scratch/crimmins/vhi/processed/ANNUAL_MAX_smNDVI_complete_1982-2019_SWUS.grd") 
# weekly smNDVI
smNDVI<-stack('/scratch/crimmins/vhi/processed/smNDVI_complete_1982-2019_SWUS.grd')
# timing of max NDVI
whenMaxNDVI<-stack('/scratch/crimmins/vhi/processed/ANNUAL_MAX_timing_smNDVI_complete_1982-2019_SWUS.grd')
# gridded US Drought Monitor
load("~/RProjects/USDM/SW_USDMRaster_2000_2019_4km.RData")
USDM<-tempGrid2
rm(tempGrid2)

# USGS Pheno dataset
USGSmax<-stack('/scratch/crimmins/USGS_Pheno/processed/av_MAXN_1989-2014_SWUS.grd')

# plot individual polygons
#  plot(LMU);plot(LMU[22,], add=TRUE, border='red')
# subset poly
  poly<-subset(LMU, CRA=="40.3")
  plot(LMU)
   plot(poly, add=TRUE, border='red')
     plot(poly[1,])
     poly<-poly[1,]
   plot(LMU)
   plot(poly, add=TRUE, border='red')
    
# summary stat ts for LRU mean or median - or return all data points for analysis of within poly variability
beginCluster(n=6)
  rpmsTS<-raster::extract(rpmsSW, poly, fun=median, df=TRUE, na.rm=TRUE)
    rpmsTS<-as.data.frame(t(rpmsTS[,-1]))
  precipTS<-raster::extract(precip, poly, fun=median, df=TRUE, na.rm=TRUE)
    precipTS<-as.data.frame(t(precipTS[,-1]))
  smNDVImaxTS<-raster::extract(smNDVImax, poly, fun=median, df=TRUE, na.rm=TRUE)
    smNDVImaxTS<-as.data.frame(t(smNDVImaxTS[,-1]))
  smNDVITS<-raster::extract(smNDVI, poly, fun=median, df=TRUE, na.rm=TRUE)
    smNDVITS<-as.data.frame(t(smNDVITS[,-1]))
  petTS<-raster::extract(pet, poly, fun=median, df=TRUE, na.rm=TRUE)
    petTS<-as.data.frame(t(petTS[,-1]))
  usdmTS<-raster::extract(USDM, poly, fun=median, df=TRUE, na.rm=TRUE)
    usdmTS<-as.data.frame(t(usdmTS[,-1]))
  whenMaxTS<-raster::extract(whenMaxNDVI, poly, fun=median, df=TRUE, na.rm=TRUE)
    whenMaxTS<-as.data.frame(t(whenMaxTS[,-1]))  
  USGSMaxTS<-raster::extract(USGSmax, poly, fun=median, df=TRUE, na.rm=TRUE)
    USGSMaxTS<-as.data.frame(t(USGSMaxTS[,-1])) 
endCluster()  

# add dates to time series
rpmsTS$date<-seq(as.Date("1984-12-01", "%Y-%m-%d"), as.Date("2019-12-01", "%Y-%m-%d"), by="years")
smNDVImaxTS$date<-seq(as.Date("1982-12-01", "%Y-%m-%d"), as.Date("2019-12-01", "%Y-%m-%d"), by="years")
whenMaxTS$date<-seq(as.Date("1982-12-01", "%Y-%m-%d"), as.Date("2019-12-01", "%Y-%m-%d"), by="years")
precipTS$date<-seq(as.Date("1979-01-01", "%Y-%m-%d"), as.Date("2019-12-01", "%Y-%m-%d"), by="months")
petTS$date<-seq(as.Date("1979-01-01", "%Y-%m-%d"), as.Date("2019-12-01", "%Y-%m-%d"), by="months")
USGSMaxTS$date<-seq(as.Date("1989-12-01", "%Y-%m-%d"), as.Date("2014-12-01", "%Y-%m-%d"), by="years")
# smNDVI dates
  dates<-as.data.frame(names(smNDVI))
  colnames(dates)<-"dateCode"
  dates<-separate(dates,dateCode,c("X","date"), sep ="X")
    smNDVITS$date<-as.Date(dates$date, format="%Y.%m.%d")
# USDM dates
  usdmTS$date<-fileNames$date  
       
# weighted mean climate dataframe
  moClimData<-cbind.data.frame(precipTS$date, precipTS$V1, petTS$V1)
    moClimData$precip3mo<-movingFun(moClimData$`precipTS$V1`, 3,type = "to", sum)
    moClimData$pet3mo<-movingFun(moClimData$`petTS$V1`, 3,type = "to", sum)
    moClimData$SPI3<-spi(moClimData$`precipTS$V1`, 3)$fitted  
    moClimData$SPEI3<-spei(moClimData$`precipTS$V1`- moClimData$`petTS$V1`, 3)$fitted
    moClimData$month<-as.numeric(format(moClimData$`precipTS$date`, format="%m"))
    moClimData$year<-as.numeric(format(moClimData$`precipTS$date`, format="%Y"))

# trim all datasets to common period of record 1984-2019
    moClimData<-subset(moClimData, moClimData$`precipTS$date`>=as.Date("1984-01-01", format="%Y-%m-%d"))
    smNDVImaxTS<-subset(smNDVImaxTS, date>="1984-01-01")
    smNDVITS<-subset(smNDVITS, date>="1984-01-01")
    whenMaxTS<-subset(whenMaxTS, date>="1984-01-01") 
    
# compare rpms and smNDVI 
plot(rpmsTS$V1,smNDVImaxTS$V1)
cor(rpmsTS$V1,smNDVImaxTS$V1, method="spearman")
plot(rpmsTS$V1,whenMaxTS$V1)
cor(rpmsTS$V1,whenMaxTS$V1, method="spearman")

# compare all RS datasets
library(PerformanceAnalytics)
rsData<-merge(USGSMaxTS,merge(rpmsTS, smNDVImaxTS, by="date"), by="date")
colnames(rsData)<-c("Date","USGS","RPMS","smNDVI")
  chart.Correlation(rsData[,2:4], histogram=TRUE, pch=19) 

# rpms anomalies
rpmsTS$anom<-rpmsTS$V1-mean(rpmsTS$V1)
smNDVImaxTS$anom<-smNDVImaxTS$V1-mean(smNDVImaxTS$V1)
    
# plots ----
p<-ggplot(moClimData, aes(moClimData$`precipTS$date`,moClimData$SPI3))+
  geom_line(color='orange')
p<-p+geom_line(data=moClimData, aes(moClimData$`precipTS$date`,moClimData$SPEI3), color='red')+
  geom_hline(yintercept = 0)

temp<-subset(moClimData, month==1 | month==1) # key 3 month periods for productivity
p<-p+geom_line(data=temp, aes(x=temp$'precipTS$date',y=temp$SPEI3))+
  xlab("SPI/SPEI")+
  ylab("date")+
  ggtitle("NCRS CRA 35.3 NW AZ - monthly and censored (NDJ) SPI/SPEI, USDM"  )

p<-p+geom_bar(data=usdmTS, aes(x=date, y=-1*(V1+1)), stat='identity', alpha = 0.4)    

pRPMS<-ggplot(rpmsTS, aes(date,anom))+
  geom_bar(stat='identity', fill="brown")+
  ggtitle("RPMS Production Anomaly (lbs/ac)")

pNDVImax<-ggplot(smNDVImaxTS, aes(date,anom))+
  geom_bar(stat='identity', fill="green")+
  ggtitle("Max-NDVI Anomaly (unitless)")

pWhenmax<-ggplot(whenMaxTS, aes(date,V1))+
  geom_bar(stat='identity', fill="purple")+
  ggtitle("Week of Max-NDVI Anomaly")

pNDVI<-ggplot(smNDVITS, aes(smNDVITS$date,V1))+
  geom_line(color='green')

plot_grid(p, pRPMS, pNDVImax,pWhenmax,pNDVI ,ncol = 1, align = 'v',rel_heights = c(2,1,1,1,1))
# ----


# all subsets to build models with all seasons
# SPI = 6, SPEI=7
allSeas<-moClimData[,c(8,9,6)]
allSeas<-dcast(allSeas, year~month)
colnames(allSeas)<-c("year",paste0(month.abb[11],"_",month.abb[1]),paste0(month.abb[12],"_",month.abb[2]),
                     paste0(month.abb[seq(1,12-2,1)],"_",month.abb[seq(1+2,12,1)]))
# dataframe for regressions 
trimSeas<-subset(allSeas, year>=1984)
trimSeas$rpms<-rpmsTS$V1
#trimSeas$rpms<-smNDVImaxTS$V1

# quantreg ----
library(quantreg)
qs <- 1:9/10
qr1 <- rq(rpms ~ Jul_Sep, data=trimSeas, tau = 0.25)
# pseudo r2
fit0 <- rq(rpms~1,tau=0.5,data=trimSeas)
fit1 <- rq(rpms~Jul_Sep+Oct_Dec,tau=0.5,data=trimSeas)

rho <- function(u,tau=.5)u*(tau - (u < 0))
R1 <- 1 - fit1$rho/fit0$rho
# ----

# lm regression diagnostics ----
library(performance)
library(lindia)

model<-lm(rpms~Jul_Sep+Oct_Dec,data=trimSeas)
summary(model)

cor(trimSeas$Jul_Sep,trimSeas$Oct_Dec) # check corr of predictors
qplot(trimSeas$Jul_Sep,trimSeas$Oct_Dec)
qplot(trimSeas$rpms,trimSeas$Oct_Dec)
qplot(trimSeas$rpms,trimSeas$Jul_Sep)


check_model(model) # performance
gg_diagnose(model) # lindia
model_performance(model) # performance
# plot time series plot
# add 'fit', 'lwr', and 'upr' columns to dataframe (generated by predict)
temp<-trimSeas[,c(10,13,14)]
temp.predict <- cbind(temp, predict(model, interval = 'confidence'))

# plot the points (actual observations), regression line, and confidence interval
p <- ggplot(temp.predict, aes(Jul_Sep,rpms,color=Oct_Dec))
p <- p + geom_point()
p <- p + geom_line(aes(Jul_Sep, rpms))
p + geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.3)+
  ggtitle("41-3AZ Annual Production (RPMS)v JAS-SPI3, nClimGrid")
# time series
p<-ggplot(temp.predict, aes(as.Date(temp.predict$`precipTS$date`),rpms))+
  geom_line()
p+geom_line(aes(as.Date(temp.predict$`precipTS$date`),fit), color='red')+
  ggtitle("41-3AZ RPMS Production and JAS-SPI3 nClimGrid (obs=black, pred=red)")
ggplot(temp.predict, aes(as.Date(temp.predict$`precipTS$date`),rpms-fit))+
  geom_line()+
  ggtitle("41-3AZ RPMS Production by JAS-SPI3 nClimGrid model  (obs-pred)")+
  geom_hline(yintercept=0)
# ----

# compare smNDVI and rpms drought classifications ----
# library(gtools)
#   droughts<-cbind.data.frame(rpmsTS$date,rpmsTS$V1,smNDVImaxTS$V1)
#   droughts$rpms<- quantcut(droughts$`rpmsTS$V1`, c(0,0.03,0.06,0.11,0.21,0.30,1)) # USDM percentiles
#     levels(droughts$rpms) <- c("D4","D3","D2","D1","D0","No Drought")
#   droughts$smNDVI<- quantcut(droughts$`smNDVImaxTS$V1`, c(0,0.03,0.06,0.11,0.21,0.30,1)) # USDM percentiles
#     levels(droughts$smNDVI) <- c("D4","D3","D2","D1","D0","No Drought")
# ----
  
# ordinal regression with drought categories
library(gtools)
 trimSeas$rpms<- quantcut(trimSeas$rpms, c(0,0.03,0.06,0.11,0.21,0.30,1)) # USDM percentiles
# trimSeas$rpms<- quantcut(trimSeas$rpms, c(0,0.05,0.10,0.15,0.25,0.35,1)) 
# trimSeas$rpms<- quantcut(trimSeas$rpms, c(0,0.33,0.66,1)) 

# rename levels
 levels(trimSeas$rpms) <- c("D4","D3","D2","D1","D0","No Drought")
# levels(trimSeas$rpms) <- c("Below","Normal","Above")

# package with polr
library(MASS)
library(rms)
m <- polr(rpms~Jul_Sep, data = trimSeas, Hess=TRUE)
  summary(m)
  ctable <- coef(summary(m))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  cbind(ctable, "p value" = p)
  confint(m)
  exp(cbind(OR = coef(m), ci))
new_data <- data.frame("Jul_Sep"=1)
  round(predict(m,new_data,type = "p"), 3)
  
  
  # plot ----
  library(ggplot2)  
  temp<-melt(trimSeas[,2:14])
  ggplot(temp, aes(rpms,value, color=rpms))+
    geom_boxplot(size = .75) +
    #geom_jitter(alpha = .5) +
    facet_grid(.~variable, margins = TRUE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  # ----
  

# all one and two factor models with rms or polr
# expand matrix to number of variables
library(MASS)
library(rms)  
regMat <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE))

# find one and two factor models
regMat <- regMat[which(rowSums(regMat == "TRUE")<=2),]
regMat <- regMat[-(dim(regMat)[1]),]
names(regMat) <- colnames(trimSeas[1:ncol(trimSeas)-1])

# rename
regressors <- colnames(trimSeas[1:ncol(trimSeas)-1])

allModelsList <- apply(regMat, 1, function(x) as.formula(
  paste(c("rpms ~", regressors[x]),
        collapse=" + ")) )

# allM <- lapply(allModelsList,
#                            function(x) polr(x, data=trimSeas, Hess=TRUE))

allM <- lapply(allModelsList,
               function(x) lrm(x, data=trimSeas, x=TRUE, y=TRUE))

# get stats out and find best lrm model
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
stats<-lapply(X = allM, FUN = `[[`, "stats")
  r2<-lapply(X=stats, FUN = `[[`, 10)
model<-lapply(X = allM, FUN = `[[`, "terms")
  model<-lapply(X = model, FUN = `[[`, 3)
dev<-lapply(X = allM, FUN = `[[`, c("deviance"))
  dev<-lapply(X = dev, FUN = `[[`, 2)
r2<-as.data.frame(unlist(nullToNA(r2)))
  colnames(r2)<-"r2value"
r2$model<-unlist(nullToNA(model))
r2$dev<-unlist(nullToNA(dev))

allM[[which(r2$r2value==max(r2$r2value, na.rm = TRUE))]]  
coef(allM[[which(r2$r2value==max(r2$r2value, na.rm = TRUE))]])[7]  


validate(allM[[which(r2$r2value==max(r2$r2value, na.rm = TRUE))]])

calibrate(allM[[52]])


bestM<-polr(rpms~Nov_Jan+Feb_Apr, data = trimSeas)
  exp(coef(bestM))
  new_data <- data.frame("Jan_Mar"= -2,"Jun_Aug"=0)
  round(predict(bestM,new_data,type = "probs"), 3)
  

