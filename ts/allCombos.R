# Process TopoWx into matrix of correlations by start date, base T...
# 9/5/18 MAC

library(tidyr)
library(dplyr)

#---- load existing data?
load("~/RProjects/TopoWx/TopoWxTS_Nashville.RData")

# ---- Process into cumulative GDDs
# reshape and get tmean
tempDataWide <- spread(tempData, variable, bufferedPoint)
tempDataWide$tmean<-(tempDataWide$tmax+tempDataWide$tmin)/2
# add date vars
tempDataWide$month<-as.numeric(format(tempDataWide$DateTime, "%m"))
tempDataWide$year<-as.numeric(format(tempDataWide$DateTime, "%Y"))
tempDataWide$day<-as.numeric(format(tempDataWide$DateTime, "%d"))
tempDataWide$doy<-strptime(tempDataWide$DateTime, "%Y-%m-%d")$yday+1  
#tempDataWide<-filter(tempDataWide, month!=2 | day!=29)
#tempDataWide<-filter(tempDataWide, month!=12 | day!=31)

# start date
tempDataWide<-filter(tempDataWide,  doy >= 62)

# calc GDDs
baseT<-0
tempDataWide$tmean <- unlist(lapply(tempDataWide$tmean, function(x) ifelse(x>=baseT, x-baseT, 0)))

#tempDataWide$tmean[tempDataWide$tmean < baseT] <- 0
cumGDD <- tempDataWide %>% 
  dplyr::group_by(year, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
  dplyr::summarise(value = sum(tmean)) %>%
  dplyr::mutate(csum = cumsum(value))
tempDataWide$GDD<-cumGDD$csum

# get doy of threshold
GDDdoy<- function(x,y){
  doy<-which.max(x >= y)
  if(length(doy)==0){
    doy<-NA
  }else{
    doy<-doy
  }
  return(doy)
}
threshDOY <- tempDataWide %>% 
  dplyr::group_by(year) %>% # still doesn't quite work adjYear kicks off before adjDOY
  dplyr::summarise(thresh1 = GDDdoy(GDD,50), thresh2=GDDdoy(GDD,450))
cor(threshDOY$thresh1, threshDOY$thresh2)
