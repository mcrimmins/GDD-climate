# Process TopoWx into matrix of correlations by start date, base T...
# 9/5/18 MAC

library(tidyr)
library(dplyr)

# FUNCTION - get doy of threshold
GDDdoy<- function(x,y){
  doy<-which.max(x >= y)
  if(length(doy)==0){
    doy<-NA
  }else{
    doy<-doy
  }
  return(doy)
}

#---- load existing data?
load("~/RProjects/TopoWx/TopoWxTS_Nashville.RData")
#load("~/RProjects/TopoWx/ts/TopoWxTS_MN.RData")

# ---- Process into cumulative GDDs
startDates<-c(1,32,62,92)
baseTs<-c(0,5,10,15,20)
corrs <- data.frame(matrix(ncol =length(startDates), nrow = length(baseTs))) 
  colnames(corrs) <- as.character(startDates)
  rownames(corrs) <- as.character(baseTs)

for(i in 1:length(startDates)){
    # reshape and get tmean
    tempDataWide <- spread(tempData, variable, bufferedPoint)
    tempDataWide$tmean<-(tempDataWide$tmax+tempDataWide$tmin)/2
    # add date vars
    tempDataWide$month<-as.numeric(format(tempDataWide$DateTime, "%m"))
    tempDataWide$year<-as.numeric(format(tempDataWide$DateTime, "%Y"))
    tempDataWide$day<-as.numeric(format(tempDataWide$DateTime, "%d"))
    tempDataWide$doy<-strptime(tempDataWide$DateTime, "%Y-%m-%d")$yday+1  
    # --test impact of leap year--
    #tempDataWide<-filter(tempDataWide, month!=2 | day!=29)
    #tempDataWide<-filter(tempDataWide, month!=12 | day!=31)
    
    # start date
    tempDataWide<-filter(tempDataWide,  doy >= startDates[i])
    
    # calc GDDs
    for(j in 1:length(baseTs)){
      tempDataWide2<-tempDataWide
      baseT<-baseTs[j]
      tempDataWide2$tmean <- unlist(lapply(tempDataWide2$tmean, function(x) ifelse(x>=baseT, x-baseT, 0)))
      #tempDataWide$tmean[tempDataWide$tmean < baseT] <- 0
      cumGDD <- tempDataWide2 %>% 
        dplyr::group_by(year, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
        dplyr::summarise(value = sum(tmean)) %>%
        dplyr::mutate(csum = cumsum(value))
      tempDataWide2$GDD<-cumGDD$csum
      
      threshDOY <- tempDataWide2 %>% 
        dplyr::group_by(year) %>% # still doesn't quite work adjYear kicks off before adjDOY
        dplyr::summarise(thresh1 = GDDdoy(GDD,50), thresh2=GDDdoy(GDD,450))
      corrs[j,i]<-cor(threshDOY$thresh1, threshDOY$thresh2)
     }
}
  
# create plots
  library(reshape2)
  library(ggplot2)
  corrs$baseT<- as.factor(baseTs)
  corrMelt<-melt(corrs)
  #corrMelt$baseT<-as.factor(corrMelt$baseT)
  #corrMelt$baseT<-levels(c("0","5","10","15","20"))
  
  ggplot(data=corrMelt)+
    geom_tile(aes(x=variable,y=baseT,fill=value),color = "black")+ 
    geom_text(aes(x=variable,y=baseT,fill=value, label = round(value,2)), size=4)+
    scale_fill_gradient(low="lightyellow", high="lightblue",
                         guide = guide_legend(title="Pearson r"))+
    labs(x = "Start Date", y="Base Temp", title="50v450GDD Correlation for Nashville")

  
  
    
  
  
  
  