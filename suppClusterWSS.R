# supp material - cluster WSS error
# MAC 4/16/18

library(cowplot)
library(tidyr)

load("./fixed/zProd_FindClusterDiagnostics.RData")
df<-as.data.frame(cbind(seq(1,20,by=1), wss,minwss,btwss,totss))
colnames(df)[1]<-"cluster"
df$diffWSS<-c(NA,diff(df$wss)*-1)
df$diffMinWSS<-c(NA,diff(df$minwss)*-1)
  df$diffWSS[2]<-NA
  df$diffMinWSS[2]<-NA
df <- df[c(1,2,6,3,7)]
df<-df[2:20,]
# filter
#df <- df[which(df$cluster>=5 & df$cluster<=15),]

df<-gather(df, key = "cluster")
  colnames(df)[2]<-"var"
  df$var<-factor(df$var, levels = c("wss","diffWSS","minwss","diffMinWSS"))
  to_string <- as_labeller(c("wss" = "Within Cluster Sum of Squares", "diffWSS" ="Difference in Within Cluster Sum of Squares, k-1 to k",
                             "minwss" = "Minimum Within Cluster Sum of Squares","diffMinWSS" = "Difference in Minimum Within Cluster Sum of Squares, k-1 to k"))

ggplot(df, aes((cluster), value))+
  geom_line()+
  geom_point(data=df, aes(x=(cluster),y=value))+
  geom_vline(xintercept=15)+
  ylab("Squared Error")+
  xlab("Cluster (k)")+
  #xlim(10,15)+
  facet_wrap(~var, nrow = 4, scales = "free",  labeller = to_string)


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