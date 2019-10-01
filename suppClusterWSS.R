# supp material - cluster WSS error
# MAC 4/16/18

library(cowplot)
library(tidyr)

load("./fixed/FindClusterDiagnostics.RData")
#load("./fixed/zProd_FindClusterDiagnostics.RData")
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

df<-gather(df, 'wss','diffWSS','minwss','diffMinWSS',key = 'var',value='values')
  #colnames(df)[2]<-"var"
  df$var<-factor(df$var, levels = c("wss","diffWSS","minwss","diffMinWSS"))
  to_string <- as_labeller(c("wss" = " a.) Within Cluster Sum of Squares", "diffWSS" =" b.) Difference in Within Cluster Sum of Squares, k-1 to k",
                             "minwss" = " c.) Minimum Within Cluster Sum of Squares","diffMinWSS" = " d.) Difference in Minimum Within Cluster Sum of Squares, k-1 to k"))

p<-ggplot(df, aes((cluster), values))+
  geom_line()+
  geom_point(data=df, aes(x=(cluster),y=values))+
  geom_vline(xintercept=12, linetype="dotted")+
  ylab("Squared Error")+
  xlab("Cluster (k)")+
  #xlim(10,15)+
  facet_wrap(~var, nrow = 4, scales = "free",  labeller = to_string)+
  theme(strip.text.x = element_text(angle = 0, hjust = 0))

# plot to png
png("/home/crimmins/RProjects/TopoWx/clusterTrends/figs/SuppFig_WSS_screePlots.png", width = 7, height = 7, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()


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