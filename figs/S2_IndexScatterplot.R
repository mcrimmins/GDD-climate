# Season length, start and correlation scatterplots
# 8/10/18 MAC

library(raster)
library(reshape)
library(ggplot2)
library(cowplot)

# linear model  BASE 10----
# load mask
maskNA<-raster("./fixed/maskNAalt.grd")
# load datasets
load("./fixed/bothBaseT/pearsonCorrBaseT10.RData")
load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")
temp<-stack(mask(sdDiff50_450Res,maskNA), mask(sdGDD50Res,maskNA), mask(corRasterDet_BaseT10[[1]],maskNA))
names(temp) <- c('duration', 'start', 'corr')

# ggplot version
v1 <- data.frame((values(temp)))

# linear model BASE 0----
# load mask
maskNA_0<-raster("./fixed/maskNAalt_baseT0.grd") 
# load datasets
load("./fixed/bothBaseT/pearsonCorrBaseT0.RData")
load("./fixed/bothBaseT/sDevsBothBaseT_Detrended.RData")
temp<-stack(mask(sdDiff50_450_0Res,maskNA_0), mask(sdGDD50_0Res,maskNA_0), mask(corRasterDet_BaseT0[[1]],maskNA_0))
names(temp) <- c('duration', 'start', 'corr')

# ggplot version
v2 <- data.frame((values(temp)))

# font size
fontS<-8

# plot
p1<-ggplot(v1, aes(x=start, y=duration, color=corr)) + 
  geom_point(shape=16, size=0.25,alpha = 0.3)+
  #geom_smooth(method=lm, se=FALSE)+
  geom_abline()+
  scale_color_gradient(low = "#0091ff", high = "#f0650e", limits=c(0,1))+
  theme_bw()+
  ylim(0,35)+xlim(0,35)+
  labs(x="SD(T1) - days", y="SD(duration) - days", color="Pearson's r", 
       title="b) Season Start vs Duration Variability (10°C base temp)")+
  #theme_bw(base_size=6)
  theme(axis.text=element_text(size=fontS),
        axis.title=element_text(size=fontS),
        plot.title=element_text(size=fontS,face="bold"),
        legend.text = element_text(size=fontS),
        legend.title = element_text(size=fontS),
        legend.key.width=unit(0.5,"line"))

p2<-ggplot(v2, aes(x=start, y=duration, color=corr)) + 
  geom_point(shape=16, size=0.25,alpha = 0.3)+
  #geom_smooth(method=lm, se=FALSE)+
  geom_abline()+
  scale_color_gradient(low = "#0091ff", high = "#f0650e", limits=c(0,1))+
  theme_bw()+
  ylim(0,35)+xlim(0,35)+
  labs(x="SD(T1) - days", y="SD(duration) - days", color="Pearson's r", 
       title="a) Season Start vs Duration Variability (0°C base temp)")+
  #theme_bw(base_size=6)+
  theme(axis.text=element_text(size=fontS),
        axis.title=element_text(size=fontS),
        plot.title=element_text(size=fontS,face="bold"),
        legend.text = element_text(size=fontS),
        legend.title = element_text(size=fontS))+
  guides(colour=FALSE)

#pALL<-plot_grid(p2, p1, labels = c("a", "b"), label_size = 6, rel_widths = c(1,1.25))
pALL<-plot_grid(p2, p1, rel_widths = c(1,1.25))


#ggsave(plot = pALL, width = 5, height = 2.5, units = "in",dpi = 300, filename = "./figs/S2_scatterplot.png")

ggsave(plot = pALL, width = 7.48, height = 4.53, units = "in",dpi = 300, filename = "./figs/manuscript/Fig3_scatterplot.pdf")

