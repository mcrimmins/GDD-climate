# scratch heat map code


# correlations
# get summary stats for plots
meanGDDs<-spread(zStats,threshold,GDDValue)
meanGDDs<-meanGDDs[,c(1,2,3,5,4,6)]

corrGDD <- meanGDDs %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(GDDValue, GDDValue))
  do(data.frame(Cor=t(cor(.[,4:6], .[,4:6], method="kendall"))))
# add in split period correlations
# plot in heatmap, http://sebastianraschka.com/Articles/heatmaps_in_r.html
library(gplots)
rnames <- as.matrix(corrGDD[,1])
mat_data <- round(data.matrix(corrGDD[,2:ncol(corrGDD)]),2)
mat_data_all <- round(data.matrix(corrGDD[,2:ncol(corrGDD)]),2)
rownames(mat_data) <- rnames

heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          main = "1948-2016 GDD Correlations (mean GDDs, Kendall r)", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          col=rev(heat.colors(256)),
          margins =c(7,7),
          rowsep = seq(0,35,3),
          sepcolor = "black",
          Colv="NA",
          Rowv = "NA",
          dendrogram = "none",
          trace="none")            # turn off column clustering

# late period correlation 1981-2016
meanGDDs <- meanGDDs[ which(meanGDDs$year >= 1981),]

corrGDD <- meanGDDs %>%
  group_by(cluster) %>% # add in threshold
  #summarise(corrs = cor(GDDValue, GDDValue))
  do(data.frame(Cor=t(cor(.[,4:6], .[,4:6], method="kendall"))))
# add in split period correlations
# plot in heatmap, http://sebastianraschka.com/Articles/heatmaps_in_r.html
library(gplots)
rnames <- as.matrix(corrGDD[,1])
mat_data <- round(data.matrix(corrGDD[,2:ncol(corrGDD)]),2)
mat_data_late <- round(data.matrix(corrGDD[,2:ncol(corrGDD)]),2)
rownames(mat_data) <- rnames

heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          main = "1981-2016 GDD Correlations (mean GDDs, Kendall r)", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          col=rev(heat.colors(256)),
          margins =c(5,5),
          rowsep = seq(0,35,3),
          sepcolor = "black",
          Colv="NA",
          Rowv = "NA",
          dendrogram = "none",
          trace="none")            # turn off column clustering

# corr diff heatmap
mat_data<-mat_data_late-mat_data_all
heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          main = "GDD Correlations late-all", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          col=rev(heat.colors(256)),
          margins =c(5,5),
          rowsep = seq(0,35,3),
          sepcolor = "black",
          Colv="NA",
          Rowv = "NA",
          dendrogram = "none",
          trace="none")            # turn off column clustering