setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
library(skimr)
O3.df <- read_excel("assets/2017O3.xls")
# make Fecha of type Date
O3.df$FECHA <- as.Date(O3.df$FECHA)
skim(O3.df)
summary(O3.df)

#### preprocessing ####
source("preprocesing.R")
O3.red <- preprocess(O3.df)
skim(O3.red)
summary(O3.red)

# correlation analysis
library(corrplot)
M <- cor(O3.red[,-c(1,2)])
corrplot(M, type = "upper", order = "hclust", addCoef.col = "black", number.font=0.8, number.cex = 0.8)
options(digits=3)
print(M)
# ALL HIGH CORR VARS
# we can do PCA to reduce dimension
pcs <- prcomp(O3.red[,-c(1,2)])
summary(pcs)
# with 9 variables we get a good proportion or significance (0.888)
O3.prcomp <- as_tibble(pcs$x[,1:9])
O3.prcomp

library(Hmisc)
hist(O3.prcomp) 
#
## scale
O3.scaled <- as_tibble(scale(O3.prcomp))
O3.scaled
summary(O3.scaled)

#### cluster tendency ####

#library(hopkins)
library(clustertend)
# Compute Hopkins statistic 
#####-_-####
#hopkins::hopkins(O3.scaled)
set.seed(123)
clustertend::hopkins(O3.scaled, n=700)

#### clustering ####
source("clust_func.R")
cluster_possibles <- map (1:9, ~clust_flex(.x, df.scaled = O3.scaled))

library(patchwork) # allows for plotting in grid ggplot's with '+'
cluster_possibles[[1]] + cluster_possibles[[2]] + cluster_possibles[[3]] +
  cluster_possibles[[4]] + cluster_possibles[[5]] + cluster_possibles[[6]] +
  cluster_possibles[[7]] + cluster_possibles[[8]] + cluster_possibles[[9]] +
  plot_annotation (
    title = "CLARA Clustering of O3 across potential number of clusters \U0022k\U0022 ",
    caption = "Visualization: FCC BUAP | Data: RAMA - Red Automática de Monitoreo Atmosférico",
    theme = theme (
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold", margin = margin
                                (0,0,20,0)),
      plot.caption = element_text (hjust = 1, size = 7, margin = margin (15,0,0,0))
    )
  )

# TARDA ###
methodologies <- c("wss", "silhouette", "gap_stat")
diss = dist(O3.scaled, method = "manhattan")
cluster_optimal <- map (methodologies, ~fviz_nbclust (O3.scaled, FUNcluster = clara, diss = diss,method = .x, k.max=4))
# elbow
cluster_optimal[[1]] # k-> 2 
# silhouette
cluster_optimal[[2]] # k-> 2
# gap statistic
cluster_optimal[[3]] # k-> 4 (2 is still good)
cluster_optimal[[3]]$data
#gap(2) -> 0.938
#gap(4) -< 0.956. diff=0.018. Therefore 2 is still good

# main metrics suggest k=2 
# k=2 will be used
# No need for more analysis

#### 30 indices ####
#library (NbClust)
############################ TARDA ############################
#library(parallelMap)

#cluster_30_indexes <- NbClust(data = O3.scaled, distance = "manhattan", min.nc = 2, max.nc =
#                                9, method = "complete", index ="all")

#fviz_nbclust(cluster_30_indexes, clara) +
#  theme_minimal() +
#  labs(title = "Frequency of Optimal Clusters using 30 indexes in NbClust Package")

clust_final <- clara(O3.scaled, k = 2, metric="manhattan", pamLike = T)
print(clust_final)
dd2 <- cbind(O3.red, cluster = clust_final$cluster)
head(dd2)
fviz_cluster(clust_final,
             palette = "Dark2", # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)
# with og dataset 
clust_final <- clara(scale(O3.red[,-c(1,2)]), k = 2, metric="manhattan", pamLike = T)
print(clust_final)
dd2 <- cbind(O3.red, cluster = clust_final$cluster)
head(dd2)
fviz_cluster(clust_final,
             palette = "Dark2", # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)

options(digits=3)
# average of data compared to clusters
dd <- dd2[,-c(1,2)]
m <- sapply(dd[,-ncol(dd)], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 1,-ncol(dd)], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 2,-ncol(dd)], mean)
print(m)
mean(m)

# max
data.frame(lapply(dd[,-ncol(dd)], max))

