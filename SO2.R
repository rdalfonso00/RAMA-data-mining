setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
library(skimr)
SO2.df <- read_excel("assets/2017SO2.xls")
# make Fecha of type Date
SO2.df$FECHA <- as.Date(SO2.df$FECHA)
skim(SO2.df)
summary(SO2.df)

#### preprocessing ####
source("preprocesing.R")
SO2.red <- preprocess(SO2.df)
skim(SO2.red)
summary(SO2.red)
# no vars out
# Correlation analysis
library(corrplot)
M <- cor(SO2.red[,-c(1,2)])
corrplot(M, type = "upper", order = "hclust", addCoef.col = "black", number.font=0.8, number.cex = 0.8)
options(digits=3)
print(M)
# will delete CCA & MGH
SO2.red <- select(SO2.red, -c("CCA", "MGH"))

library(Hmisc)
hist(SO2.red[,-c(1,2)]) 
#
## scale
SO2.scaled <- as_tibble(scale(SO2.red[,-c(1,2)]))
SO2.scaled
summary(SO2.scaled)

#### cluster tendency ####

#library(hopkins)
library(clustertend)
# Compute Hopkins statistic 
#####-_-####
#hopkins::hopkins(SO2.scaled)
set.seed(123)
clustertend::hopkins(SO2.scaled, n=700)

#### clustering ####
source("clust_func.R")
cluster_possibles <- map (1:9, ~clust_flex(.x, df.scaled = SO2.scaled))

library(patchwork) # allows for plotting in grid ggplot's with '+'
cluster_possibles[[1]] + cluster_possibles[[2]] + cluster_possibles[[3]] +
  cluster_possibles[[4]] + cluster_possibles[[5]] + cluster_possibles[[6]] +
  cluster_possibles[[7]] + cluster_possibles[[8]] + cluster_possibles[[9]] +
  plot_annotation (
    title = "CLARA Clustering of SO2 across potential number of clusters \U0022k\U0022 ",
    caption = "Visualization: FCC BUAP | Data: RAMA - Red Automática de Monitoreo Atmosférico",
    theme = theme (
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold", margin = margin
                                (0,0,20,0)),
      plot.caption = element_text (hjust = 1, size = 7, margin = margin (15,0,0,0))
    )
  )

# TARDA ###
methodologies <- c("wss", "silhouette", "gap_stat")
diss = dist(SO2.scaled, method = "manhattan")
cluster_optimal <- map (methodologies, ~fviz_nbclust (SO2.scaled, FUNcluster = clara, 
                                                      diss = diss, method = .x, k.max=6))
# elbow
cluster_optimal[[1]] # k-> 2
# silhouette
cluster_optimal[[2]] # k-> 2
# gap statistic
cluster_optimal[[3]] # k-> 2
cluster_optimal[[3]]$data

# main metrics  suggest k=2

# NO need for more analysis

#### 30 indices ####
#library (NbClust)
############################ TARDA ############################

#cluster_30_indexes <- NbClust(data = SO2.scaled, distance = "manhattan", min.nc = 2, max.nc =
#                               5, method = "complete", index ="all")

#fviz_nbclust(cluster_30_indexes, clara) +
#  theme_minimal() +
#  labs(title = "Frequency of Optimal Clusters using 30 indexes in NbClust Package")
# k=2 selected!

clust_final <- clara(SO2.scaled, k = 2, metric="manhattan", pamLike = T)
print(clust_final)
dd2 <- cbind(SO2.red, cluster = clust_final$cluster)
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

