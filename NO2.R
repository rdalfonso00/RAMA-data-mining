setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
library(skimr)
NO2.df <- read_excel("assets/2017NO2.xls")
# make Fecha of type Date
NO2.df$FECHA <- as.Date(NO2.df$FECHA)
skim(NO2.df)

#### preprocessing ####
source("preprocesing.R")
NO2.red <- NO2.df[,-c(1,2)]
NO2.red <- preprocess(NO2.red)
skim(NO2.red)

# correlation analysis
library(corrplot)
M <- cor(NO2.red[,-c(1,2)])
corrplot(M, type = "upper", order = "hclust", addCoef.col = "black", number.font=0.8, number.cex = 0.8)
options(digits=3)
print(M)
# will drop CUT and MER
NO2.red <- select(NO2.red, -c("CUT", "MER"))
library(Hmisc)
hist.data.frame(NO2.red[,-c(1,2)]) # no
#
## scale
NO2.scaled <- as_tibble(scale(NO2.red))
NO2.scaled
summary(NO2.scaled)

#### cluster tendency ####

#library(hopkins)
library(clustertend)
# Compute Hopkins statistic 
#####-_-####
#hopkins::hopkins(NO2.scaled)
set.seed(123)
clustertend::hopkins(NO2.scaled, n=700)

#### clustering ####

source("clust_func.R")
cluster_possibles <- map (1:9, ~clust_flex(.x, df.scaled = NO2.scaled))

library(patchwork) # allows for plotting in grid ggplot's with '+'
cluster_possibles[[1]] + cluster_possibles[[2]] + cluster_possibles[[3]] +
  cluster_possibles[[4]] + cluster_possibles[[5]] + cluster_possibles[[6]] +
  cluster_possibles[[7]] + cluster_possibles[[8]] + cluster_possibles[[9]] +
  plot_annotation (
    title = "CLARA Clustering of NO2 across potential number of clusters \U0022k\U0022 ",
    caption = "Visualization: FCC BUAP | Data: RAMA - Red Automática de Monitoreo Atmosférico",
    theme = theme (
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold", margin = margin
                                (0,0,20,0)),
      plot.caption = element_text (hjust = 1, size = 7, margin = margin (15,0,0,0))
    )
  )

# TARDA ###
methodologies <- c("wss", "silhouette", "gap_stat")
diss = dist(NO2.scaled, method = "manhattan")
cluster_optimal <- map (methodologies, ~fviz_nbclust (NO2.scaled, FUNcluster = clara, diss = diss,method = .x))
# elbow
cluster_optimal[[1]] # k-> 2 or 3
# silhouette
cluster_optimal[[2]] # k-> 2
# gap statistic
cluster_optimal[[3]] # k-> 2
# main metric suggest k=2

# No need for more analysis

#### 30 indices ####
#library (NbClust)
############################ TARDA ############################
#library(parallelMap)

#cluster_30_indexes <- NbClust(data = NO2.scaled, distance = "euclidean", min.nc = 2, max.nc =
#                                9, method = "complete", index ="all")

#fviz_nbclust(cluster_30_indexes, clara) +
#  theme_minimal() +
#  labs(title = "Frequency of Optimal Clusters using 30 indexes in NbClust Package")

clust_final <- pam(NO2.scaled, k = 2)
print(clust_final)
dd <- cbind(NO2.red, cluster = clust_final$cluster)
head(dd)
fviz_cluster(clust_final,
             palette = "Dark2", # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)

options(digits=3)
# average of data compared to clusters
m <- sapply(dd[,-ncol(dd)], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 1,-ncol(dd)], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 2,-ncol(dd)], mean)
print(m)
mean(m)
