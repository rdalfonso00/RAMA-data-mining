setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
library(skimr)
PM10.df <- read_excel("assets/2017PM10.xls")
# make Fecha of type Date
PM10.df$FECHA <- as.Date(PM10.df$FECHA)
skim(PM10.df)
summary(PM10.df)

#### preprocessing ####
source("preprocesing.R")
PM10.red <- preprocess(PM10.df)
skim(PM10.red)
summary(PM10.red)

# Correlation analysis
library(corrplot)
M <- cor(PM10.red[,-c(1,2)])
corrplot(M, type = "upper", order = "hclust", addCoef.col = "black", number.font=0.8, number.cex = 0.8)
options(digits=3)
print(M)
# no high corr vars

library(Hmisc)
hist(PM10.red[,-c(1,2)]) 
#
## scale
PM10.scaled <- as_tibble(scale(PM10.red[,-c(1,2)]))
PM10.scaled
summary(PM10.scaled)

#### cluster tendency ####

#library(hopkins)
library(clustertend)
# Compute Hopkins statistic 
#####-_-####
#hopkins::hopkins(PM10.scaled)
set.seed(123)
clustertend::hopkins(PM10.scaled, n=700)

#### clustering ####
source("clust_func.R")
cluster_possibles <- map (1:9, ~clust_flex(.x, df.scaled = PM10.scaled))

library(patchwork) # allows for plotting in grid ggplot's with '+'
cluster_possibles[[1]] + cluster_possibles[[2]] + cluster_possibles[[3]] +
  cluster_possibles[[4]] + cluster_possibles[[5]] + cluster_possibles[[6]] +
  cluster_possibles[[7]] + cluster_possibles[[8]] + cluster_possibles[[9]] +
  plot_annotation (
    title = "CLARA Clustering of PM10 across potential number of clusters \U0022k\U0022 ",
    caption = "Visualization: FCC BUAP | Data: RAMA - Red Automática de Monitoreo Atmosférico",
    theme = theme (
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold", margin = margin
                                (0,0,20,0)),
      plot.caption = element_text (hjust = 1, size = 7, margin = margin (15,0,0,0))
    )
  )

# TARDA ###
methodologies <- c("wss", "silhouette", "gap_stat")
diss = dist(PM10.scaled, method = "manhattan")
cluster_optimal <- map (methodologies, ~fviz_nbclust (PM10.scaled, FUNcluster = clara, 
                                                      diss = diss, method = .x, k.max=5))
# elbow
cluster_optimal[[1]] # k-> 2 or 3
# silhouette
cluster_optimal[[2]] # k-> 2
# gap statistic
cluster_optimal[[3]] # k-> 3
cluster_optimal[[3]]$data

# main metrics  mixed

# need more analysis

#### 30 indices ####
library (NbClust)
############################ TARDA ############################

cluster_30_indexes <- NbClust(data = PM10.scaled, distance = "manhattan", min.nc = 2, max.nc =
                                5, method = "complete", index ="all")

fviz_nbclust(cluster_30_indexes, clara) +
  theme_minimal() +
  labs(title = "Frequency of Optimal Clusters using 30 indexes in NbClust Package")
########

clust_final <- pam(PM10.scaled, k = 3, metric="manhattan")
print(clust_final)
dd2 <- cbind(PM10.red, cluster = clust_final$cluster)
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
m <-sapply(dd[dd["cluster"] == 3,-ncol(dd)], mean)
print(m)
mean(m)
