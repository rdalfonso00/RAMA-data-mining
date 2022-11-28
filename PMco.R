setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
library(skimr)
PMCO.df <- read_excel("assets/2017PMCO.xls")
# make Fecha of type Date
PMCO.df$FECHA <- as.Date(PMCO.df$FECHA)
skim(PMCO.df)

## see if omiting is viable ##
temp.df <- PMCO.df
temp.df <- temp.df %>% select_if(~length(unique(.))!=1)
temp.df[temp.df == -99] <- NA
skim(temp.df)
temp.df %>% na.omit
# when removing NaN, only 136 observations, limiting the df too much the set, 
# so the method of imputing with the mean will be used

source("preprocesing.R")
PMCO.red <- clean_data(PMCO.df, -99)
skim(PMCO.red)
print(PMCO.red)

### correlation analysis
library(corrplot)
M <- cor(PMCO.red[,-c(1,2)])
corrplot(M, type = "upper", order = "hclust", addCoef.col = "black")
options(digits=3)
print(M)

# hist
library(Hmisc)
hist.data.frame(PMCO.red[,-c(1,2)])
# delete right most data (extreme)
PMCO.cut <- PMCO.red
PMCO.cut[,-c(1,2)] <- PMCO.red[,-c(1,2)] %>% lapply(repl_upper_NA)
PMCO.cut <- na.omit(PMCO.cut)
PMCO.cut
hist.data.frame(PMCO.cut[,-c(1,2)])


# the up code was for exlporation, the funcion 'rep_out_mean' was implemented to remove outliers

# using 'preprocess' function
PMCO.final <- preprocess(PMCO.df)

summary(PMCO.final - PMCO.cut) # -> equal, preprocess() works

PMCO.scaled <- as_tibble(scale(PMCO.final[,-c(1,2)]))
summary(PMCO.scaled)

########### cluster tendency ###########
library(clustertend)
library(hopkins)
# Compute Hopkins statistic 
set.seed(123)

#####-_-####
#hopkins::hopkins(PMCO.scaled, m = 1000)
clustertend::hopkins(PMCO.scaled, n=1000)
# visualize VAT ##### DONT DO WITH TOO MUCH DATA 
#fviz_dist(dist(PMCO.red), show_labels = FALSE)+
#  labs(title = "PMco data")

#### clustering with pam() ####
source("clust_func.R")
cluster_possibles <- map (1:9, ~clust_flex(.x, df.scaled = PMCO.scaled))

library(patchwork) # allows for plotting in grid ggplot's with '+'
cluster_possibles[[1]] + cluster_possibles[[2]] + cluster_possibles[[3]] +
  cluster_possibles[[4]] + cluster_possibles[[5]] + cluster_possibles[[6]] +
  cluster_possibles[[7]] + cluster_possibles[[8]] + cluster_possibles[[9]] +
  plot_annotation (
    title = "CLARA Clustering of PMco across potential number of clusters \U0022k\U0022 ",
    caption = "Visualization: FCC BUAP | Data: RAMA - Red Automática de Monitoreo Atmosférico",
    theme = theme (
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold", margin = margin
                                (0,0,20,0)),
      plot.caption = element_text (hjust = 1, size = 7, margin = margin (15,0,0,0))
    )
  )

##### TARDA #####
methodologies <- c("wss", "silhouette", "gap_stat")
cluster_optimal <- map (methodologies, ~fviz_nbclust (PMCO.scaled, pam, method = .x))
# elbow
cluster_optimal[[1]]
# silhouette
cluster_optimal[[2]]
# gap statistic
cluster_optimal[[3]]

library (NbClust)
############################ TARDA ############################
cluster_30_indexes <- NbClust(data = PMCO.scaled, distance = "manhattan", min.nc = 2, max.nc =
                                6, method = "complete", index ="all")
fviz_nbclust(cluster_30_indexes, clara) +
  theme_minimal() +
  labs(title = "Frequency of Optimal Clusters using 30 indexes in NbClust Package")

# k=3 is the winner

clust_final <- clara(PMCO.scaled, k = 3, pamLike = TRUE)
print(clust_final)
dd <- cbind(PMCO.final, cluster = clust_final$cluster)
head(dd)
fviz_cluster(clust_final,
             palette = "Dark2", # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
             )

# aplicar estadísticos por cluster

options(digits=3)
# average of data compared to clusters
m <- sapply(dd[,-c(1,2,ncol(dd))], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 1,-c(1,2,ncol(dd))], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 2,-c(1,2,ncol(dd))], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 3,-c(1,2,ncol(dd))], mean)
print(m)
mean(m)
