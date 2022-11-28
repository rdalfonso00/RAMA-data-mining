setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
library(skimr)
CO.df <- read_excel("assets/2017CO.xls")
# make Fecha of type Date
CO.df$FECHA <- as.Date(CO.df$FECHA)
skim(CO.df)

#### preprocessing ####
source("preprocesing.R")
CO.red <- CO.df[,-c(1,2)]
CO.red <- preprocess(CO.red)
skim(CO.red)
summary(CO.red)

# outlier detection

stats <- sapply(CO.red, boxplot.stats)
library(glue)
options(digits=6)
for (i in 1:ncol(stats)) {
  print(glue("For {colnames(stats)[i]} there are {length(stats[,i]$out)} outliers. ({length(stats[,i]$out)/nrow(CO.red)}%)"))
}
#### testing 1-99% percentile exclusion
lower_bound <- quantile(CO.red$XAL, 0.01)
lower_bound
upper_bound <- quantile(CO.red$XAL, 0.99)
upper_bound

s1 <- CO.red$XAL
out <- which(CO.red$XAL < lower_bound | CO.red$XAL > upper_bound)
s1[out] <- NA
summary(s1)
# good


CO.scaled <- as_tibble(scale(CO.red))
CO.scaled
#### cluster tendency ####

library(hopkins)
# Compute Hopkins statistic 
set.seed(123)
library(clustertend)
#####-_-####
hopkins::hopkins(CO.scaled, m=nrow(CO.scaled)-1)
clustertend::hopkins(CO.scaled, n=1000)

#### clustering? ####
#### clustering with pam() ####
library(cluster)
library(factoextra)
library(glue)
library(purrr)
# determining K
CO.scaled <- na.omit(as_tibble(CO.scaled))
pam_flex <- function (k) {
  df_pam <- clara(CO.scaled, k, pamLike = TRUE)
  fviz_cluster(df_pam, geom = "point", data = CO.scaled) +
    labs(title = glue("{k} clusters")) +
    theme (
      plot.background = element_blank(),
      panel.background = element_blank(),plot.title = element_text (margin = margin(0,0,5,0), hjust =
                                                                      0.5, size = 12, color = "grey", family = "Lato"),
      legend.text = element_text(hjust = 0, size = 8, family = "Lato"),
      legend.position = "none",
      legend.title = element_text(size = 8),
      axis.title = element_text (size = 8),
      axis.text = element_text (size = 8)
    )
}
cluster_possibles <- map (1:9, pam_flex)
library(patchwork) # allows for plotting in grid ggplot's with '+'
cluster_possibles[[1]] + cluster_possibles[[2]] + cluster_possibles[[3]] +
  cluster_possibles[[4]] + cluster_possibles[[5]] + cluster_possibles[[6]] +
  cluster_possibles[[7]] + cluster_possibles[[8]] + cluster_possibles[[9]] +
  plot_annotation (
    title = "CLARA Clustering of CO across potential number of clusters \U0022k\U0022 ",
    caption = "Visualization: FCC BUAP | Data: RAMA - Red Automática de Monitoreo Atmosférico",
    theme = theme (
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold", margin = margin
                                (0,0,20,0)),
      plot.caption = element_text (hjust = 1, size = 7, margin = margin (15,0,0,0))
    )
  )

# TARDA ###
methodologies <- c("wss", "silhouette", "gap_stat")
cluster_optimal <- map (methodologies, ~fviz_nbclust (CO.scaled, clara, method = .x))
# elbow
cluster_optimal[[1]] # -> 2
# silhouette
cluster_optimal[[2]] # -> 2
# gap statistic
cluster_optimal[[3]] # -> 2
                     # se concluye k = 2 #

clust_final <- clara(CO.scaled, k = 2, pamLike = TRUE)
print(clust_final)
dd <- cbind(CO.red, cluster = clust_final$cluster)
head(dd)
fviz_cluster(clust_final,
             palette = c("#00AFBB", "#FC4E07", "yellow"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)
options(digits=3)

# average of data compared to clusters
m <- sapply(dd[,-28], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 1,-28], mean)
print(m)
mean(m)
m <-sapply(dd[dd["cluster"] == 2,-28], mean)
print(m)
mean(m)
