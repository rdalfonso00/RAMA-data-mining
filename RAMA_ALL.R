setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
library(skimr)
CO.df <- read_excel("assets/2017CO.xls")
NO.df <- read_excel("assets/2017NO.xls")
NO2.df <- read_excel("assets/2017NO2.xls")
NOX.df <- read_excel("assets/2017NOX.xls")
O3.df <- read_excel("assets/2017O3.xls")
PM10.df <- read_excel("assets/2017PM10.xls")
PM25.df <- read_excel("assets/2017PM25.xls")
PMCO.df <- read_excel("assets/2017PMCO.xls")
SO2.df <- read_excel("assets/2017SO2.xls")

data <- list(CO.df, NO.df, NO2.df, NOX.df, O3.df, PM10.df, PM25.df, PMCO.df, SO2.df)
# change date to date format
chg_date <- function(df){ df$FECHA <- as.Date(df$FECHA); return(df)}
data <- lapply(data, chg_date)
# delete date and hour
data.red <- lapply(data, function(x) x[,-c(1,2)])
# preprocess
source("preprocesing.R")
data.prep <- lapply(data.red, clean_data, nanVal=-99) # no outlier detection yet
#### combine all datasets
## rowMeans for obtaining 1 columns for each contaminant
rama.all <- lapply(data.prep, rowMeans)
rama.all <- lapply(rama.all, as_tibble)
rama.all
## rename columns
colnames(rama.all[[1]]) <- c("CO")
colnames(rama.all[[2]]) <- c("NO")
colnames(rama.all[[3]]) <- c("NO2")
colnames(rama.all[[4]]) <- c("NOX")
colnames(rama.all[[5]]) <- c("O3")
colnames(rama.all[[6]]) <- c("PM10")
colnames(rama.all[[7]]) <- c("PM25")
colnames(rama.all[[8]]) <- c("PMCO")
colnames(rama.all[[9]]) <- c("SO2")
rama.all
## merge all columns
rama.merged <- rama.all[[1]]
rama.merged <- as_tibble(cbind(rama.merged, rama.all)[,-1])
rama.merged
# exploratory analysis
library(skimr)
skim(rama.merged)
summary(rama.merged)


# outlier detection
library(ggplot2)
library(reshape)
library(tidyr)

rama.merged %>%  gather(cols, value) %>%  
  ggplot(aes(x = value)) + geom_boxplot(color="blue") + facet_wrap(~cols, scale="free")

 
# 1% lower and upper deleted - bind date and hour
library(Hmisc)
hist.data.frame(rama.merged)
rama.reduced <- cbind(NO.df[,c(1,2)], rama.merged) %>% lapply(repl_upper_NA) %>% as_tibble %>% na.omit
rama.reduced
hist.data.frame(rama.reduced[,-c(1,2)])

#scale for different measures
rama.scaled <- rama.reduced
rama.scaled[,3:11] <- scale(rama.reduced[,3:11])
summary(rama.scaled)
# correlation analysis

library (GGally)
ggpairs(
  data = rama.scaled,
  columns = c(3:11),
  diag = list(continuous = wrap("barDiag", color = "blue", size =4)),
  upper = list(continuous = wrap("cor", size = 4, bins = 60))
)
# high corr, drop NOX and PM10
rama.scaled <- select(rama.scaled, -c("NOX", "PM10"))

# can we cluster?
library(clustertend)
# Compute Hopkins statistic 
set.seed(123)

library(parallelMap)

clustertend::hopkins(rama.scaled[,-c(1,2)], n=1000)


# good number, proceed to viz clusters
source("clust_func.R")
cluster_possibles <- map (1:9, ~clust_flex(.x, df.scaled = rama.scaled[,-c(1,2)]))

library(patchwork)
cluster_possibles[[1]] + cluster_possibles[[2]] + cluster_possibles[[3]] +
  cluster_possibles[[4]] + cluster_possibles[[5]] + cluster_possibles[[6]] +
  cluster_possibles[[7]] + cluster_possibles[[8]] + cluster_possibles[[9]] +
  plot_annotation (
    title = "CLARA Clustering of ALL RAMA 2017 across potential number of clusters \U0022k\U0022 ",
    caption = "Visualization: FCC BUAP | Data: RAMA - Red Automática de Monitoreo Atmosférico",
    theme = theme (
      plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold", margin = margin
                                (0,0,20,0)),
      plot.caption = element_text (hjust = 1, size = 7, margin = margin (15,0,0,0))
    )
  )

# what k?
library (NbClust)

cluster_30_indexes <- NbClust(data = rama.scaled[,-c(1,2)], distance = "manhattan", min.nc = 2, max.nc =
                                6, method = "complete", index ="all")

fviz_nbclust(cluster_30_indexes, clara) +
  theme_minimal() +
  labs(title = "Frequency of Optimal Clusters using 30 indexes in NbClust Package")

# asuming k = 2
clust_final <- clara(rama.scaled[,-c(1,2)], k = 2, metric = "manhattan")
fviz_cluster(clust_final,
             palette = "Dark2", # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)
dd <- cbind(rama.reduced, cluster = clust_final$cluster) %>% as_tibble
head(dd)

options(digits=5, scipen=0)
dd2 <- select(dd, c("CO","NO","NO2","NOX","O3","PM10","PM25","PMCO","SO2","cluster"))
# average of data compared to clusters
m <- sapply(dd2[-ncol(dd2)], mean)
print(m)
mean(m)
m <-sapply(dd2[dd2["cluster"] == 1,-ncol(dd2)], mean)
print(m)
mean(m)
m <-sapply(dd2[dd2["cluster"] == 2,-ncol(dd2)], mean)
print(m)
mean(m)

