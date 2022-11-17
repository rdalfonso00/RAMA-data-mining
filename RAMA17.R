setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
library(skimr)
PMCO.df <- read_excel("assets/2017PMCO.xls")
skim(PMCO.df)
print(PMCO.df)

## see if omiting is viable ##
temp.df <- PMCO.df
temp.df <- temp.df %>% select_if(~length(unique(.))!=1)
temp.df[temp.df == -99] <- NA
temp.df %>% na.omit
# when removing NaN, only 136 observations, limiting the df too much the set, 
# so the method of imputing with the mean will be used

source("preprocesing.R")
PMCO.red <- clean_data(PMCO.df, -99)
skim(PMCO.red)
print(PMCO.red)


######## exploratory analysis ###########
library(ggplot2)
library(reshape)
library(tidyr)

box_p <- PMCO.red[,-c(1,2)] %>%  gather(cols, value) %>%  
  ggplot(aes(x = value)) + geom_boxplot(color="red") + facet_wrap(~cols, scale="free")
library(glue)
for (i in 1:ncol(PMCO.red[,-c(1,2)])) {
  stats <- boxplot.stats(PMCO.red[,i])
  #print(glue("For {colnames(PMCO.red)[3]} there are {length(stats$out)} outliers"))
}
boxplot.stats(PMCO.red$AJM)
print(stats)
print(box_p)
length(stats$out)
