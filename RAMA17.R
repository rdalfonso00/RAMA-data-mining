# setwd("~/Repositorios/PF Mineria22")
library(readxl)
library(dplyr)
PMCO.df <- read_excel("Repositorios/PF Mineria22/assets/2017PMCO.xls")
summary(PMCO.df)
print(PMCO.df)

del_nulls <- function(df){
  if (length(unique(df)) == 1) {
    print("xd")
  }
}
# select only non NULL columns
PMCO.red <- PMCO.df %>% select_if(~length(unique(.))!=1)
# remove NaN == -99 with mean
replaceNANmean <- function(x) replace(x, x==-99, mean(x, na.rm = TRUE))

PMCO.red[] <- lapply(PMCO.red, replaceNANmean)
summary(PMCO.red)

# agrupar esto y volverlo función para aplicarlo a cada df
hist(PMCO.red[,"CAM"])
