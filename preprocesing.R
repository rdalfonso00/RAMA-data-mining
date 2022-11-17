#' clean_data(df)
#'
#' @param df the dataframe to be cleaned
#' @param nanVal the custom NaN value to search for
#' @return the df with no useless (al equal) columns and no NaN's
#' @export
#'
#' @examples
clean_data <- function(df, nanVal){
  library(dplyr)
  #library(naniar)
  clean.df <- df %>% select_if(~length(unique(.))!=1)
  # remove NaN == -99 with mean
  clean.df[clean.df == nanVal] <- NA
  replaceNANmean <- function(x) replace(x, is.na(x), round(mean(x, na.rm = TRUE)))
  
  clean.df[] <- lapply(clean.df, replaceNANmean)
  return(clean.df)
}