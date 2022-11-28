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
  # remove columns with all equal valules
  clean.df <- df %>% select_if(~length(unique(.)) != 1)
  # remove NaN == -99 with mean
  clean.df[clean.df == nanVal] <- NA
  # remove colums with less than 5% efective data
  clean.df <- select_if(clean.df, !( colSums(is.na(clean.df)) > nrow(clean.df)*(1-0.2)) )
  replaceNANmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
  
  clean.df[] <- lapply(clean.df, replaceNANmean)
  return(clean.df)
}

repl_out_box <- function(c){
  b <- boxplot.stats(c)
  s1 <- c
  #s1[which(c %in% b$out)] <- mean(c[which(! c %in% b$out)],na.rm=TRUE)
  s1[which(c %in% b$out)] <- NA
  return(s1)
}

repl_out_NA <- function(c){
  lower_bound <- quantile(c, 0.01)
  lower_bound
  upper_bound <- quantile(c, 0.99)
  upper_bound
  out <- which(c < lower_bound | c > upper_bound)
  s1 <- c
  s1[out] <- NA
  #s1[which(c %in% b$out)] <- NA
  return(s1)
}

repl_upper_NA <- function(c){
  upper_bound <- quantile(c, 0.975)
  upper_bound
  out <- which( c > upper_bound)
  s1 <- c
  s1[out] <- NA
  #s1[which(c %in% b$out)] <- NA
  return(s1)
}

preprocess <- function(df){
  nanVal=-99
  temp.df <- clean_data(df, nanVal)
  final.df <- temp.df
  final.df[,-c(1,2)] <- temp.df[,-c(1,2)] %>% lapply(repl_upper_NA)
  final.df <- na.omit(final.df)
  final.df <- as_tibble(final.df)
  return(final.df)
}
