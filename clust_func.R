library(factoextra)
library(cluster)
library(glue)
library(purrr)
clust_flex <- function (k, df.scaled) {
  df_pam <- clara(df.scaled, k, metric = "manhattan", pamLike = T)
  fviz_cluster(df_pam, geom = "point", data = df.scaled) +
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
