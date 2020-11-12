#' Exploratory analysis for choosing mixing parameter
#' @title explore_best_mix
#'
#' @param processed_data sf; An sf object likely returned from \code{make_cluster_data}.
#' Object should include the features to be clustered and their associated polygon
#' geometries.
#' @param cluster_number integer; number of clusters to be used.
#' @param range numeric; **optional** range of mixing parameters to consider.
#' These values are passed as the alpha parameter in \code{ClustGeo::hclustgeo}
#' and \code{ClustGeo::choicealpha}. The values must be between 0 and 1, the
#' default is \code{seq(0, 0.5, 0.1)}.
#'
#' @return Two plots are returned. The first is the output from \code{ClustGeo::choicealpha}
#' and the second is a faceted plot showing the field cluster results for each
#' level of alpha in \code{range}. This information should be used to choose a
#' value for the mixing paramters (alpha).

explore_best_mix <- function(processed_data, cluster_number, range = seq(0, 0.5, 0.1)){

  # make a copy of the processed data
  plot_data <- data.table::copy(processed_data)

  # scale data frame and drop geometry
  scaled_df <- scale(st_drop_geometry(processed_data))

  # dissimilarity matrix of scaled data
  D0 <- dist(scaled_df, method = 'euclidean')

  # dissimilarity matrix of coordinates
  # set aggregation to constant to avoid warning from st_centroid()
  st_agr(processed_data) = 'constant'
  coord_df <- do.call(rbind, st_centroid(processed_data) %>% st_geometry())
  D1 <- dist(coord_df, method = "euclidean")

  # plotting method from ClustGeo
  par(mfrow = c(1, 2))
  cr <- ClustGeo::choicealpha(D0, D1, range.alpha = range, K = cluster_number, graph = TRUE)
  par(mfrow = c(1, 1))

  # plot maps
  for(a in seq_along(range)){

    tree_geo <- ClustGeo::hclustgeo(D0, D1, alpha = range[a])

    sub_grp_geo <- cutree(tree_geo, k = cluster_number)

    plot_data[, ncol(plot_data) + 1] <- sub_grp_geo
    names(plot_data)[ncol(plot_data)] <- paste0('cluster_geo_', range[a])

  }

  # only plot new columns
  plot_col_names <- names(plot_data)[grepl('cluster_geo', names(plot_data))]
  plot(plot_data[, plot_col_names])
}

#' Make final clusters
#' @title finalize_clusters
#'
#' @param processed_data sf; An sf object likely returned from \code{make_cluster_data}.
#' Object should include the features to be clustered and their associated polygon
#' geometries.
#' @param cluster_number integer; number of clusters to be used.
#' @param mixing_parameter numeric; Values to be passed as the alpha parameter
#' in \code{ClustGeo::hclustgeo} which controls the weighting between the data
#' and distance dissimilarity matrices. Value must be between 0 and 1.
#'
#' @return The sf object \code{processed_data} with a new column (cluster) giving
#' the cluster designation for each row.

finalize_clusters <- function(processed_data, cluster_number, mixing_parameter){

  # scale data frame and drop geometry
  scaled_df <- scale(st_drop_geometry(processed_data))

  # dissimilarity matrix of data
  D0 <- dist(scaled_df, method = 'euclidean')

  # dissimilarity matrix of coordinates
  # set aggregation to constant to avoid warning from st_centroid()
  st_agr(processed_data) = 'constant'
  coord_df <- do.call(rbind, st_centroid(processed_data) %>% st_geometry())
  D1 <- dist(coord_df, method = "euclidean")

  tree_geo <- ClustGeo::hclustgeo(D0, D1, alpha = mixing_parameter)

  # Cut tree into K groups
  sub_grp_geo <- cutree(tree_geo, k = cluster_number)

  # add cluster column to processed_data
  processed_data$cluster <- sub_grp_geo

  return(processed_data)
}
