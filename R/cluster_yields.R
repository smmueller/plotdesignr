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
  plot(plot_data[, plot_col_names], border = NA, pal = RColorBrewer::brewer.pal(8, 'Set1'))
}

#' Make dendrogram and single map
#' @title explore_dedrogram
#'
#' @param processed_data sf; An sf object likely returned from \code{make_cluster_data}.
#' Object should include the features to be clustered and their associated polygon
#' geometries.
#' @param cluster_number integer; number of clusters to be used.
#'
#' @return A two panel plot with the first panel being the field map colored by
#' cluster and the second panel being a dedrogram with colored rectangles showing
#' the clusters. The colors in the two plots should match.

explore_dendrogram <- function(processed_data, cluster_number){
  # make a copy of the processed data
  plot_data <- data.table::copy(processed_data)

  # scale data frame and drop geometry
  scaled_df <- scale(st_drop_geometry(plot_data))

  tree <- ClustGeo::hclustgeo(dist(scaled_df))
  sub_grp <- cutree(tree, k = cluster_number)

  plot_data$clusters <- sub_grp

  # to prevent warning in RColorBrewer call
  n_color <- ifelse(cluster_number <= 2, 3, cluster_number)

  par(mfrow = c(1, 2))
  # plot map
  plot(plot_data[, 'clusters'], border = NA, pal = RColorBrewer::brewer.pal(n_color, 'Set1'),
       key.pos = NULL, reset = FALSE)
  # plot dendrogram
  plot(tree, hang = -1, labels = F, main = NULL)
  rect.hclust(tree, k = cluster_number, border = RColorBrewer::brewer.pal(n_color, 'Set1'))
  par(mfrow = c(1, 1))
}

#' Tests for appropriate cluster number
#' @title explore_cluster_number
#'
#' @param processed_data sf; An sf object likely returned from \code{make_cluster_data}.
#' Object should include the features to be clustered and their associated polygon
#' geometries.
#' @param kmax integer; **optional** maximum cluster number to be passed to
#' factoextra::fviz_nbclust and NbClust::NbClust.
#'
#' @return A three panel plot showing the relationship between cluster number and
#' total within sum of squares, silhouette width, and Calinski and Harabasz Index.
#' These results can be used as a guide for choosing cluster number.

explore_cluster_number <- function(processed_data, kmax = 10){
  if(kmax < 2){
    stop('kmax must be >= 2')
  }

  scaled_df <- scale(st_drop_geometry(processed_data))

  one <- fviz_nbclust(scaled_df, hcut, method = "wss", nboot = 25, k.max = kmax) +
    labs(title = NULL) +
    theme(axis.title.y = element_text(size = 10))
  two <- fviz_nbclust(scaled_df, hcut, method = "silhouette", nboot = 25, k.max = kmax) +
    labs(title = NULL) +
    theme(axis.title.y = element_text(size = 10))
  CH <- NbClust::NbClust(scaled_df, distance = 'euclidean', method = 'ward.D',
                         min.nc = 2, max.nc = kmax, index = 'ch')$All.index
  CH_df <- data.frame(clust = 2:kmax,
                      CH = CH)
  three <-
    ggpubr::ggline(CH_df, x = "clust", y = "CH", group = 1,
                      color = 'steelblue', ylab = 'Calinski Harabasz Index',
                      xlab = "Number of clusters k",
                      main = NULL) +
      geom_vline(xintercept = CH_df[which.max(CH_df$CH), "clust"], linetype=2, color = 'steelblue') +
      scale_x_continuous(breaks = 1:kmax, limits = c(1, kmax)) +
      theme(axis.title.y = element_text(size = 10))

  ggpubr::ggarrange(one, two, three)
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
