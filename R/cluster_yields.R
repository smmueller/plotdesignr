#' @title Exploratory analysis for choosing mixing parameter
#' @export
#'
#' @param processed_data sf; An sf object likely returned from \code{make_cluster_data}.
#' Object should include the features to be clustered and their associated polygon
#' geometries.
#' @param cluster_number integer; number of clusters to be used.
#' @param range numeric; **optional** range of mixing parameters to consider.
#' These values are passed as the alpha parameter in \code{ClustGeo::hclustgeo}
#' and \code{ClustGeo::choicealpha}. The values must be between 0 and 1, the
#' default is \code{seq(0, 0.5, 0.1)}.
#' @param plot logical; **optional** should plot be returned in the current graphics
#' device (likely in RStudio). Default is TRUE.
#' @param output_path string; **optional** path where plot should be saved.
#' Default is NULL. If path is provided, PDFs with the names
#' "mixing_parameters_map_k_clusters" and "mixing_parameters_plot_k_clusters"
#' will be created, where k is the integer passed to cluster_number.
#'
#' @return Two plots are returned. The first is the output from \code{ClustGeo::choicealpha}
#' and the second is a faceted plot showing the field cluster results for each
#' level of alpha in \code{range}. This information should be used to choose a
#' value for the mixing paramters (alpha).

explore_best_mix <- function(processed_data, cluster_number, range = seq(0, 0.5, 0.1),
                             plot = TRUE, output_path = NULL){

  no_side_effect_warning(plot, output_path)
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

  # get Q for different levels of alpha
  cr <- ClustGeo::choicealpha(D0, D1, range.alpha = range, K = cluster_number, graph = FALSE)
  choicealpha_plot %<a-% ClustGeo::plot.choicealpha(cr)

  # plot maps
  for(a in seq_along(range)){

    tree_geo <- ClustGeo::hclustgeo(D0, D1, alpha = range[a])

    sub_grp_geo <- cutree(tree_geo, k = cluster_number)

    plot_data[, ncol(plot_data) + 1] <- sub_grp_geo
    names(plot_data)[ncol(plot_data)] <- paste0('cluster_geo_', range[a])
  }

  # only plot new columns
  plot_col_names <- names(plot_data)[grepl('cluster_geo', names(plot_data))]
  choicealpha_map %<a-% plot(plot_data[, plot_col_names], border = NA,
                             pal = RColorBrewer::brewer.pal(8, 'Set1'))

  plot_handler(plot_logical = plot, output_path = output_path, plot_call = choicealpha_plot,
               plot_name = paste('mixing_parameter_plot', cluster_number, 'clusters', sep = '_'))

  plot_handler(plot_logical = plot, output_path = output_path, plot_call = choicealpha_map,
               plot_name = paste('mixing_parameter_map', cluster_number, 'clusters', sep = '_'))
}

#' @title Make dendrogram and single map
#' @export
#'
#' @param processed_data sf; An sf object likely returned from \code{make_cluster_data}.
#' Object should include the features to be clustered and their associated polygon
#' geometries.
#' @param cluster_number integer; number of clusters to be used.
#' @param plot logical; **optional** should plot be returned in the current graphics
#' device (likely in RStudio). Default is TRUE.
#' @param output_path string; **optional** path where plot should be saved.
#' Default is NULL. If path is provided, a PDF with the name "dendrogram_k_clusters"
#' will be created, where k is the integer passed to cluster_number.
#'
#' @return A two panel plot with the first panel being the field map colored by
#' cluster and the second panel being a dedrogram with colored rectangles showing
#' the clusters. The colors in the two plots should match.

explore_dendrogram <- function(processed_data, cluster_number, plot = TRUE,
                               output_path = NULL){

  no_side_effect_warning(plot, output_path)
  # make a copy of the processed data
  plot_data <- data.table::copy(processed_data)

  # scale data frame and drop geometry
  scaled_df <- scale(st_drop_geometry(plot_data))

  tree <- ClustGeo::hclustgeo(dist(scaled_df))
  sub_grp <- cutree(tree, k = cluster_number)

  plot_data$clusters <- sub_grp

  # returns or saves dendro_plot
  dendro_plot %<a-% make_dendrogram_plot(plot_data, tree, cluster_number)

  plot_handler(plot_logical = plot, output_path = output_path, plot_call = dendro_plot,
               plot_name = paste('dendrogram', cluster_number, 'clusters', sep = '_'))
}

#' @title Create active binding to plot code in a split screen device
#'
#' @param plot_data sf; a copy of the \code{processed_data} passed to
#' \explore{explore_dendrogram}. \code{processed_data} is an sf object, likely
#' returned from \code{make_cluster_data}. Object should include the features to
#' be clustered and their associated polygon geometries.
#' @param tree hclust; an hclust object returned from \code{ClustGeo::hclustgeo}.
#' @param cluster_number integer; number of clusters to be used.
#'
#' @return A two panel plot with the first panel being the field map colored by
#' cluster and the second panel being a dedrogram with colored rectangles showing
#' the clusters. The colors in the two plots should match.

make_dendrogram_plot <- function(plot_data, tree, cluster_number){
  # to prevent warning in RColorBrewer call
  n_color <- ifelse(cluster_number <= 2, 3, cluster_number)

  split.screen(c(1, 2))
  screen(1)
    # map
  plot(plot_data[, 'clusters'], border = NA, pal = RColorBrewer::brewer.pal(n_color, 'Set1'),
         key.pos = NULL, reset = FALSE, main = 'hierarchical clusters')
  screen(2)
    # dendrogram
  plot(tree, hang = -1, labels = FALSE, main = NULL, xlab = NA, sub = NA)
  rect.hclust(tree, k = cluster_number, border = 'red') # RColorBrewer::brewer.pal(n_color, 'Set1')
  close.screen(all = TRUE)
}

#' @title Tests for appropriate cluster number
#' @export
#'
#' @param processed_data sf; An sf object likely returned from \code{make_cluster_data}.
#' Object should include the features to be clustered and their associated polygon
#' geometries.
#' @param kmax integer; **optional** maximum cluster number to be passed to
#' factoextra::fviz_nbclust and NbClust::NbClust.
#' @param plot logical; **optional** should plot be returned in the current graphics
#' device (likely in RStudio). Default is TRUE.
#' @param output_path string; **optional** path where plot should be saved.
#' Default is NULL. If path is provided, a PDF with the name
#' "suggested_optimal_cluster_number".
#'
#' @return A three panel plot showing the relationship between cluster number and
#' total within sum of squares, silhouette width, and Calinski and Harabasz Index.
#' These results can be used as a guide for choosing cluster number.

explore_cluster_number <- function(processed_data, kmax = 10, plot =  TRUE,
                                   output_path = NULL){
  if(kmax < 2){
    stop('kmax must be >= 2')
  }

  no_side_effect_warning(plot, output_path)

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

  cluster_plot %<a-% print(ggpubr::ggarrange(one, two, three))

  plot_handler(plot_logical = plot, output_path = output_path, plot_call = cluster_plot,
               plot_name = 'suggested_optimal_cluster_number')
}

#' @title Make final clusters
#' @export
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
