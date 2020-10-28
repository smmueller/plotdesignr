devtools::load_all()

# define arguments
Path <- '/Users/Sarah/Google Drive/Martha - Yield Data/Field LN/'
Files <- paste0(c('2010', '2012', '2014'), '.shp')
File_ids <- c('2010', '2012', '2014')
Grid_field_name <- '2010'
Var_of_interest <- rep('Yld_Vol_Dr', length(Files))

combine_width <- 25/3.281 # ft to meters
Alpha <- 50
Passes_to_clip <- 3 # controls buffer size
Cellsize_scaler <- 2 # controls grid cell size

# 1. Create a single data frame to cluster
cluster_df <- make_cluster_data(path = Path, files = Files, file_ids = File_ids,
                                grid_field_name = Grid_field_name,
                                var_of_interest = Var_of_interest,
                                harvest_width = combine_width, alpha = Alpha,
                                passes_to_clip = Passes_to_clip,
                                cellsize_scaler = Cellsize_scaler)
plot(cluster_df)

# 2. Choose number of Clusters
cluster_scaled <- scale(st_drop_geometry(cluster_df))

# including factoextra & ggplot2 in the imports does not actually load ggplot2, and then this fails.
library(factoextra)
fviz_nbclust(cluster_scaled, hcut, method = "wss", nboot = 50)
fviz_nbclust(cluster_scaled, hcut, method = "silhouette", nboot = 50)

Cluster_number <- 2

explore_best_mix(processed_data = cluster_df, cluster_number = 4)

cluster_ln <- finalize_clusters(processed_data = cluster_df, cluster_number = 2, mixing_parameter = 0.2)




