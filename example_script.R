devtools::load_all()

# define arguments
Path <- '/Users/Sarah/Google Drive/Martha - Yield Data/Field LN/'
Files <- paste0(c('2010', '2012', '2014'), '.shp')
File_ids <- c('2010', '2012', '2014')
Grid_field_name <- '2010'
Var_of_interest <- rep('Yld_Vol_Dr', length(Files))

combine_width <- 15/3.281 # ft to meters
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
plot(cluster_df, border =  NA)

# 2. Choose number of Clusters
cluster_scaled <- scale(st_drop_geometry(cluster_df))

# including factoextra & ggplot2 in the imports does not actually load ggplot2, and then this fails.
library(factoextra)
fviz_nbclust(cluster_scaled, hcut, method = "wss", nboot = 25)
fviz_nbclust(cluster_scaled, hcut, method = "silhouette", nboot = 25)

Cluster_number <- 2

explore_best_mix(processed_data = cluster_df, cluster_number = 4)

cluster_ln <- finalize_clusters(processed_data = cluster_df, cluster_number = 4, mixing_parameter = 0.2)
plot(cluster_ln[, 'cluster'], border = NA, axes = T)

# 3. Set up simulation experiment
# 3.1 Choose location of simulated experiment
# TODO option for continous experiment (traditional)
clust_sp <- as(cluster_ln %>% dplyr::select(-Yld_Vol_Dr_2010, -Yld_Vol_Dr_2012, -Yld_Vol_Dr_2014), 'Spatial')
sp::plot(clust_sp, col = clust_sp@data$cluster, border =  NA)

block_n <- 4

loc <- graphics::locator(n = block_n, type = 'n')
xyCoords <- cbind(x = loc$x, y = loc$y)
text(xyCoords, labels = 1:block_n, cex = 1.5)

# 3.2 Create block polygon
plot_l <- 200/3.281
plot_w <- 45/3.281
border_w <- 15/3.281
treatment_n <- 4

block_mask_list <- lapply(1:nrow(xyCoords), function(n){
  origin <- xyCoords[n, ]
  block_poly <- draw_block(centroid = xyCoords[n,], treatment_number = treatment_n,
                          plot_length = plot_l, plot_width = plot_w, border_width = border_w)
  block_mask <- st_polygon(block_poly) %>% st_sfc(crs = st_crs(cluster_ln))
  block_mask_sf <- st_sf(block = n, geom = block_mask)
})

block_mask <- do.call(rbind, block_mask_list)
plot(block_mask, col = 'gray', pch = 19, cex = 2, add = T)

# 3.3 Get yields from new blocks
# read in a raw file to get point data
temp_field <- st_read(paste0(Path, '2014.shp'))
temp_field <- temp_field %>% distinct(.keep_all = TRUE)

temp_field <- update_field_crs(temp_field)
# plot(temp_field[, 'Yld_Vol_Dr'], pch = 19, cex = 0.1)
masked_temp_field <- st_intersection(block_mask, temp_field)
plot(masked_temp_field[, c('block', 'Yld_Vol_Dr')], pch =  19, cex = 0.5)
