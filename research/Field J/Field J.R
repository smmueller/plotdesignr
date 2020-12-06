devtools::load_all()

# define arguments
Path <- '/Users/Sarah/Google Drive/Martha - Yield Data/J/'

all_names <- list.files(Path)
Files <- all_names[grepl('Corn.shp', all_names)]
file_ids <- gsub(' Corn.shp', '', Files)

################################################################################
# EXPLORATORY ANALYSIS ---------------------------------------------------------
################################################################################

fields <- lapply(Files, function(file){
  temp_field <- st_read(paste0(Path, file))
  temp_field <- temp_field %>% distinct(.keep_all = TRUE)

  temp_field <- update_field_crs(temp_field)
})
names(fields) <- file_ids

par(mfrow = c(2, 4))
for(i in seq_along(fields)){
  plot(fields[[i]]['Yld_Vol_Dr'], key.pos = NULL, reset = FALSE, pch = 19, cex = 0.1,
       main = names(fields[i]))
}

################################################################################
# PLOTDESIGNR ------------------------------------------------------------------
################################################################################

Var_of_interest <- rep('Yld_Vol_Dr', length(Files))

combine_width <- 15/3.281 # ft to meters
Alpha <- 50
Passes_to_clip <- 4 # controls buffer size
Cellsize_scaler <- 2 # controls grid cell size

# if get errors trying to find the field boundary, increase alpha
cluster_df <- make_cluster_data(path = Path, files = Files,
                                file_ids = file_ids, grid_field_name = '2019',
                                var_of_interest = Var_of_interest,
                                harvest_width = combine_width, alpha = Alpha,
                                passes_to_clip = Passes_to_clip,
                                cellsize_scaler = Cellsize_scaler)
plot(cluster_df[, 6])

# 2. Choose number of Clusters
cluster_scaled <- scale(st_drop_geometry(cluster_df))

# including factoextra & ggplot2 in the imports does not actually load ggplot2, and then this fails.
library(factoextra)
fviz_nbclust(cluster_scaled, hcut, method = "wss", nboot = 25)
fviz_nbclust(cluster_scaled, hcut, method = "silhouette", nboot = 25)

CH <- NbClust::NbClust(cluster_scaled, distance = 'euclidean', method = 'ward.D',
                       min.nc = 2, max.nc = 10, index = 'ch')$All.index
plot(2:10, CH, pch = 19)

Cluster_number <- 3

explore_best_mix(processed_data = cluster_df, cluster_number = Cluster_number, range = seq(0, 0.3, 0.1))

conservative <- finalize_clusters(processed_data = cluster_df, cluster_number = 3, mixing_parameter = 0.1)

# combine clusters 1 and 2 to make the map easier to read
conservative <- conservative %>% mutate(alt_cluster = ifelse(cluster == 3, 3, 1))
plot(conservative[, c('cluster', 'alt_cluster')])

option1 <- conservative %>% group_by(cluster) %>% summarize(geometry = st_union(geometry))
option2 <- conservative %>% group_by(alt_cluster) %>% summarize(geometry = st_union(geometry))
option2 <- st_cast(option2[1:2,], 'MULTIPOLYGON')

st_write(option1, 'research/Field J/results_j/recommendation_j_all.shp')
st_write(option2, 'research/Field J/results_j/recommendation_j_simple.shp')


