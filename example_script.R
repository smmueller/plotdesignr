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

explore_best_mix(processed_data = cluster_df, cluster_number = 2)

cluster_ln <- finalize_clusters(processed_data = cluster_df, cluster_number = 2, mixing_parameter = 0.1)
plot(cluster_ln[, 'cluster'], border = NA, axes = T)

# 3. Set up simulation experiment
# 3.1 Choose location of simulated experiment
clust_sp <- as(cluster_ln %>% dplyr::select(-Yld_Vol_Dr_2010, -Yld_Vol_Dr_2012, -Yld_Vol_Dr_2014), 'Spatial')
sp::plot(clust_sp, col = clust_sp@data$cluster, border =  NA)

block_n <- 4

loc <- graphics::locator(n = block_n, type = 'n')
xyCoords <- cbind(x = loc$x, y = loc$y)
text(xyCoords, labels = 1:block_n, cex = 1.5, col = 'white')

# 3.2 Create polygons for simulation experiments
# 3.2.1 Create new blocks
plot_l <- 300/3.281
plot_w <- 45/3.281
border_w <- 15/3.281
treatment_n <- 4

block_mask_list <- lapply(1:nrow(xyCoords), function(n){

  block_poly <- draw_block(centroid = xyCoords[n, ], treatment_number = treatment_n,
                           plot_length = plot_l, plot_width = plot_w, border_width = border_w, crs = st_crs(cluster_ln))
  block_poly <- cbind(block = n, block_poly)
  # block_mask <- st_polygon(block_poly) %>% st_sfc(crs = st_crs(cluster_ln))
  # block_mask_sf <- st_sf(block = n, geom = block_mask)
})

block_mask <- do.call(rbind, block_mask_list)
plot(block_mask[, 'block'], add = T)

# 3.2.2 Create traditional experiment
experiment_loc <- graphics::locator(n = 1, type = 'n')
experiment_xyCoords <- cbind(x = experiment_loc$x, y = experiment_loc$y)
text(experiment_xyCoords, labels = 'Exp', cex = 1.5, col = 'white')

block_origins <- find_experiment_origins(centroid = experiment_xyCoords, rows = 2,
                                    cols = 2, treatment_number = treatment_n,
                                    plot_length = plot_l, plot_width = plot_w,
                                    border_width = border_w)

exp_block_mask_list <- lapply(1:nrow(block_origins), function(n){

  block_poly <- draw_block(origin = block_origins[n,], treatment_number = treatment_n,
                           plot_length = plot_l, plot_width = plot_w, border_width = border_w, crs = st_crs(cluster_ln))
  block_poly <- cbind(block = n, block_poly)
})

exp_block_mask <- do.call(rbind, exp_block_mask_list)
plot(exp_block_mask[, 'block'], add = T)

exp_final <- rotate_experiment(exp_block_mask, rotation_angle = -95)
beta_final_list <- lapply(1:block_n, function(bn){
  rotate_experiment(block_mask %>% filter(block == bn), rotation_angle = -95)
})
beta_final <- do.call(rbind, beta_final_list)

plot(exp_final[, 'block'], add = T)
plot(beta_final[, 'block'], add = T)

# combine the 2 experiments into 1 df
simulation_polys <- rbind(exp_final %>% mutate(id = 'traditional'), beta_final %>% mutate(id = 'beta'))

# 3.3 Get yields from new blocks
# read in a raw file to get point data
temp_field <- st_read(paste0(Path, '2014.shp'))
temp_field <- temp_field %>% distinct(.keep_all = TRUE)

temp_field <- update_field_crs(temp_field)
# plot(temp_field[, 'Yld_Vol_Dr'], pch = 19, cex = 0.1)
masked_temp_field <- st_intersection(simulation_polys, temp_field)
plot(masked_temp_field[, c('block', 'Yld_Vol_Dr')], pch =  19, cex = 0.5)

sim_df <- data.table::copy(masked_temp_field)
st_geometry(sim_df) <- NULL
sim_df <- sim_df %>% dplyr::select(block, plot, id, Yld_Vol_Dr)
sim_df_means <- sim_df %>% group_by(block, plot, id) %>% summarize(yield = mean(Yld_Vol_Dr))
sim_df_means <- sim_df_means %>% group_by(block, id) %>% mutate(treat = sample(1:4, n()))

# make sure there are no differences before adding treatment
# lmer is loaded with simr
library(simr)
traditional_lmer <- lmer(yield ~ treat + (1| block), data = sim_df_means %>% filter(id == 'traditional'))
summary(traditional_lmer)

beta_lmer <- lmer(yield ~ treat + (1| block), data = sim_df_means %>% filter(id == 'beta'))
summary(beta_lmer)

# 3.4 Simulation with simr

# change effect size
fixef(traditional_lmer)['treat'] <- 2
fixef(beta_lmer)['treat'] <- 2

power_traditional <- powerSim(traditional_lmer, nsim = 50, progress = FALSE)
power_beta <- powerSim(beta_lmer, nsim = 50, progress = FALSE)

ef <- seq(0.5, 5, 0.5)
nsim <- 100

power_traditional_ls <- lapply(ef, function(i){
  fixef(traditional_lmer)['treat'] <- i
  ef_pval <- powerSim(traditional_lmer, nsim = nsim, progress = FALSE)$pval
  return(ef_pval)
})
names(power_traditional_ls) <- ef

power_beta_ls <- lapply(ef, function(i){
  fixef(beta_lmer)['treat'] <- i
  ef_pval <- powerSim(beta_lmer, nsim = nsim, progress = FALSE)$pval
  return(ef_pval)
})
names(power_beta_ls) <- ef

# convert to power instead of pval
power_traditional_ls <- lapply(power_traditional_ls, function(x) 1 - x)
power_beta_ls <- lapply(power_beta_ls, function(x) 1 - x)

# convert to data table
ex <- list(traditional = as.data.frame(power_traditional_ls),
           beta = as.data.frame(power_beta_ls))
com <- data.table::rbindlist(ex, idcol = 'source')
com_long <- data.table::melt(com, id.vars = 'source', variable.name = 'effect_size', value.name = 'power')

plot_df <- com_long[, .('power' = mean(power),
                        'upper' = quantile(power, 0.975),
                        'lower' = quantile(power, 0.025)), by = .(source, effect_size)]

library(ggplot2)
ggplot(plot_df, aes(x = effect_size, y = power, col = source, group = source)) +
  geom_point(position = position_dodge(width = 0.2), size = 2) +
  geom_line(linetype = 'dashed') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.2)) +
  geom_hline(aes(yintercept = 0.8)) +
  ylim(0, 1) +
  # labs(title = 'Power by Effect Size using 2018 Yields') +
  theme_bw()




