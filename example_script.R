devtools::load_all()

# what to do with these global options?
# default_margin <- par()$mar
# par(mar=rep(0,4))
# palette(RColorBrewer::brewer.pal(8, 'Set1'))

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

################################################################################
# 1. Create a single data frame to cluster -------------------------------------
################################################################################
cluster_df <- make_cluster_data(path = Path, files = Files, file_ids = File_ids,
                                grid_field_name = Grid_field_name,
                                var_of_interest = Var_of_interest,
                                harvest_width = combine_width, alpha = Alpha,
                                passes_to_clip = Passes_to_clip,
                                cellsize_scaler = Cellsize_scaler)
plot(cluster_df, border =  NA)

################################################################################
# 2. Choose number of Clusters -------------------------------------------------
################################################################################
# view dendrogram to see where clusters might naturally break
explore_dendrogram(processed_data = cluster_df, cluster_number = 3)

# look at some tests to suggest cluster number
explore_cluster_number(processed_data = cluster_df, kmax = 6)

# view how mixing results to choose mixing parameter (alpha in ClustGeo::hclustgeo)
explore_best_mix(processed_data = cluster_df, cluster_number = 4, range = seq(0, 0.4, 0.1))

# finalize cluster number and mixing parameter choice
cluster_ln <- finalize_clusters(processed_data = cluster_df, cluster_number = 4, mixing_parameter = 0.2)
plot(cluster_ln[, 'cluster'], border = NA, axes = T, pal = palette())

################################################################################
# 3. Set up simulation experiment ----------------------------------------------
################################################################################
plot_l <- 300/3.281
plot_w <- 45/3.281
border_w <- 15/3.281
treatment_n <- 4
block_n <- 4

# 3.1 Create disconnected experiment
beta <- make_experiment(experiment_type = 'disconnected',
                        clustered_sf = cluster_ln, n_locations = block_n,
                        treatment_number = treatment_n, plot_length = plot_l,
                        plot_width = plot_w, border_width = border_w,
                        crs = st_crs(cluster_ln), rotation_angle = -95)

# 3.2 Create traditional (connected) experiment
traditional <- make_experiment(experiment_type = 'connected',
                               clustered_sf = cluster_ln, n_locations = 1,
                               treatment_number = treatment_n, plot_length = plot_l,
                               plot_width = plot_w, border_width = border_w,
                               crs = st_crs(cluster_ln), rotation_angle = -95,
                               block_rows = 2, block_cols = 2)

################################################################################
# 4. Run simulations -----------------------------------------------------------
################################################################################

sim_data <- get_test_data(path = Path, file = '2012.shp', var_of_interest = 'Yld_Vol_Dr')
sim_data <- sim_data %>% filter(Yld_Vol_Dr < 100)
# plot(sim_data %>% filter(Yld_Vol_Dr < 100), pch = 19, cex = 0.3)

# 4.2 Reshape data to run ANOVA
# combine the 2 experiments into 1 df
simulation_polys <- rbind(traditional %>% mutate(id = 'traditional'),
                          beta %>% mutate(id = 'beta'))

# find intersection between yield data and experiment polygons
masked_temp_field <- st_intersection(simulation_polys %>% st_set_agr('constant'),
                                     sim_data %>% st_set_agr('constant'))
# plot(masked_temp_field2[, c('block', 'Yld_Vol_Dr')], pch =  19, cex = 0.5)

# get plot means for ANOVA
sim_df_means <- masked_temp_field %>%
  st_drop_geometry() %>%
  group_by(block, plot, id) %>%
  summarize(yield = mean(Yld_Vol_Dr))
# add treatment
set.seed(1113)
sim_df_means <- sim_df_means %>% group_by(block, id) %>% mutate(treatment = sample(1:4, n()))

# make sure there are no differences before adding treatment. Also need the lmerMod
# object for powerSim
# lmer is loaded with simr
library(simr)
library(parallel)
library(ggplot2)

lmer_list <- lapply(unique(sim_df_means$id), function(exp_id){
  exp_lmer <- lmer(yield ~ treatment + (1| block), data = sim_df_means %>% filter(id == exp_id))
})
names(lmer_list) <- unique(sim_df_means$id)
lapply(lmer_list, summary)

# 4.3 Simulation with simr

ef <- seq(0.5, 3, 0.5)
nsim <- 100

cl <- makeCluster(detectCores() - 1, type = 'FORK')
clusterSetRNGStream(cl, iseed = 1113)
sim_list <- lapply(lmer_list, function(lmer_res){
  power_res <- parLapply(cl, ef, function(i){
    fixef(lmer_res)['treatment'] <- i
    ef_pval <- powerSim(lmer_res, nsim = nsim, progress = FALSE)$pval
    # convert to power instead of pvalue
    ef_power <- 1 - ef_pval

    return(ef_power)
  })
  names(power_res) <- ef
  power_df <- data.frame(power_res)

  return(power_df)
})
stopCluster(cl)

# arrange data for plotting
power_df <- data.table::rbindlist(sim_list, idcol = 'source')
power_df_long <- data.table::melt(power_df, id.vars = 'source', variable.name = 'effect_size', value.name = 'power')
power_df_long[, effect_size := gsub('X', '', effect_size)]

plot_df <- power_df_long[, .('power' = mean(power),
                             'upper' = quantile(power, 0.975),
                             'lower' = quantile(power, 0.025)), by = .(source, effect_size)]

ggplot(plot_df, aes(x = effect_size, y = power, col = source, group = source)) +
  geom_hline(aes(yintercept = 0.8)) +
  geom_point(position = position_dodge(width = 0.2), size = 2) +
  geom_line(alpha = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.2)) +
  ylim(0, 1) +
  theme_classic()




