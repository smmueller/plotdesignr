devtools::load_all()

ex_config <- read_yaml('~/Repositories/plotdesignr/docs/example_workflow_config.yml')
ex_config$path <- 'docs/example_data/'
ex_config$output_path <- 'docs/example_workflow_plots/'

par(mfrow = c(2, 2))
for(i in seq_along(intro)){
  plot(intro[[i]][, Var_of_interest[i]], pch = 19, cex = 0.1, key.pos = NULL, reset = FALSE,
       breaks = seq(50, 250, 50), main = File_ids[i])
}
rm(intro)
par(mfrow = c(1, 1))

Grid_field_name <- '2018'

combine_width <- 15/3.281 # ft to meters
Alpha <- 50
Passes_to_clip <- 3 # controls buffer size
Cellsize_scaler <- 2 # controls grid cell size

################################################################################
# 1. Create a single data frame to cluster -------------------------------------
################################################################################
cluster_df <- make_cluster_data(ex_config)

################################################################################
# 2. Choose number of Clusters -------------------------------------------------
################################################################################
# view dendrogram to see where clusters might naturally break
explore_dendrogram(processed_data = cluster_df, cluster_number = 4)

# look at some tests to suggest cluster number
explore_cluster_number(processed_data = cluster_df, kmax = 6)

# view how mixing results to choose mixing parameter (alpha in ClustGeo::hclustgeo)
explore_best_mix(processed_data = cluster_df, cluster_number = 4, range = seq(0, 0.3, 0.1))

# finalize cluster number and mixing parameter choice
cluster_d <- finalize_clusters(processed_data = cluster_df, cluster_number = 3, mixing_parameter = 0.1)
cluster_d0 <- finalize_clusters(processed_data = cluster_df, cluster_number = 3, mixing_parameter = 0.4)
# plot(cluster_d[, 'cluster'], border = NA, axes = T)

################################################################################
# 3. Set up simulation experiment ----------------------------------------------
################################################################################

# 3.1 Create disconnected experiment
disconnected_01 <- make_experiment(experiment_type = 'disconnected',
                          clustered_sf = cluster_d, config = ex_config,
                          rotation_angle = NULL, plot_name = 'disconnected_01')

disconnected_04 <- make_experiment(experiment_type = 'disconnected',
                                   clustered_sf = cluster_d0, config = ex_config,
                                   rotation_angle = NULL, plot_name = 'disconnected_04')

# 3.2 Create traditional (connected) experiment
traditional_square <- make_experiment(experiment_type = 'connected',
                               clustered_sf = cluster_d, config = ex_config,
                               rotation_angle = NULL, plot_name = 'traditional_square',
                               block_rows = 2, block_cols = 2)

traditional_long <- make_experiment(experiment_type = 'connected',
                                  clustered_sf = cluster_d, config = ex_config,
                                  rotation_angle = NULL, plot_name = 'traditional_long',
                                  block_rows = 4, block_cols = 1)

# saveRDS(disconnected_01, 'docs/saved_experiments/disconnected_01.rds')
# saveRDS(disconnected_04, 'docs/saved_experiments/disconnected_04.rds')
# saveRDS(traditional_square, 'docs/saved_experiments/traditional_square.rds')
# saveRDS(traditional_long, 'docs/saved_experiments/traditional_long.rds')
################################################################################
# 4. Run simulations -----------------------------------------------------------
################################################################################

# Read in simulation data
sim_data_list <- get_test_data(ex_config)

# Find intersection with experiment polygons
Experiment_List <- list(traditional_square = traditional_square,
                        traditional_long = traditional_long,
                        disconnected_01 = disconnected_01,
                        disconnected_04 = disconnected_04)


masked_field <- get_experiment_data(experiment_list = Experiment_List,
                                    simulation_data = sim_data_list)

# get plot means for ANOVA -- probably not worth putting this in a function?
sim_df_means <- masked_field %>%
  st_drop_geometry() %>%
  group_by(block, plot, unique_id, experiment_id, file_id) %>%
  summarize(yield = mean(Yld_Vol_Dr))
# add treatment
set.seed(1113)
sim_df_means <- sim_df_means %>% group_by(block, unique_id) %>% mutate(treatment = sample(1:4, n()))

# make sure there are no differences before adding treatment. Also need the lmerMod
# object for powerSim
# lmer is loaded with simr
library(simr)
library(parallel)
library(ggplot2)

lmer_list <- lapply(unique(sim_df_means$unique_id), function(id){
  temp_data <- sim_df_means %>% filter(unique_id == id)
  exp_lmer <- lmer(yield ~ treatment + (1| block), data = temp_data)
})
names(lmer_list) <- unique(sim_df_means$unique_id)
lapply(lmer_list, anova)

random_ef <- lapply(names(lmer_list), function(l){
  temp_data <- sim_df_means %>% filter(unique_id == l)
  random <- lmerTest::ranova(lmer_list[[l]])
})
names(random_ef) <- names(lmer_list)

# Simulation with simr
ef <- seq(0.5, 5, 0.5)
nsim <- 300

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
# saveRDS(power_df, 'docs/saved_experiments/power_df.rds')
power_df_long <- data.table::melt(power_df, id.vars = 'source', variable.name = 'effect_size', value.name = 'power')
power_df_long[, effect_size := gsub('X', '', effect_size)]

plot_df <- power_df_long[, .('power' = mean(power),
                             'upper' = quantile(power, 0.975),
                             'lower' = quantile(power, 0.025)), by = .(source, effect_size)]
# plot_df$source <- gsub('trad_', 'trad', plot_df$source)

plot_df[, c('experiment_id', 'file_id') := data.table::tstrsplit(source, "_(?!.*_)", perl = TRUE)]

ggplot(plot_df, aes(x = effect_size, y = power, col = experiment_id, group = experiment_id)) +
  geom_hline(aes(yintercept = 0.8)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_line(alpha = 0.3) +
  # geom_errorbar(data = plot_df[experiment_id %in% c('disconnected_01', 'traditional_square')], aes(ymin = lower, ymax = upper), width = 0.1,
  #               position = position_dodge(width = 0.7), alpha = 0.5) +
  ylim(0, 1) +
  facet_wrap(~ file_id, nrow = 2) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid = element_blank())

ggplot(data = plot_df[experiment_id %in% c('disconnected_01', 'traditional_square')],
       aes(x = effect_size, y = power, col = experiment_id, group = experiment_id)) +
  geom_hline(aes(yintercept = 0.8)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_line(alpha = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1,
                position = position_dodge(width = 0.5), alpha = 0.5) +
  ylim(0, 1) +
  facet_wrap(~ file_id, nrow = 2) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid = element_blank())




