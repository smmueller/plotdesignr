# name and type requirements for all inputs

MAKE_CLUSTER_REQUIRED <- list(c('name' = 'path', 'type' = 'character'),
                              c('name' = 'files', 'type' = 'character'),
                              c('name' = 'file_ids', 'type' = 'character'),
                              c('name' = 'grid_field_name', 'type' = 'character'),
                              c('name' = 'var_of_interest', 'type' = 'character'),
                              c('name' = 'harvest_width', 'type' = 'numeric'),
                              c('name' = 'alpha', 'type' = 'numeric'),
                              c('name' = 'passes_to_clip', 'type' = 'numeric'),
                              c('name' = 'cellsize_scaler', 'type' = 'numeric'))

MAKE_EXPERIMENT_REQUIRED <- list(c('name' = 'plot_length', 'type' = 'numeric'),
                                 c('name' = 'plot_width', 'type' = 'numeric'),
                                 c('name' = 'border_width', 'type' = 'numeric'),
                                 c('name' = 'treatment_number', 'type' = 'numeric'),
                                 c('name' = 'block_number', 'type' = 'numeric'))

usethis::use_data(MAKE_CLUSTER_REQUIRED)
usethis::use_data(MAKE_EXPERIMENT_REQUIRED)
