devtools::load_all()

# define arguments
Path <- '/Users/Sarah/Google Drive/Martha - Yield Data/Field LN/'
Files <- c('2010', '2012', '2014') #'2006', '2006'
# file_ids <- NULL # either give id's or they will be filled in with files
Var_of_interest <- c('Yld_Vol_Dr', 'Yld_Vol_Dr', 'Elevation_', 'Yld_Mass_D')

combine_width <- 25/3.281 # ft to meters
Alpha <- 10
Passes_to_clip <- 3 # controls buffer size
Cellsize_scaler <- 2 # controls grid cell size

cluster_df <- make_cluster_data(path = Path, files = Files, var_of_interest = Var_of_interest,
                                harvest_width = combine_width, alpha = Alpha,
                                passes_to_clip = Passes_to_clip,
                                cellsize_scaler = Cellsize_scaler)
plot(cluster_dt)


