#' @title Create a matrix of the points defining a single plot polygon
#'
#' @param plot_length numeric; desired plot length in meters.
#' @param plot_width numeric; desired plot width in meters.
#' @param x0 numeric; the x value of the bottom left (south west) corner of the
#' plot.
#' @param y0 numeric; the y value of the bottom left (south west) corner of the
#' plot.
#'
#' @return A matrix of the five points describing the polygon of a singe plot. There
#' are five points because polygons must be closed with the first and last value
#' repeated.

get_plot_boundary <- function(plot_length, plot_width, x0, y0){

  x1 <- x0 + plot_width
  y1 <- y0 + plot_length

  one_plot <- rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))

  return(one_plot)
}

#' @title Find the bottom left (south west) corner of a block
#'
#' @param centroid numeric vector; named vector of the xy point of the desired
#' block centroid. Must have the names \code{x} and \code{y}.
#' @param treatment_number integer; the number of treatments or plots that should
#' be included in each block.
#' @param plot_length numeric; desired plot length in meters.
#' @param plot_width numeric; desired plot width in meters.
#' @param border_width numeric; desired width between plots in meters.
#'
#' @return A named vector of xy coordinates describing the location of the bottom
#' left (south west) corner of one block.
#'
#' @note \code{border_width} is only used to to create borders between plots inside
#' the block. The block is draw with no external border.

get_block_origin <- function(centroid, treatment_number, plot_length, plot_width,
                             border_width){
  # find the size of the whole block
  block_width <- treatment_number*plot_width + (treatment_number - 1)*border_width
  block_length <- plot_length

  # find the origin
  y0 <- centroid['y'] - block_length*0.5
  x0 <- centroid['x'] - block_width*0.5

  origin <- c(x0, y0)

  return(origin)
}

#' @title Create polygons of a block with plots
#'
#' @param treatment_number integer; the number of treatments or plots that should
#' be included in each block.
#' @param plot_length numeric; desired plot length in meters.
#' @param plot_width numeric; desired plot width in meters.
#' @param border_width numeric; desired width between plots in meters.
#' @param crs crs; coordinate referense system to be applied to new polygons.
#' @param centroid numeric vector; **optional** a named vector of the xy point
#' of the desired block centroid. Must have the names \code{x} and \code{y}.
#' This should be provided if creating a disconnected experiment.
#' @param origin numeric vector; **optional** a named vector of the xy point
#' of the bottom left (south west) corner of the block. Must have the names
#' \code{x} and \code{y}. This should be provided if creating a connected
#' (traditional) experiment.
#'
#' @return A list of sf polygons with plot ids (1:treatment_number).

get_block_polygons <- function(treatment_number, plot_length, plot_width,
                               border_width, crs, origin){

  plot_list <- lapply(1:treatment_number, function(i){
    if(i == 1){
      temp_plot <- get_plot_boundary(plot_length, plot_width, x0 = origin['x'], y0 = origin['y'])
    }else{
      new_plot_width <- plot_width*(i - 1)
      new_border_width <- border_width*(i - 1)
      temp_plot <- get_plot_boundary(plot_length, plot_width, x0 = origin['x'] +
                                  new_plot_width + new_border_width, y0 = origin['y'])
    }
    one_plot_geom <- st_polygon(list(temp_plot)) %>% st_sfc(crs = crs)
    one_plot <- st_sf(plot = i, geom = one_plot_geom)
    return(one_plot)
  })

  one_block <- do.call(rbind, plot_list)

  return(one_block)
}

#' @title Find the bottom left (south west) corner of each block in a connected experiment
#'
#' @param centroid numeric vector; named vector of the xy point of the center of
#' the whole experiment. Must have the names \code{x} and \code{y}.
#' @param treatment_number integer; the number of treatments or plots that should
#' be included in each block.
#' @param plot_length numeric; desired plot length in meters.
#' @param plot_width numeric; desired plot width in meters.
#' @param border_width numeric; desired width between plots in meters.
#' @param block_rows integer; number of rows the blocks should be arranged into.
#' \code{block_rows * block_cols} equals total number of blocks.
#' @param block_cols integer; number of cols the blocks should be arranged into.
#' \code{block_rows * block_cols} equals total number of blocks.
#'
#' @return A matrix of the xy origins (bottom left/south west corner) of each
#' block in an experiment. The matrix columns are named x and y.

get_experiment_origins <- function(centroid, treatment_number, plot_length,
                                   plot_width, border_width, block_rows, block_cols){

  # size of a single block with no outside borders
  block_width <- treatment_number*plot_width + (treatment_number - 1)*border_width
  block_length <- plot_length

  # size of the whole experiment. only consider inner borders between blocks
  experiment_width <- block_width*block_cols + border_width*(block_cols - 1)
  experiment_length <- block_length*block_rows + border_width*(block_rows - 1)

  # origin of the whole experiment
  b1y0 <- centroid[, 'y'] - experiment_length*0.5
  b1x0 <- centroid[, 'x'] - experiment_width*0.5

  # find bottom left for first column (all x is b1x0)
  y <- vector(length = length(1:block_rows))
  y[1] <- b1y0
  if(block_rows > 1){
    for(i in 2:block_rows){
      prev <- i - 1
      y[i] <- y[prev] + block_length + border_width
    }
  }

  # find bottom left for first row (all y is b1y0)
  x <- vector(length = length(1:block_cols))
  x[1] <- b1x0
  if(block_cols > 1){
    for(i in 2:block_cols){
      prev <- i - 1
      x[i] <- x[prev] + block_width + border_width
    }
  }

  # find all combinations
  origin_matrix <- as.matrix(expand.grid(x = x, y = y))

  return(origin_matrix)
}

#' @title Rotation around a single point
#'
#' @param a numeric
#'
#' @return rotated point
#' @note taken from https://r-spatial.github.io/sf/articles/sf3.html

rotate_single <- function(a){
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

#' @title Merge experiment or block polygon and rotate by a give angle
#'
#' @param original_experiment sf; an sf object containing the polygons for each
#' plot and/or blocks. The rotation will occur around the centroid of \code{original_experiment}.
#' Therefore, if each block should be rotated individually, they need to be passed
#' one at a time.
#' @param rotaion_anlge integer; angle by which the original experiment should
#' be rotated.
#'
#' @return The rotated sf object. Will have the same dimentions as \code{original_experiment}
#' but with the POLYGON geometries rotated.
#'
#' @note Taken from https://stackoverflow.com/questions/51282724/creating-a-regular-polygon-grid-over-a-spatial-extent-rotated-by-a-given-angle

rotate_polygon <- function(original_experiment, rotation_angle){
  rotated <- (st_geometry(original_experiment) -
              st_centroid(st_union(original_experiment))) * rotate_single(rotation_angle * pi / 180) +
              st_centroid(st_union(original_experiment))

  # add block and plot assignments
  rotated_dat <- st_sf(data.frame(st_drop_geometry(original_experiment),
                                  geom = rotated %>% st_sfc(crs = st_crs(original_experiment))))

  return(rotated_dat)
}

#' @title Rotate either connected or disconnected experiment by given angle
#'
#' @param original_experiment sf; an sf object containing the polygons for each
#' plot and/or blocks. The rotation will occur around the centroid of \code{original_experiment}.
#' Therefore, if each block should be rotated individually, they need to be passed
#' one at a time.
#' @param rotaion_anlge integer; angle by which the original experiment should
#' be rotated.
#' @param experiment_type string; which experiment type should be created? Current
#' options are "connected", which creates as single traditional experiment and
#' expects the user to select only one centroid, or "disconnected" which expects
#' the user to select the centroid of each block independently.
#'
#' @return The rotated sf object. Will have the same dimentions as \code{original_experiment}
#' but with the POLYGON geometries rotated.

rotate_experiment <- function(original_experiment, rotation_angle, experiment_type){
  # for disconnected experiments, blocks must be rotated seperately
  if(experiment_type == 'disconnected'){
    blocks <- unique(original_experiment$block)
    exp_rot_list <- lapply(blocks, function(b){
      rotate_polygon(original_experiment %>% filter(block == b), rotation_angle = rotation_angle)
    })
    rotated <- do.call(rbind, exp_rot_list)
  } else{
    rotated <- rotate_polygon(original_experiment, rotation_angle = rotation_angle)
  }

  return(rotated)
}

#' @title Choose locations of simulated expeirment
#'
#' @param clustered_sf sf; An sf object with a column \code{cluster} designating
#' which cluster each polygon has been assigned to. Most like returned from
#' \code{finalize_clusters}.
#' @param n_locations integer; number of needed points.
#'
#' @return A matrix with columns \code{x, y} and nrows = \code{n_locations}. Values
#' represent the coordinates of the locations clicked on the map.

choose_location <- function(clustered_sf, n_locations){
  palette(RColorBrewer::brewer.pal(8, 'Pastel1'))
  # convert to sp in order for graphics::locator to find the lat long
  clust_sp <- as(clustered_sf %>% dplyr::select(geometry, cluster), 'Spatial')

  # plot clusters
  sp::plot(clust_sp, col = clust_sp@data$cluster, border =  NA)

  cat('Please choose', n_locations, 'point(s) by clicking on the map.')
  loc <- graphics::locator(n = n_locations, type = 'n')
  xy_coords <- cbind(x = loc$x, y = loc$y)
  palette('default')

  return(xy_coords)
}

#' @title Create polygons for simulation experiment
#'
#' @param xy_coords matrix; A matrix with columns \code{x, y} representing the
#' coordinates of either the centroid of each block (disconnected experiment),
#' or the centroid of a connected experiment.
#' @param treatment_number integer; the number of treatments or plots that should
#' be included in each block.
#' @param plot_length numeric; desired plot length in meters.
#' @param plot_width numeric; desired plot width in meters.
#' @param border_width numeric; desired width between plots in meters.
#' @param crs crs; coordinate referense system to be applied to new polygons.
#'
#' @return An sf object containing the polygons for each plot in an experiment.

get_experiment_polygons <- function(xy_coords, treatment_number, plot_length,
                                    plot_width, border_width, crs){
  # create a list of blocks
  experiment_list <- lapply(1:nrow(xy_coords), function(n){
    block_poly <- get_block_polygons(origin = xy_coords[n, ],
                                     treatment_number = treatment_number,
                                     plot_length = plot_length,
                                     plot_width = plot_width,
                                     border_width = border_width, crs = crs)
    block_poly <- cbind(block = n, block_poly)
  })

  # bind all blocks into one experiment
  experiment <- do.call(rbind, experiment_list)

  return(experiment)
}

#' @title Make an experiment
#' @export
#'
#' @param experiment_type string; which experiment type should be created? Current
#' options are "connected", which creates as single traditional experiment and
#' expects the user to select only one centroid, or "disconnected" which expects
#' the user to select the centroid of each block independently.
#' @param clustered_sf sf; An sf object with a column \code{cluster} designating
#' which cluster each polygon has been assigned to. Most like returned from
#' \code{finalize_clusters}.
#' @param config list; a named list containing all the needed inputs. The following
#' must be included:
#' \itemize{
#'  \item{\code{plot_length} numeric; desired plot length in meters.}
#'  \item{\code{plot_width} numeric; desired plot width in meters.}
#'  \item{\code{border_width} numeric; desired width between plots in meters.}
#'  \item{\code{treatment_number} integer; the number of treatments or plots that should be included in each block.}
#'  \item{\code{block_number} integer; the number of blocks that should be included in each experiment.}
#' }
#' @param plot_name string; name that should be given to the figure. Do not include ".pdf".
#' If no plotting directory has been given in the \code{config}, NULL should be
#' passed.
#' @param rotation_angle integer; **optional** angle by which the original experiment should
#' be rotated. Default is NULL, which will result in plots oriented North-South.
#' @param block_cols integer; **optional** for "connected" experiment, how many
#' columns should the blocks be arranged into? \code{block_cols}*\code{block_rows}
#' should be equal to \code{config$block_number}.
#' @param block_rows integer; **optional** for "connected" experiment, how many
#' rows should the blocks be arranged into? \code{block_cols}*\code{block_rows}
#' should be equal to \code{config$block_number}.
#'
#' @return An sf object containing the polygons for each plot in an experiment.
#'
#' @note The CRS from \code{clustered_sf} will be used as the CRS to draw the polygons,
#' therefore it is important that the CRS is in UTM.

make_experiment <- function(experiment_type, clustered_sf, n_locations,
                            config, plot_name, rotation_angle = NULL,
                            block_cols = NULL, block_rows = NULL){

  if(!(experiment_type %in% c('connected', 'disconnected'))){
    stop('experiment_type must be either "connected" or "disconnected"')
  }
  if(experiment_type == 'connected' && block_cols*block_rows != config$block_number){
    stop('block_cols * block_rows must equal config$block_number')
  }

  # check config inputs
  input_checker(config, 'make_experiment')

  # how many centroids need to be selected?
  n_locations <- ifelse(experiment_type == 'disconnected', config$block_number, 1)

  # store original margins and reset for better plots
  original_mar <- par()$mar
  par(mar = c(1.0, 1.0, 1.2, 1.0))

  locations <- choose_location(clustered_sf = clustered_sf, n_locations = n_locations)

  params_list <- list(treatment_number = config$treatment_number,
                      plot_length = config$plot_length,
                      plot_width = config$plot_width,
                      border_width = config$border_width,
                      crs = st_crs(clustered_sf))

  if(experiment_type == 'disconnected'){
    block_origins <- t(apply(locations, 1, function(row){
      get_block_origin(centroid = row,
                       treatment_number = config$treatment_number,
                       plot_length = config$plot_length,
                       plot_width =config$plot_width,
                       border_width = config$border_width)
    }))
    params_list[['xy_coords']] <- block_origins
  }

  if(experiment_type == 'connected'){
    block_origins <- get_experiment_origins(centroid = locations,
                                            block_rows = block_rows,
                                            block_cols = block_cols,
                                            treatment_number = config$treatment_number,
                                            plot_length = config$plot_length,
                                            plot_width = config$plot_width,
                                            border_width = config$border_width)
    params_list[['xy_coords']] <- block_origins
  }

  exp <- do.call(get_experiment_polygons, params_list)

  if(!is.null(rotation_angle)){
    exp <- rotate_experiment(original_experiment = exp,
                             rotation_angle = rotation_angle,
                             experiment_type = experiment_type)
  }

  plot_exp <- plot_experiment(exp)

  plot_handler(plot_logical = TRUE, output_path = config$output_path,
               plot_call = plot_exp, plot_name = plot_name)

  # reset plot margins
  par(mar = original_mar)
  return(exp)
}

#' @title Plot experiment polygons
#'
#' @param experiment sf; an sf object containing the polygons for each
#' plot and/or blocks.
#'
#' @return Addes the plotted polygons to an existing map of the experiment with
#' labels containing the block numbers.

plot_experiment <- function(experiment){
  num <- lapply(unique(experiment$block), function(b){
    experiment %>% filter(block == b) %>% st_union() %>% st_centroid() %>% st_sf()
  })
  num <- do.call(rbind, num)
  num$block_lab <- as.character(unique(experiment$block))
  plot(experiment[, 'block'], add = T, border = 'black')

  plot(num, pch = 15, cex = 3, col = 'white', add = TRUE)
  plot(num, pch = num$block_lab, cex = 1, col = 'black', add = TRUE)
}
