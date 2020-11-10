# functions for creating new experiments

#' @return A matrix of the 5 points making up a single plot polygon
get_plot_boundary <- function(plot_length, plot_width, x0, y0){

  x1 <- x0 + plot_width
  y1 <- y0 + plot_length

  one_plot <- rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))
  return(one_plot)
}

# find bottom left corner of a block
find_block_origin <- function(centroid, treatment_number, plot_length, plot_width, border_width){
  # ignore outside borders
  block_width <- treatment_number*plot_width + (treatment_number - 1)*border_width
  block_length <- plot_length

  y0 <- centroid[2] - block_length*0.5
  x0 <- centroid[1] - block_width*0.5

  origin <- c(x0, y0)

  return(origin)
}
#' the centroids or origin need to come with the column names x and y
#' @return list of sf polygons with plot ids (1:treatment_number) the only element in the data frame.
draw_block <- function(treatment_number, plot_length, plot_width, border_width, centroid = NULL,
                       origin = NULL, crs){

  if(is.null(centroid) & is.null(origin)){
    stop('Either block centroid or origin must be supplied')
  }
  if(!is.null(centroid) & !is.null(origin)){
    stop('Both block centroid and origin has been provided. Please only provide one.')
  }

  if(!is.null(centroid)){
    # find x and y starting points based on the desired centroids
    origin <- find_block_origin(centroid, treatment_number, plot_length, plot_width, border_width)
  }

  plot_list <- lapply(1:treatment_number, function(i){
    if(i == 1){
      temp <- get_plot_boundary(plot_length, plot_width, x0 = origin['x'], y0 = origin['y'])
    }else{
      new_plot_width <- plot_width*(i - 1)
      new_border_width <- border_width*(i - 1)
      temp <- get_plot_boundary(plot_length, plot_width, x0 = origin['x'] +
                                  new_plot_width + new_border_width, y0 = origin['y'])
    }
    one_plot_geom <- st_polygon(list(temp)) %>% st_sfc(crs = crs)
    one_plot <- st_sf(plot = i, geom = one_plot_geom)
    return(one_plot)
  })

  one_block <- do.call(rbind, plot_list)
  return(one_block)
}

# find bottom left of each block if only the experiment center is given
#' @param centroid center of the whole experiment
find_experiment_origins <- function(centroid, rows, cols,
                               treatment_number, plot_length, plot_width, border_width){

  # size of a single block. ignore outside borders
  block_width <- treatment_number*plot_width + (treatment_number - 1)*border_width
  block_length <- plot_length

  # size of the whole experiment. only consider inner borders between blocks
  experiment_width <- block_width*cols + border_width*(cols - 1)
  experiment_length <- block_length*rows + border_width*(rows - 1)


  b1y0 <- centroid[2] - experiment_length*0.5
  b1x0 <- centroid[1] - experiment_width*0.5

  # find bottom left for first column (all x is b1x0)
  y <- vector(length = length(1:rows))
  y[1] <- b1y0
  for(i in 2:rows){
    prev <- i - 1
    y[i] <- y[prev] + block_length + border_width
  }

  # find bottom left for first row (all y is b1y0)
  x <- vector(length = length(1:cols))
  x[1] <- b1x0
  for(i in 2:cols){
    prev <- i - 1
    x[i] <- x[prev] + block_width + border_width
  }

  # find all combinations
  origin_matrix <- as.matrix(expand.grid(x = x, y = y))

  return(origin_matrix)
}

#https://stackoverflow.com/questions/51282724/creating-a-regular-polygon-grid-over-a-spatial-extent-rotated-by-a-given-angle
# rotation around a single point
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
# rotate a whole experiment by a give angle
rotate_experiment <- function(original_experiment, rotation_angle){
  rotated <- (st_geometry(original_experiment) -
              st_centroid(st_union(original_experiment))) * rot(rotation_angle * pi / 180) +
              st_centroid(st_union(original_experiment))

  # add block and plot assignments
  rotated_dat <- st_sf(data.frame(st_drop_geometry(original_experiment),
                                  geom = rotated %>% st_sfc(crs = st_crs(original_experiment))))

  return(rotated_dat)
}



