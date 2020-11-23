#' Create a field boundary from points in an sf object
#' @title get_field_boundary
#'
#' @param field sf; spatial object with points
#' @param alpha numeric; alpha value to be passed to \code{alphahull::ashape}.
#'
#' @return A polygon of the field boundary

get_field_boundary <- function(field, alpha){

  # create a matrix of coordinates and drope any duplicates
  field_coords <- do.call(rbind, st_geometry(field))
  colnames(field_coords) <- c('x', 'y')

  # make an ashape object
  ashape_object <- alphahull::ashape(field_coords, alpha = alpha)

  # make a boundary vector
  bound <- get_ashape_boundary_indices(ashape_object)

  # get the points that define the bondary
  bound_points <- field[bound, 'geometry']

  # convert to a polygon
  poly <- st_cast(st_combine(bound_points), 'POLYGON')

  return(poly)
}

#' Get the coordinate indices the descripe the boundary of an ashape object
#' @title get_ashape_boundary_indices
#'
#' @param ashape ashape; an object returned from alphahull::ashape.
#'
#' @return A vector of indices representing the indices of the xy matrix used to
#' create \code{ashape} that describe the shape's boundary.
#'
#' @note adopted from https://rpubs.com/geospacedman/alphasimple and
#' https://babichmorrowc.github.io/post/2019-03-18-alpha-hull/

get_ashape_boundary_indices <- function(ashape){

  # Convert node numbers into characters
  ashape$edges[,1] <- as.character(ashape$edges[, 1])
  ashape_graph <- igraph::graph_from_edgelist(ashape$edges[, 1:2], directed = FALSE)

  # check that ashape_graph is a single enclosing polygon
  if(!igraph::is.connected(ashape_graph)){
    stop("Graph not connected")
  }
  if(any(igraph::degree(ashape_graph) != 2)){
    stop("Graph not circular")
  }
  if(igraph::clusters(ashape_graph)$no > 1){
    stop("Graph composed of more than one circle")
  }

  # Delete one edge to create a chain
  cut_graph <- ashape_graph - igraph::E(ashape_graph)[1]

  # Find chain end points
  ends = names(which(igraph::degree(cut_graph) == 1))
  path = igraph::get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]

  # this is an index into the points
  pathX = as.numeric(igraph::V(ashape_graph)[path]$name)

  # join the ends
  pathX = c(pathX, pathX[1])

  return(pathX)
}

#' Find the appropriate UTM based on longitude
#' @title get_utm
#'
#' @param long numeric; longitude. Longitude should be given as degrees East.
#' Examples: San Fransico, CA is -122, Sydney Australia is 151.
#' @return UTM zone
#'
#' @note based on https://stackoverflow.com/questions/9186496/determining-utm-zone-to-convert-from-longitude-latitude

get_utm <- function(long) {

  utm <- (floor((long + 180)/6) %% 60) + 1

  return(utm)
}

#' Make polygon grid of field
#' @title get_field_grid
#'
#' @param field sf; an sf object of point geometries.
#' @param alpha numeric; parameter that controls the level of simplication in the
#' field boundary. Larger numbers are more simple and follow the data points less closely.
#' This parameter is passed to \code{alphahull::ashape}.
#' @param harvest_width numeric; width of harvest header, in meters.
#' @param passes_to_clip integer; the number of harvest passes to clip when creating
#' field buffer.
#' @param cellsize_scaler numeric; controls the size of the grid cells. The value
#' of \code{combind_width * cellsize_scaler} is passed to \code{sf::st_make_grid}.
#'
#' @return An sfc_POLYGON object that contains a hexagonal grid of cell size
#' \code{harvest_width * cellsize_scaler}, clipped to the size of the field buffer.
#' The field buffer is the detected field boundardy, with its simplifiction controlled
#' by \code{alpha}, minus \code{harvest_width * passes_to_clip}.
#'
#' @note
#' TODO maybe add ... to give more flexibility in passing arguments?

get_field_grid <- function(field, alpha, harvest_width, passes_to_clip, cellsize_scaler){
  # find the outside pass
  hull <- get_field_boundary(field = field, alpha = alpha)

  # add half the harvester width to the outside pass to create field boundary
  boundary <- st_buffer(hull, harvest_width*0.5)

  # create buffer inside field boundary
  buffer <- st_buffer(boundary, -harvest_width * passes_to_clip)

  # make a grid for the whole field.
  poly_grid <- st_make_grid(boundary, cellsize = harvest_width * cellsize_scaler, square = FALSE)

  # returns logicial of whether each polygon is inside the buffer.
  # only keeps whole polygons (not partial)
  clipped_grid <- st_within(poly_grid, buffer, sparse = FALSE)

  # subset to whole polygons inside the buffer
  poly_grid <- poly_grid[clipped_grid, ]

  return(poly_grid)
}

#' Update field CRS to UTM zone
#' @title update_field_crs
#'
#' @param field sf; an sf object. The longitude of the bbox is used to find the
#' correct UTM zone.
#'
#' @return An updated sf object with a UTM CRS added.
#'
#' @note This is too simple for wide use.
# TODO need a more thorough way of updating zone

update_field_crs <- function(field){
  utm_zone <- get_utm(st_bbox(field)[1]) # st_bbox()[1] will get xmin of the bbox
  utm_crs <- sp::CRS(paste0('+proj=utm +zone=', utm_zone, ' +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))
  temp_utm_field <- st_transform(field, utm_crs)

  return(temp_utm_field)
}

#' Read one or more files
#' @title get_all_files
#'
#' @param path string; directory where the files are stored
#' @param files string; names of files to be opened within the directory given by
#' \code{path}.
#' @param file_ids string; names to assign to new columns. Must be in the same
#' order as \code{files}.
#' @param to_utm logical; should crs be converted to UTM? Default it true.
#'
#' @return A named list of sf objects

get_all_files <- function(path, files, file_ids, to_utm = TRUE){
  # read in each field, remove any duplicated rows, update to UTM
  fields <- lapply(files, function(file){
    temp_field <- st_read(paste0(path, file))
    temp_field <- temp_field %>% distinct(.keep_all = TRUE)

    if(to_utm){
      temp_field <- update_field_crs(temp_field)
    }
    return(temp_field)
  })
  names(fields) <- file_ids

  return(fields)
}

#' Create a single data frame of field attributes from multiple files to use in clustering
#' @title make_cluster_data
#'
#' @param config list; a named list containing all the needed inputs. The following
#' must be included:
#' \itemize{
#'  \item{\code{path} string; directory where the files are stored}
#'  \item{\code{files} string; names of files to be opened within the directory given by \code{path}.}
#'  \item{\code{file_ids} string; names to assign to new columns. Must be in the same order as \code{files}.}
#'  \item{\code{grid_field_name} string; the \code{file_id} of the field that should be used to create the field grid. This should be a file that is representative because it will be used to make the boundary and grid applied to all other fields.}
#'  \item{\code{var_of_interest} string; column name in each file that should be retained in the final combined data frame.}
#'  \item{\code{harvest_width} numeric; width of harvest header, in meters.}
#'  \item{\code{alpha} numeric; parameter that controls the level of simplication in the field boundary. Larger numbers are more simple and follow the data points less closely. This parameter is passed to \code{alphahull::ashape}.}
#'  \item{\code{passes_to_clip} integer; the number of harvest passes to clip when creating field buffer.}
#'  \item{\code{cellsize_scaler} numeric; controls the size of the grid cells. The value of \code{combind_width * cellsize_scaler} is passed to \code{sf::st_make_grid}.}
#'  \item{\code{output_path} string; optional, if provided, plots will be saved to the directory given by this path. This can be helpful because some output plots are large and slow to load in the graphics device.}
#' }
#' @param plot logical; **optional** should a faceted plot of \code{config$var_of_interest}
#' be plotted? Default is TRUE. Note: this only controls whether a plot should be
#' created in the current R Studio graphics device. plot = FALSE will NOT surpress
#' saving plots if an output path has been given in the config list.
#'
#' @return An sf data frame containing the requested columns from each file,
#' named as \code{paste(var_of_interest, file_ids, sep = _)}, aggregated to a common
#' grid of hexagonal polygons of size \code{harvest_width * cellsize_scaler},
#' clipped to the size of the field buffer.
#' The field buffer is the detected field boundardy, with its simplifiction controlled
#' by \code{alpha}, minus \code{harvest_width * passes_to_clip}. The value of each
#' polygon represents the median of the underlying point observations that fell
#' within each polygon in the grid.
#' @export
#'
#' @note
#' TODO can you pass multiple variable names to var_of_interest?
#' TODO a checking function that makes sure all inputs are correct before loading
#' the files.

make_cluster_data <- function(config, plot = TRUE){
  # make sure all needed arguments are present in the config
  input_checker(config, 'make_cluster_data')

  # read in each field, remove any duplicated rows, update to UTM
  fields <- get_all_files(config$path, config$files, config$file_ids)

  # make grid of a single field
  field_grid <- get_field_grid(field = fields[[config$grid_field_name]],
                               alpha = config$alpha,
                               harvest_width = config$harvest_width,
                               passes_to_clip = config$passes_to_clip,
                               cellsize_scaler = config$cellsize_scaler)

  # add field id's to var of interest list
  var_list <- as.list(config$var_of_interest)
  names(var_list) <- config$file_ids

  # aggregate points within the clipped polygons
  # TODO if a filled as >90% NA polygons, delete?
  clipped_fields <- lapply(seq_along(fields), function(f){
    field <- fields[[f]]
    var_name <- names(fields)[f]
    select_col <- var_list[[var_name]]

    agg_yield_poly <- aggregate(field[, select_col], field_grid, median, na.rm = TRUE)

    # update name with file id
    new_name <- paste(select_col, config$file_ids[f], sep = '_')
    agg_yield_poly <- agg_yield_poly %>% rename(!!new_name := all_of(select_col))

    return(agg_yield_poly)
  })

  # join all fields together
  multi_year <- clipped_fields[[1]]
  for(i in 2:length(clipped_fields)){
    multi_year <- st_join(multi_year, clipped_fields[[i]], join = st_equals)
  }

  # remove any NA rows
  all_data <- multi_year %>% filter_at(vars(-geometry), all_vars(!is.na(.)))

  # save image to output path if one has been given
  if(!is.null(config$output_path)){
    save_fig(path = config$output_path, name = 'cluster_data',
             plot_call = plot(all_data, border =  NA))
  }
  # output image to graphics device if plot == TRUE
  if(plot){
    plot(all_data, border =  NA)
  }

  return(all_data)
}
