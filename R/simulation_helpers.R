#' Get a yields to use in simulation
#' @title get_test_data
#'
#' @param path string; directory where the files are stored
#' @param files string; names of files to be opened within the directory given by
#' \code{path}.
#' @param file_ids string; names to assign to new columns. Must be in the same
#' order as \code{files}.
#' @param var_of_interest string; column name that should be retained for
#' simulation experiments. Should be either length 1, if the needed name is the
#' in all files, or length(files).
#'
#' @return A named list of sf data frames of the original point values for the resquested
#' columns, with the CRS converted to UTM.

get_test_data <- function(path, files, file_ids, var_of_interest){

  # expand var_of_interest if not the same length as files
  if(length(var_of_interest) > 1 & length(var_of_interest) != length(files)){
    stop('var_of_interest must be either length 1 or length(files)')
  }
  if(length(var_of_interest) != length(files)){
    var_of_interest <- rep(var_of_interest, length(files))
    warning('Assuming the same var_of_interest for all files')
  }

  # read in data
  fields <- get_all_files(path, files, file_ids)

  # subset data to only variable of interest.
  # update name to all be the same as the first var_of_interest
  fields_sub <- lapply(1:length(fields), function(f){
    temp_field <- fields[[f]] %>% select(var_of_interest[f])
    if(var_of_interest[f] != var_of_interest[1]){
      temp_field <- temp_field %>% rename(!!var_of_interest[1] := var_of_interest[f])
      warning(var_of_interest[f], ' has been renamed to ', var_of_interest[1], ' to allow for binding.')
    }
    return(temp_field)
  })
  names(fields_sub) <- file_ids

  return(fields_sub)
}

#' Subset simulation data to experimental polygons
#' @title get_experiment_data
#'
#' @param experiment_list named list; named list of sf objects returned
#' from \code{make_experiment} describing experiment polygons. Must be named with
#' the strings that should be used as ID's.
#' @param simulation_data sf; A named list of sf objects returned from get_test_data.
#' This is the data that will be used in simulation experiments.
#'
#' @return An sf object of the simulation_data point observatoins, subset to only
#' though intersecting with the polygons in experiment_list. Each point is
#' identified by id (name of experiment from experiment_list), block, and plot.
#' If experiments overlap, the overlapping points will be once for each experiment.

get_experiment_data <- function(experiment_list, simulation_data){
  # both inputs lists must be named
  if(is.null(names(experiment_list))){
    stop('experiment_list must be a named list')
  }
  if(is.null(names(simulation_data))){
    stop('simulation_data must be a named list')
  }

  # add id columns and bind into single sf
  sim_polys_list <- lapply(names(experiment_list), function(exp){
    experiment_list[[exp]] %>% mutate(experiment_id = exp)
  })
  sim_polys <- do.call(rbind, sim_polys_list)

  # find intersection between simulation data and experiment polygons
  experiment_data_list <- lapply(names(simulation_data), function(sim){
    temp_data <- st_intersection(sim_polys %>% st_set_agr('constant'),
                                 simulation_data[[sim]] %>% st_set_agr('constant'))
    temp_data <- temp_data %>% mutate(file_id = sim)

  })
  experiment_data <- do.call(rbind, experiment_data_list)

  # add a unique identifier for each combination
  experiment_data <- experiment_data %>% mutate(unique_id = paste(experiment_id, file_id, sep = '_'))

  return(experiment_data)
}


