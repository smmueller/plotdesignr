#' Check configuration file
#' @title input_checker
#'
#' @param config list; a named list containing all the needed inputs.
#' @param which_function string; string indicating which requirement list should
#' be used. The current options are "make_cluster_data" or "make_experiment".
#'
#' @return If there are any missing names or mismatched types between the requirement
#' list and the config, an error will be returned. Otherwise, nothing is returned.

input_checker <- function(config, which_function){
  # get correct requirement list
  required <- switch(which_function,
                     'make_cluster_data' = MAKE_CLUSTER_REQUIRED,
                     'make_experiment' = MAKE_EXPERIMENT_REQUIRED)

  # check names
  name_errors <- NULL
  for(i in seq_along(required)){
    if(!(required[[i]]['name'] %in% names(inputs_config))){
      name_errors <- append(name_errors, required[[i]]['name'])
    }
  }
  if(length(name_errors) > 0){
    stop('The required input(s): ', paste(name_errors, sep = '', collapse = ', '),
         '\nare missing from the given configuration')
  }

  # check types
  type_errors <- NULL
  expected <- NULL
  found <- NULL
  for(i in seq_along(required)){
    which_type <- switch (required[[i]]['type'],
                          'character' = is.character,
                          'numeric' = is.numeric
    )

    which_name <- required[[i]]['name']
    if(!which_type(inputs_config[[which_name]])){
      type_errors <- append(type_errors, required[[i]]['name'])
      expected <- append(expected, required[[i]]['type'])
      found <- append(found, class(inputs_config[[which_name]]))
    }
  }
  if(length(type_errors) > 0){
    message <- lapply(1:length(type_errors), function(i){
      paste('\nFor the input', type_errors[i],
            'expected type', expected[i],
            'but found type', found[i]
      )})
    stop(message)
  }
}

save_fig <- function(path, name, plot_call){
  # check if output directory exists, if not create it
  if(!dir.exists(path)){
    dir.create(path)
  }

  pdf(paste0(path, name, '.pdf'), width = 5, height = 4)
  plot_call
  invisible(dev.off())
}










