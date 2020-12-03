#' @title Check configuration file
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
    if(!(required[[i]]['name'] %in% names(config))){
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
    if(!which_type(config[[which_name]])){
      type_errors <- append(type_errors, required[[i]]['name'])
      expected <- append(expected, required[[i]]['type'])
      found <- append(found, class(config[[which_name]]))
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

#' @title Save a figure as a pdf
#'
#' @param path string; path to the directory where the figure should be saved.
#' If the directory does not already exist, it will be created. If there is
#' already a figure in the directory with the same name, it will be overwritten.
#' @param name string; name that should be given to the figure. Do not include ".pdf".
#' @param plot_call plot; either the code to generate a plot (ex: \code{plot(1:10)})
#' or an active binding to code that generates a plot.
#' @param plot_logical logical; TRUE if a plot has already been created and
#' displayed in the default graphics device, otherwise FALSE.
#'
#' @return A saved pdf of the figure created by \code{plot_call} in the directory
#' named by \code{path}.

save_fig <- function(path, name, plot_call, plot_logical){
  # check if output directory exists, if not create it
  if(!dir.exists(path)){
    dir.create(path)
  }
  # if plot has already been generated, just copy it. Otherwise create it
  if(plot_logical){
    dev.copy(pdf, paste0(path, name, '.pdf'), width = 5, height = 4)
  } else{
    pdf(paste0(path, name, '.pdf'), width = 5, height = 4)
    plot_call
  }

  trash <- dev.off()
}

#' @title Single call for saving and displaying plots
#'
#' @param plot_logical logical; TRUE if a plot should be displayed in the default
#' graphics device, FALSE if no plot should be displayed.
#' @param output_path string;  path to the directory where the figure should be saved.
#' If the directory does not already exist, it will be created. If there is
#' already a figure in the directory with the same name, it will be overwritten.
#' @param plot_call plot; either the code to generate a plot (ex: \code{plot(1:10)})
#' or an active binding to code that generates a plot.
#' @param plot_name string; name that should be given to the figure. Do not include ".pdf".
#'
#' @return Either a figure output to the default graphics device, a saved pdf,
#' or both.

plot_handler <- function(plot_logical, output_path, plot_call, plot_name){

  if(plot_logical){
    plot_call
  }

  # save image to output path if one has been given
  if(!is.null(output_path)){
    save_fig(path = output_path,
             name = plot_name,
             plot_call = plot_call,
             plot_logical = plot_logical)
  }
}

#' @title Stop execution if nothing is going to be returned or saved
#'
#' @param plot logical; should plot be returned in the current graphics
#' device (likely in RStudio).
#' @param output_path string; path where plot should be saved.
#' Default is NULL. If path is provided, a PDF with the name "dendrogram_k_clusters"
#' will be created, where k is the integer passed to cluster_number.
#'
#' @return If plot = FALSE and output_path = NULL, an error is returned. Otherwise,
#' nothing is returned.

no_side_effect_warning <- function(plot, output_path){
  if(!plot & is.null(output_path)){
    stop('When plot = FALSE and output_path = NULL, nothing will be saved or returned. Function was stopped.')
  }
}
