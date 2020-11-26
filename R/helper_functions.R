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

#' Save a figure as a pdf
#' @title save_fig
#'
#' @param path string; path to the directory where the figure should be saved.
#' If the directory does not already exist, it will be created. If there is
#' already a figure in the directory with the same name, it will be overwritten.
#' @param name string; name that should be given to the figure. Do not include ".pdf".
#' @param plot_call plot; either the code to generate a plot (ex: \code{plot(1:10)})
#' or an active binding to code that generates a plot.
#'
#' @return A saved pdf of the figure created by \code{plot_call} in the directory
#' named by \code{path}.

save_fig <- function(path, name, plot_call){
  # check if output directory exists, if not create it
  if(!dir.exists(path)){
    dir.create(path)
  }

  pdf(paste0(path, name, '.pdf'), width = 5, height = 4)
  plot_call
  trash <- dev.off()
}

#' Single call for saving and displaying plots
#' @title plot_handler
#'
#' @param plot_logical logical; TRUE if a plot should be displayed in the default
#' graphics device, FALSE if no plot should be displayed.
#' @param output_path string;  path to the directory where the figure should be saved.
#' If the directory does not already exist, it will be created. If there is
#' already a figure in the directory with the same name, it will be overwritten.
#' @param plot_call_out plot; either the code to generate a plot (ex: \code{plot(1:10)})
#' or an active binding to code that generates a plot. Should be the same as \code{plot_call_save}.
#' @param plot_call_save plot; either the code to generate a plot (ex: \code{plot(1:10)})
#' or an active binding to code that generates a plot. Should be the same as \code{plot_call_out}.
#' @param plot_name string; name that should be given to the figure. Do not include ".pdf".
#'
#' @return Either a figure output to the default graphics device, a saved pdf,
#' or both.
#'
#' @note The use of two arguments, plot_call_out and plot_call_save is a hack
#' to because there is come kind of scoping or binding problem that would not
#' allow a single plot call argument to be both output and saved in two calls.

# TODO: fix plot_call_out & plot_call_save
plot_handler <- function(plot_logical, output_path, plot_call_out, plot_call_save, plot_name){

  if(plot_logical){
    plot_call_out
  }

  # save image to output path if one has been given
  if(!is.null(output_path)){
    save_fig(path = output_path,
             name = plot_name,
             plot_call = plot_call_save)
  }
}

#' Stop execution if nothing is going to be returned or saved
#' @title no_side_effect_warning
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
