# functions for creating new experiments

#' @return A matrix of the 5 points making up a single plot polygon
draw_plot <- function(plot_length, plot_width, x0, y0){

  x1 <- x0 + plot_width
  y1 <- y0 + plot_length

  one_plot <- rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))
  return(one_plot)
}

find_origin <- function(centroid, treatment_number, plot_length, plot_width, border_width){
  y0 <- centroid[2] - plot_length*0.5
  x0 <- centroid[1] - (plot_width * (treatment_number / 2) + (border_width * ((treatment_number / 2) - 0.5)))

  return(list(x0 = x0, y0 = y0))
}

#' @return list of lists. First element of the lists is the plot, sublist is the plot polygon points
draw_block <- function(treatment_number, plot_length, plot_width, border_width, centroid){

  # find x and y starting points based on the desired centroids
  origin <- find_origin(centroid, treatment_number, plot_length, plot_width, border_width)

  plot_list <- lapply(1:treatment_number, function(i){
    if(i == 1){
      temp <- draw_plot(plot_length, plot_width, x0 = origin[['x0']], y0 = origin[['y0']])
      return(temp)
    }
    new_plot_width <- plot_width*(i - 1)
    new_border_width <- border_width*(i - 1)
    temp <- draw_plot(plot_length, plot_width, x0 = origin[['x0']] + new_plot_width + new_border_width, y0 = origin[['y0']])
    return(temp)
  })

  names(plot_list) <- 1:treatment_number # might not have a purpose
  return(plot_list)
}
