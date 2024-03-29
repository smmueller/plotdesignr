% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_experiments.R
\name{get_experiment_polygons}
\alias{get_experiment_polygons}
\title{Create polygons for simulation experiment}
\usage{
get_experiment_polygons(
  xy_coords,
  treatment_number,
  plot_length,
  plot_width,
  border_width,
  crs
)
}
\arguments{
\item{xy_coords}{matrix; A matrix with columns \code{x, y} representing the
coordinates of either the centroid of each block (disconnected experiment),
or the centroid of a connected experiment.}

\item{treatment_number}{integer; the number of treatments or plots that should
be included in each block.}

\item{plot_length}{numeric; desired plot length in meters.}

\item{plot_width}{numeric; desired plot width in meters.}

\item{border_width}{numeric; desired width between plots in meters.}

\item{crs}{crs; coordinate referense system to be applied to new polygons.}
}
\value{
An sf object containing the polygons for each plot in an experiment.
}
\description{
Create polygons for simulation experiment
}
