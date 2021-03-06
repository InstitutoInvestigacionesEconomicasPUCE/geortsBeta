% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rts_map_raster.R
\name{rts_map_raster}
\alias{rts_map_raster}
\title{Map representation of Geographical Time Series (on a grid)}
\usage{
rts_map_raster(TS, positions.TS, weights.TS = NULL, RTS = NULL,
  positions.RTS = NULL, type = c("2D", "3D", "2D+3D", "2D-dynamic",
  "3D-dynamic"), k = 1, fpss = 5, windowsize = c(700, 700),
  save.plot = TRUE, file.name = "plot", nx.grid = 50, ny.grid = 50,
  type.grid = c("square", "convex"), ...)
}
\arguments{
\item{TS}{is a object of class "ts" (time series), with \code{ncol()>2}}

\item{positions.TS}{is a data frame that containing longitude (\code{lon}) and latitude (\code{lat}) asociated to time series in TS.}

\item{weights.TS}{is a data frame that containing weights (of influence) of time series in TS.}

\item{positions.RTS}{is a data frame that containing values of longitude (\code{lon}) and latitude (\code{lat}) of each time serie to be reconstructed. If \code{positions.RTS=NULL}, then \code{positions.RTS} is calculated as a grid of points into a square that contains all points of \code{positions.TS}.}

\item{type}{is the type of plot to be shown, available options are: \code{"2D","3D","2D+3D", "2D-dynamic","3D-dynamic"}}

\item{k}{Represent the number of time serie (column of \code{TS}). Is used only if \code{type} parameter is set as one of following options: \code{"2D","3D"} or \code{"2D+3D"}}

\item{fpss}{is the number of frames per second. Is only used for types: \code{"2D-dynamic"} or \code{"3D-dynamic"}}

\item{windowsize}{is a vector that contain width and height}

\item{save.plot}{set as TRUE for save the plot}

\item{file.name}{is a character for name of saved plot}

\item{nx.grid}{is a interger that represents the number of points of longitude \code{log}(new positions) of the grid.}

\item{ny.grid}{is a interger that represents the number of points of latitude \code{lat}(new positions) of the grid.}

\item{type.grid}{Typo of grid to build, available options are: \code{"convex","square"}}

\item{...}{Arguments passed to \code{geoRts}, or \code{plot_gg} function from  \code{rayshader} package}
}
\value{
returns a object of class "leaflet", that contain a animated map with points that represents the time series in it's geographical positions
}
\description{
This function build an dynamic map, which represents the geographical time series on a grid
}
