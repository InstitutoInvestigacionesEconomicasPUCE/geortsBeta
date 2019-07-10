#' @title Create a hull grid of points
#' @description This function Create a hull grid based on it's positions (\code{positions.TS})
#' @param positions.TS is a data frame that containing longitude (\code{lon}) and latitude (\code{lat}) asociated to time series in TS.
#' @param type.grid Typo of grid to build, available options are: \code{"convex","square"}
#' @param nx.grid is a interger that represents the number of points of longitude \code{log}(new positions) of the grid.
#' @param ny.grid is a interger that represents the number of points of latitude \code{lat}(new positions) of the grid.
#' @import sp
#' @import grDevices
#' @export

rts_hull_grid = function(positions.TS,nx.grid=10*dim(positions.TS)[1], ny.grid=10*dim(positions.TS)[1], type.grid = c("square","convex")) {

  type.grid0 = c("square","convex")
  type.grid=type.grid0[match(type.grid,type.grid0 )]

  if(is.na(type.grid)&&is.na(type.grid)){
    print("Error: Choose a suitable value for 'type.grid' argument, available options are 'convex', and 'square'")
  }else{
    type.grid = type.grid[1]
    switch (type.grid,
      "convex" = {
      # require(grDevices)
      n.grid = nx.grid*ny.grid
      convexInd = grDevices::chull(positions.TS)
      convexInd = c(convexInd, convexInd[1])
      positions.RTS = sp::Polygon(coords =  as.matrix(positions.TS)[convexInd, ])
      positions.RTS = data.frame(sp::spsample(positions.RTS, n.grid, type = "regular"))
      names(positions.RTS) = c("lon","lat")
    },
    "square" = {
      rangoX = abs( max(positions.TS$lon)-min(positions.TS$lon) )
      rangoY = abs( max(positions.TS$lat)-min(positions.TS$lat) )
      positions.RTS = expand.grid(lon =seq(from=min(positions.TS$lon)-0.05*rangoX,
                                           to=max(positions.TS$lon)+0.05*rangoX,
                                           length.out = nx.grid),
                                  lat = seq(from=min(positions.TS$lat)-0.05*rangoX,
                                            to=max(positions.TS$lat)+0.05*rangoY,
                                            length.out = ny.grid))
      positions.RTS = data.frame(positions.RTS)
    })
  }
  return(positions.RTS)

}
