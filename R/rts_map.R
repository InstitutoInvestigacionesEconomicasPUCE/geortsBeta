#' @title Map representation of Geographical Time Series
#' @description This function build an dynamic map, which represents the geographical time series
#' @param TS is a ts object
#' @param RTS is a ts object
#' @param positions.TS is a data frame that contain longitude and latitude of time series
#' @param weights.TS is a vector that contain the weights of positions
#' @param names.TS is a vector that contain of names of positions
#' @param positions.RTS is a data frame that contain longitude and latitude of reconstructed time series
#' @param weights.RTS is a vector that contain the weights of positions
#' @param names.RTS is a vector that contain of names of positions
#' @return returns a object of class "leaflet", that contain a animated map with points that represents the time series in it's geographical positions
#' @import dplyr
#' @import leaflet
#' @export

rts_map = function(positions.TS,positions.RTS,scale=1,weights.TS=NULL,weights.RTS=NULL,names.TS=NULL,names.RTS=NULL){

  if(is.null(weights.TS)) weights.TS=1
  if(is.null(weights.RTS)) weights.RTS=1
  if(is.null(names.TS)) names.TS = paste0("x",1:dim(positions.TS)[1])
  if(is.null(names.RTS)) names.RTS = paste0("x",1:dim(positions.RTS)[1])

  mapDF = data.frame(rbind(positions.TS,positions.RTS),
                     weights = c(weights.TS,weights.RTS),
                     Serie = c(names.TS,names.RTS),
                     Tipo = c( rep("Tseries",dim(TS)[2]),rep("RTseries",dim(RTS)[2]) )
  )
  pal = leaflet::colorFactor(c("navy", "red"), domain = c("Tseries", "RTseries"))

  map = leaflet::leaflet(mapDF) %>% addTiles() %>%
    addCircles(lng = ~lon, lat = ~lat, weight = 2,
               radius = ~weights.TS*scale,
               color = ~pal(Tipo),
               fillOpacity = 0.3,
               popup = ~Serie,
               stroke = T
    )

  # map
  return(map)
}


