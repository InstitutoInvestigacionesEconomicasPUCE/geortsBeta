#' @title Geo-reconstruction of Time Series
#' @description This function aproximate by interpolation the values of a time series (unknonw), based in the values of near (geographically) time series.
#' @param TS is a object of class "ts" (time series), with \code{ncol()>2}
#' @param positions.TS is a data frame that containing longitude and latitude asociated to time series in TS, each distinct of
#' @param weights.TS is a data frame that containing weights (of influence) of time series in TS.
#' @param positions.RTS is a data frame that containing values of longitude and latitude of each time serie to be reconstructed. If \code{positions.RTS=NULL}, then \code{positions.RTS} is calculated as a grid of points into a square that contains all points of \code{positions.TS}.
#' @param weights.RTS is a data frame that containing weights (of influence) of time series in RTS.
#' @param nx.rts is a interger that represents the number of points of longitude \code{log}(new positions) of the grid.
#' @param ny.rts is a interger that represents the number of points of latitude \code{lat}(new positions) of the grid.
#' @param D is a distance (or disimilarity) matrix between Time Series. If positions.TS is defined and \code{D=NULL} then D is calculated as the euclidean distance matrix between positions (i.e.  between rows of \code{positions.TS}). If \code{positions.TS=NULL}, D must be defined, and then \code{positions.TS} is calculated (with not a geographical representation) using Multidimensional Scaling (MDS).
#' @return returns an object of class "ts", containing the reconstructed time series associate to RTS.positions
#' @import smacof
#' @export


geoRts = function(TS,positions.TS,weights.TS = NULL,positions.RTS=NULL,weights.RTS=NULL,D = NULL,nx.rts=10*dim(TS)[2],ny.rts=10*dim(TS)[2]){
  n = dim(TS)[2]
  if(n>=2){
    #
    if(is.null(weights.TS )) weights.TS = rep(1,dim(positions.TS)[1])
    if(is.null(weights.RTS )) weights.RTS = rep(1,dim(positions.RTS)[1])

    if(is.null(positions.TS)){
      if(is.null(D)){
        print("Error: the distance matrix D must be defined")
      }else{
        # Multidimensional Scaling
        mds = smacof::smacofSym(D,ndim = 2)
        positions.TS = data.frame(mds$conf)
        names(positions.TS) = c("lon","lat")
      }
    }

    if(is.null(positions.RTS)){
      # Grid of reconstruction
      rangoX = abs( max(positions.TS$lon)-min(positions.TS$lon) )
      rangoY = abs( max(positions.TS$lat)-min(positions.TS$lat) )
      positions.RTS =expand.grid(lon =seq(from=min(positions.TS$lon)-0.05*rangoX,
                                          to=max(positions.TS$lon)+0.05*rangoX,
                                          length.out = nx.rts),
                                 lat = seq(from=min(positions.TS$lat)-0.05*rangoX,
                                           to=max(positions.TS$lat)+0.05*rangoY,
                                           length.out = ny.rts))
      positions.RTS = data.frame(positions.RTS)
    }

    positions = rbind(positions.TS,positions.RTS)
    weights = c(weights.TS,weights.RTS)

    # Gravity Flow Matrix
    N = dim(positions)[1]
    G = matrix(NA, nrow = N,ncol = n)
    A = G
    for (i in seq(N)) {
      for(j in seq(n)){
        d = sum((positions[i,]-positions.TS[j,])^2) # Distance
        if(d!=0){
          G[i,j] = (weights[i]*weights[j])/d
        }
      }
    }

    BoolG = is.na(G)
    for(j in 1:dim(G)[2]){
      G[BoolG[,j],-j] = 0
      G[BoolG[,j],j] = 1
    }

    # Standarized Alfa coefficients
    for(i in 1:dim(A)[1]){
      A[i,] = G[i,]/sum(G[i,])
    }

    As = A[(n+1):N,]

    # Reconstruction of Time Series
    RTS = TS%*%t(As)
    RTS = ts(RTS,start = start(TS),frequency = frequency(TS))
    colnames(RTS) = paste0("Serie",(n+1):N)
  }else{
    print("Error: not enough time series for reconstruction, dim(TS)[2] must be at least 2")
  }

  return(RTS)

}

