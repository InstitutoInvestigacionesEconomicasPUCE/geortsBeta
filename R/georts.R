#' @title Geo-reconstruction of Time Series
#' @description This function aproximate by interpolation the values of a time series (unknonw), based in the values of near (geographically) time series.
#' @param TS is a object of class "ts" (time series), with \code{ncol()>2}
#' @param positions.TS is a data frame that containing longitude and latitude asociated to time series in TS
#' @param weights.TS is a data frame that containing weights (of influence) of time series in TS.
#' @param positions.RTS is a data frame that containing values of longitude and latitude about each serie for build.
#' @param weights.RTS is a data frame that containing weights (of influence) of time series in RTS.
#' @param D is a distance matrix of all ditances. This should be entered by the user
#' @return returns an object of class "ts", containing the reconstructed time series associate to RTS.positions
#' @export


geoRts = function(TS,positions.TS,weights.TS = NULL,positions.RTS,weights.RTS=NULL,D = NULL){
  n = dim(TS)[2]
  # Distances Matrix
  positions = rbind(positions.TS,positions.RTS)
  if(is.null(weights.TS )) weights.TS = rep(1,dim(positions.TS)[1])
  if(is.null(weights.RTS )) weights.RTS = rep(1,dim(positionsRTS)[1])

  weights = c(weights.TS,weights.RTS)

  # Gravity Flow Matrix
  N = dim(positions)[1]
  G = matrix(NA, nrow = N,ncol = n)
  A = G
  for (i in seq(N)) {
    for(j in seq(n)){
      if(is.null(D)){
        d = sum((positions[i,]-positions.TS[j,])^2)
      }else{
        d = D[i,j]
      }
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


  return(RTS)

}

