#' @title Simulation based on observations of a random variable
#' @description This function simulates n observations of the random variable, based on the empirical distribution function of it's observations \code{Xrand}.
#' @inheritParams rts_distrib
#' @param n is the number of observations to be simulated
#' @return a simulation of empirical distribucion of Xrand
#' @export
rts_simu = function(Xrand,n=1){
  Xrand = Xrand[!is.na(Xrand)]
  if(n>0){
    u =runif(n)
    xsim=c()
    for(k in 1:n){
      xsim[k] = min(Xrand[u[k]<=rts_distrib(Xrand)],na.rm = TRUE)
    }

  }else{
    xsim = NA
    print("Ingrese un n adecuado")
  }

  return(xsim)
}
