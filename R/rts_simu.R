#'--------------- Simulation of Time Series ----------------
#'
#' This function simulates n observation of the random variable Xrand from empirical distribution function 
#' @param Xrand is a list with observations of the randon variable
#' @param n is a interger that represent the observations tmakes 
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
