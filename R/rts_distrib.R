# RTS Empirical Distribution ----------------
#' It returns the empirical distribution function from a vector containig observations of a random variable
#'
#' @param Xrand is a vector with observations of a random variable
#' @param x is a vector which will be evaluate the Empirical Distribution function
#' @export

rts_distrib = function(Xrand,x=Xrand){
  Xrand = Xrand[!is.na(Xrand)]
  Distrib = c()
  
  for(i in 1:length(x)){
    Indicatriz = as.numeric(Xrand <= x[i])
    Distrib[i] = mean(Indicatriz, na.rm = TRUE)
  }
  return(Distrib)
}