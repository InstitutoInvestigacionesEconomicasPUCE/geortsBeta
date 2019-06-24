#' @title  Empirical Distribution Function
#' @description It returns the empirical distribution function based on a vector containig observations of a random variable
#' @param Xrand is a vector with observations of a random variable
#' @param x is a vector which will be evaluate the Empirical Distribution function based on \code{Xrand}
#' @return a vector which contain the values of empirical distribution function evaluated in \code{x}
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
