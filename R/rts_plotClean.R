#' @title Plot of Cleaned and original Time Series
#' @description This function generates a plot with the original and reconstructed series
#' @param TS is a object type ts wiht \code{ncol(.)>2} and It represents the original series that user establishes
#' @param k is the number of column (time serie) from TS to be plot.
#' @return a plot with the original series and the reconstructed series are found
#' @import ggplot2
#' @export

rts_plotClean=function(TS, k=1 ){

  TSk=TS[,k]
  TS_clean = rts_clean(TS)
  TSk_clean = TS_clean[,k]
  #........................
  Dates = seq.Date(from = as.Date(paste(c(start(TSk),1),collapse = "/")),
                   by="month" ,length.out = length(TSk) )
  x=as.data.frame(TSk)
  x$Dates = Dates

  x_cln =as.data.frame(TSk_clean)
  x_cln$Dates = Dates
  # ..............
  p1 = ggplot2::ggplot(data = x_cln,
         aes_string(x = "Dates",
                    y = "x")) +
    geom_line(size = 1,
              color="red"
    ) +
    geom_line(data = x , size = 1
    ) +
    theme_minimal()
  # plot(p1)

  return(p1)
}
