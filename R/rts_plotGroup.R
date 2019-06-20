#'--------------- Plot of Time Series ----------------
#'
#' This function generates a plot with the original and reconstructed series
#' @param TS is a object type ts wiht ncol mayor o equal to 2 and It represents the original series that user establishes
#' @param RTS is a object type ts that that is generated from the reconstruction of series
#' @return a plot with the original series and the reconstructed series are found
#' @export
rts_plotGroup=function(TS,RTS){
  require(ggplot2)
  require(reshape2)
  # require(viridis)
  #Data frame for TS
  
  Dates = seq.Date(from = as.Date(paste(c(start(TS),1),collapse = "/")),
                   by="month" ,length.out = dim(TS)[1] )  
  nbts = colnames(TS)
  nbrts = colnames(RTS)
  
  
  x=as.data.frame(TS)
  x$Dates = Dates
  
  rx=as.data.frame(RTS)
  rx$Dates = Dates
  
  plx = melt(x,id.vars = "Dates",na.rm = FALSE)
  plx$Tipo="Original"
  
  plrx=melt(rx,id.vars = "Dates",na.rm = FALSE)
  plrx$Tipo="Reconstruidas"  
  
  # series = rbind(plx,plrx)
  
  # ......................
  ggplot(data = plrx, 
         aes_string(x = "Dates", 
                    y = "value")) +
    geom_line(size = 0.8,
              aes(color = variable),
              # linetype="dashed",
              alpha=I(0.3)) + 
    geom_line(data = plx , size = 1.1, 
              aes(color = variable)) +
    # geom_point(color="grey") +
    scale_color_grey(start = 0.1, end = 0.1, na.value= "red") + 
    theme_minimal()
  
}





