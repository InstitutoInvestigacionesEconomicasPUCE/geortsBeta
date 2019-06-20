rts_plotClean=function(TS, k=1 ){
  require(ggplot2)
  
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
  ggplot(data = x_cln, 
         aes_string(x = "Dates", 
                    y = "x")) +
    geom_line(size = 1,
              color="red"
    ) + 
    geom_line(data = x , size = 1
    ) +
    theme_minimal()
  
}
