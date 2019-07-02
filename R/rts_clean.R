#' @title Cleanning Function for Time Series
#' @description This function clean a time series with missing values.
#' @param TS is a data frame of time series
#' @param seasonality either the character string "periodic" or the span (in lags) of the loess window for seasonal extraction
#' @import stlplus
#' @export


rts_clean=function(TS, seasonality=12){
  n=dim(TS)[2]
  # k=1
  lambda=1
  TSclean = matrix(NA,nrow = dim(TS)[1],ncol = n)
  for(k in 1:n){
    TSk=TS[,k]

    if(sum(is.na(TS))>0){
      descom= stlplus::stlplus(TSk,s.window = seasonality)

      Tren=descom$data$trend
      Seas=descom$data$seasonal
      Res=descom$data$remainder

      Res[is.na(Res)] = rts_simu(Xrand = Res, n=sum(is.na(Res)))
      TScleank=Tren+Seas+Res

    } else {
      TScleank=TSk
    }
    TSclean[,k] = TScleank
  }#end for
  TSclean=data.frame(TSclean)
  names(TSclean) = colnames(TS)
  TSclean = ts(TSclean,start = start(TS),frequency = frequency(TS))
  return(TSclean)
}

