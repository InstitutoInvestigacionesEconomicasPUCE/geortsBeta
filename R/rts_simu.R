
#Simulacion
rEmpirica = function(Lista_Xi,n=1){
  Lista_Xi = Lista_Xi[!is.na(Lista_Xi)]
  if(n>0){
    u =runif(n)
    xsim=c()
    for(k in 1:n){
      xsim[k] = min(Lista_Xi[u[k]<=DistribEmpirica(Lista_Xi)],na.rm = TRUE)
    }
    
  }else{
    xsim = NA
    print("Ingrese un n adecuado")
  }
  
  return(xsim)
}