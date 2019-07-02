# Example GeoRTS ------------------------

rts_data_example = function(m.ts = 8, nx.rts = 5,ny.rts = 5){
	m=m.ts
	p= 50 # large of ts
	x = lapply(round(runif(m,3,20)), FUN = function(x){
		rnorm(p,mean = x,sd=1)
	})
	TS = as.data.frame(matrix(unlist(x) , nrow = p,ncol = m))
	TS = ts(data=TS, start = c(2000,1),frequency = 12)
	TS[15:20,] = NA

	positions.TS = data.frame(lon = rnorm(m,0,10),lat=rnorm(m,30,10))
	weights.TS = round(runif(m,1,10))
	#................................
	rangoX = abs( max(positions.TS$lon)-min(positions.TS$lon) )
	rangoY = abs( max(positions.TS$lat)-min(positions.TS$lat) )
	positions.RTS =expand.grid(lon =seq(from=min(positions.TS$lon)-0.05*rangoX,
										to=max(positions.TS$lon)+0.05*rangoX,
										length.out = nx.rts),
							   lat = seq(from=min(positions.TS$lat)-0.05*rangoX,
							   		  to=max(positions.TS$lat)+0.05*rangoY,
							   		  length.out = ny.rts))

	return(list("TS0" = TS,"positions.TS0" = positions.TS,
				"weights.TS0"=weights.TS,
				"positions.RTS0" = data.frame(positions.RTS)))
}
