#' @title Map representation of Geographical Time Series (on a grid)
#' @description This function build an dynamic map, which represents the geographical time series on a grid
#' @param TS is a ts object
#' @param positions.TS is a data frame that contain longitude and latitude of time series
#' @param k ..................
#' @param weights.TS is a vector that contain the weights of positions
#' @param names.TS is a vector that contain of names of positions
#' @return returns a object of class "leaflet", that contain a animated map with points that represents the time series in it's geographical positions
#' @param type.................
#' @import gganimate
#' @import rayshader
#' @import tidyverse
#' @import rgl
#' @import magick
#' @export

# rts_map_raster = function(TS,positions.TS,weights.TS=NULL,RTS=NULL,positions.RTS=NULL,weights.RTS=NULL,type = c("2D","3D","2D+3D","2D-dynamic","3D-dynamic"), k=1, save.plot=TRUE,file.name="plot"){


rts_map_raster = function(TS,positions.TS,weights.TS=NULL,type = c("2D","3D","2D+3D","2D-dynamic","3D-dynamic"), k=1, fpss=5, windowsize = c(400, 650) ,save.plot=TRUE,file.name="plot", ...){
  # clean Time Series
  type=type[match(type, c("2D","3D","2D+3D","2D-dynamic","3D-dynamic"))]

  if(is.na(type)&&is.na(type)){
    print("Error: Choose a suitable value for 'type' argument")
  }else{
    type = type[1]
    switch(type,
           "2D" = {
             TS = rts_clean(TS)
             if(is.null(RTS)){
               RTS = geoRts(TS = TS,
                            positions.TS = positions.TS,
                            weights.TS = weights.TS,...)
             }
             positions.RTS$xk = as.numeric(RTS[k,])
             positions.RTS$w = 1
             positions.TS$xk = as.numeric(TS[k,])
             positions.TS$w = weights.TS

             pl = ggplot(data=positions.RTS,aes(lon, lat, fill = xk)) +
               geom_raster(interpolate = TRUE) + theme_minimal()+
               geom_point(data=positions.TS,aes(lon,lat,size=w),
                          color='grey',shape=1,show.legend = F)+
               scale_fill_viridis_c(option = "inferno")
           },

           "2D-dynamic" = {
             TS = rts_clean(TS)
             if(is.null(RTS)){
               RTS = geoRts(TS = TS,
                            positions.TS = positions.TS,
                            weights.TS = weights.TS,...)
             }
             positions.RTS$w = 1
             positions.TS$w = weights.TS

             positions.TSAn = data.frame()
             positions.RTSAn = data.frame()

             Dates = seq.Date(from = as.Date(paste(c(start(TS),1),collapse = "/")),
                              by="month" ,length.out = dim(TS)[1] )
             for(k in seq(dim(TS)[1]) ){
               positions.TS$Xt = as.numeric(TS[k,])
               positions.RTS$Xt = as.numeric(RTS[k,])

               positions.TS$Time = Dates[k]
               positions.RTS$Time = Dates[k]

               positions.TSAn = rbind(positions.TSAn,positions.TS)
               positions.RTSAn = rbind(positions.RTSAn,positions.RTS)
             }

             # grafico gganimate ....................................

             pl = ggplot(data=positions.RTSAn,aes(lon, lat, fill = Xt)) +
               geom_raster(interpolate = TRUE) + theme_minimal()+
               geom_point(data=positions.TSAn,aes(lon,lat,size=w),
                          color='grey',shape=1,show.legend = F)+
               scale_fill_viridis_c(option = "inferno")

             pl = pl + transition_time(Time)  +
               labs(title = "Fecha: {frame_time}")

             if(save.plot) anim_save(filename = paste0(file.name,".gif"))
           },

           "3D"={
             TS = rts_clean(TS)
             if(is.null(RTS)){
               RTS = geoRts(TS = TS,
                            positions.TS = positions.TS,
                            weights.TS = weights.TS,...)
             }
             positions.RTS$xk = as.numeric(RTS[k,])
             positions.RTS$w = 1
             positions.TS$xk = as.numeric(TS[k,])
             positions.TS$w = weights.TS

             pl = ggplot(data=positions.RTS,aes(lon, lat, fill = xk)) +
               geom_raster(interpolate = TRUE) + theme_minimal()+
               geom_point(data=positions.TS,aes(lon,lat,size=w),
                          color='grey',shape=1,show.legend = F)+
               scale_fill_viridis_c(option = "inferno")
             pl = plot_gg(pl,...)
           },

           "2D+3D"={
             TS = rts_clean(TS)
             if(is.null(RTS)){
               RTS = geoRts(TS = TS,
                            positions.TS = positions.TS,
                            weights.TS = weights.TS,...)
             }
             positions.RTS$xk = as.numeric(RTS[k,])
             positions.RTS$w = 1
             positions.TS$xk = as.numeric(TS[k,])
             positions.TS$w = weights.TS

             pl = ggplot(data=positions.RTS,aes(lon, lat, fill = xk)) +
               geom_raster(interpolate = TRUE) + theme_minimal()+
               geom_point(data=positions.TS,aes(lon,lat,size=w),
                          color='grey',shape=1,show.legend = F)+
               scale_fill_viridis_c(option = "inferno")
             par(mfrow = c(1, 2))
             plot_gg(pl, raytrace = FALSE, preview = TRUE,...)
             plot_gg(pl,...)
             # render_camera()
             render_snapshot(clear = FALSE)
             par(mfrow = c(1, 1))

           },

           "3D-dynamic"={
             exproot <- 'exports.tmp'
             dir.create(exproot)
             # animation settings
             fpy     <- 5                                          # frames per year
             riseper <- 12                                         # width of the frontal wave (years)
             years   <- 1901:2018                                  # years of data
             maxw    <- 4.5                                        # width of plot in inches
             asprat  <- 9/16                                       # aspect ratio of plot
             anitype <- '3d'                                       # export '2d' or '3d' animation?

             # derivatives
             # frames  <- 1:((length(years)+riseper*2)*fpy)
             frames = seq(as.character(Dates))  #................................ ojo
             n       <- length(frames)

             # raytracing settings
             rays    <- T                                          # use raytracing?
             sangles <- seq(-30,40,length.out=length(frames))      # vector with azimuths
             eangles <- spline(x=c(1,n*0.5,n),                     # vector with elevations angles
                               y=c(10,45,15),
                               xout=frames)$y

             # camera view angles
             thetas  <- spline(x=c(1,n*0.25,n*0.5,n*0.75,n),       # vector with thetas
                               y=c(360,345,335,328,325),
                               xout=frames)$y
             phis    <- spline(x=c(1,n*0.25,n*0.5,n*0.75,n),       # vector with phis
                               y=c(80,45,32,27,25),
                               xout=frames)$y

             # plot defined angle vectors..............
             ggplot(tibble(Frame=frames,
                           Theta=thetas,
                           Phi=phis,
                           Azimuth=sangles,
                           Elevation=eangles) %>%
                      gather('Type','Angle',2:5)) +
               geom_path(aes(Frame,Angle,color=Type),size=1) +
               facet_wrap(~Type, scales='free') +
               guides(color='none')


             # Generate PNG frames ........................................................

             if (anitype=='3d'){rgl.open()}
             for (f in frames){

               # get filename of export frame
               fn       <- file.path(exproot,sprintf('%04d.png',f))

               plc = ggplot(data= positions.RTSAn %>% filter(Time == Dates[f]) ,
                            aes(lon, lat, fill = Xt)) +
                 geom_raster(interpolate = TRUE) +
                 theme_minimal()+
                 geom_point(data=positions.TSAn,
                            aes(lon,lat,size=w),
                            color='grey',shape=1,
                            show.legend = FALSE)+
                 scale_fill_viridis_c(option = "inferno")

               #........................................
               if (anitype=='2d'){
                 ggsave(fn,curplot, width=maxw, height=maxw*asprat)
               }
               if (anitype=='3d'){
                 plot_gg(plc, width=maxw, height=maxw*asprat,
                         height_aes='fill', multicore=T, scale=150,
                         raytrace=rays, sunangle=sangles[f],
                         anglebreaks=seq(eangles[f]-2,eangles[f]+2,0.25),
                         lineantialias=T, windowsize=c(300,250),
                         theta=thetas[f], phi=phis[f], zoom=0.65, fov=42)
                 render_snapshot(filename=fn, clear=T)
               }
             }
             if (anitype=='3d'){rgl.close()}

             # Exportation to GIF -------------------------
             fl = list.files(exproot,pattern="*.png")
             archiv = paste0(exproot,"/",fl)

             #..................
             archiv %>%
               map(image_read) %>% # reads each path file
               image_join() %>% # joins image
               image_animate(fps=fpss) %>% # animates, can opt for number of loops
               image_write(paste0(file.name,".gif"))
           }

    )

  }
  return(pl)
}

