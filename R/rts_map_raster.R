#' @title Map representation of Geographical Time Series (on a grid)
#' @description This function build an dynamic map, which represents the geographical time series on a grid
#' @inheritParams geoRts
#' @param type is the type of plot to be shown, available options are: \code{"2D","3D","2D+3D", "2D-dynamic","3D-dynamic"}
#' @param k Represent the number of time serie (column of \code{TS}). Is used only if \code{type} parameter is set as one of following options: \code{"2D","3D"} or \code{"2D+3D"}
#' @param fpss is the number of frames per second. Is only used for types: \code{"2D-dynamic"} or \code{"3D-dynamic"}
#' @param windowsize is a vector that contain width and height
#' @param save.plot set as TRUE for save the plot
#' @param file.name is a character for name of saved plot
#' @param ... Arguments passed to \code{geoRts} or \code{plot_gg} function, from  \code{rayshader} package
#' @return returns a object of class "leaflet", that contain a animated map with points that represents the time series in it's geographical positions
#' @import gganimate
#' @import rayshader
#' @import tidyverse
#' @import dplyr
#' @import ggplot2
#' @import rgl
#' @import magick
#' @export

rts_map_raster = function(TS,positions.TS,weights.TS=NULL,type = c("2D","3D","2D+3D","2D-dynamic","3D-dynamic"), k=1, fpss=5, windowsize = c(700, 700) ,save.plot=TRUE,file.name="plot",nx.grid=10*dim(positions.TS)[1], ny.grid=10*dim(positions.TS)[1], ...){
  # clean Time Series
  type=type[match(type, c("2D","3D","2D+3D","2D-dynamic","3D-dynamic"))]

  if(is.na(type)&&is.na(type)){
    print("Error: Choose a suitable value for 'type' argument, available options are '2D', '3D','2D+3D','2D-dynamic',and '3D-dynamic' ")
  }else{
    type = type[1]
    switch(type,
           "2D" = {
             TS = rts_clean(TS)
             positions.RTS = rts_hull_grid(positions.TS,nx.rts=nx.grid,ny.rts = ny.grid)
             RTS = geoRts(TS = TS,
                          positions.TS = positions.TS,
                          weights.TS = weights.TS,
                          positions.RTS = positions.RTS,...)
             positions.RTS$xk = as.numeric(RTS[k,])
             positions.RTS$w = 1
             positions.TS$xk = as.numeric(TS[k,])
             positions.TS$w = weights.TS

             pl = ggplot2::ggplot(data=positions.RTS,aes(lon, lat, fill = xk)) +
               ggplot2::geom_raster(interpolate = TRUE) + ggplot2::theme_minimal()+
               ggplot2::geom_point(data=positions.TS,aes(lon,lat,size=w),
                                   color='grey',shape=1,show.legend = F)+
               ggplot2::scale_fill_viridis_c(option = "inferno")
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

             pl = ggplot2::ggplot(data=positions.RTSAn,aes(lon, lat, fill = Xt)) +
               ggplot2::geom_raster(interpolate = TRUE) + ggplot2::theme_minimal()+
               ggplot2::geom_point(data=positions.TSAn,aes(lon,lat,size=w),
                                   color='grey',shape=1,show.legend = F)+
               ggplot2::scale_fill_viridis_c(option = "inferno")

             pl = pl + gganimate::transition_time(Time)  +
               labs(title = "Date: {frame_time}")
             pl = gganimate::animate(pl,fps=fpss)

             if(save.plot) gganimate::anim_save(animation = pl,filename = paste0(file.name,".gif"))
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

             pl = ggplot2::ggplot(data=positions.RTS,aes(lon, lat, fill = xk)) +
               ggplot2::geom_raster(interpolate = TRUE) + ggplot2::theme_minimal()+
               ggplot2::geom_point(data=positions.TS,aes(lon,lat,size=w),
                                   color='grey',shape=1,show.legend = F)+
               ggplot2::scale_fill_viridis_c(option = "inferno")
             rayshader::plot_gg(pl,windowsize=windowsize,...)
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

             pl = ggplot2::ggplot(data=positions.RTS,aes(lon, lat, fill = xk)) +
               ggplot2::geom_raster(interpolate = TRUE) + ggplot2::theme_minimal()+
               ggplot2::geom_point(data=positions.TS,aes(lon,lat,size=w),
                                   color='grey',shape=1,show.legend = F)+
               ggplot2::scale_fill_viridis_c(option = "inferno")
             par(mfrow = c(1, 2))
             rayshader::plot_gg(pl, raytrace = FALSE, preview = TRUE,...)
             rayshader::plot_gg(pl,...)
             # render_camera()
             rayshader::render_snapshot(clear = FALSE)
             par(mfrow = c(1, 1))

           },

           "3D-dynamic"={
             warning("ImageMagick must be installed before using '3D-dynamic' option. Create animation could take several minutes, depends on 'windowsize' values.")
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
             ggplot2::ggplot(dplyr::tibble(Frame=frames,
                           Theta=thetas,
                           Phi=phis,
                           Azimuth=sangles,
                           Elevation=eangles) %>%
                      tidyr::gather('Type','Angle',2:5)) +
               ggplot2::ggtitle("Angles of 3D Animation") +
               ggplot2::geom_path(aes(Frame,Angle,color=Type),size=1) +
               ggplot2::facet_wrap(~Type, scales='free') +
               ggplot2::guides(color='none')


             # Generate PNG frames .....................................

             if (anitype=='3d'){rgl.open()}
             for (f in frames){

               # get filename of export frame
               fn       <- file.path(exproot,sprintf('%04d.png',f))

               plc = ggplot2::ggplot(data= positions.RTSAn %>% filter(Time == Dates[f]) ,
                            aes(lon, lat, fill = Xt)) +
                 ggplot2::geom_raster(interpolate = TRUE) +
                 ggplot2::theme_minimal()+
                 ggplot2::geom_point(data=positions.TSAn,
                            aes(lon,lat,size=w),
                            color='grey',shape=1,
                            show.legend = FALSE)+
                 ggplot2::scale_fill_viridis_c(option = "inferno")

               #........................................
               # if (anitype=='2d'){
               #   ggsave(fn,curplot, width=maxw, height=maxw*asprat)
               # }
               if (anitype=='3d'){
                 rayshader::plot_gg(plc, width = maxw, height= maxw*asprat,
                            height_aes = 'fill', multicore=T, scale=150,
                            raytrace=rays, sunangle = sangles[f],
                            anglebreaks = seq(eangles[f]-2,eangles[f]+2,0.25),
                            lineantialias = T, windowsize = windowsize,
                            theta=thetas[f], phi=phis[f], zoom=0.65, fov=42)
                 rayshader::render_snapshot(filename=fn, clear=T)
               }
             }
             if (anitype=='3d'){rgl.close()}

             # Exportation to GIF -------------------------
             fl = list.files(exproot,pattern="*.png")
             archiv = paste0(exproot,"/",fl)

             #..................
             archiv %>%
               purrr::map(image_read) %>% # reads each path file
               magick::image_join() %>% # joins image
               magick::image_animate(fps=fpss) %>% # animates, can opt for number of loops
               magick::image_write(paste0(file.name,".gif"))
             if(!save.plot) unlink(exproot,recursive = TRUE)
           }

    )

  }
  return(pl)
}

