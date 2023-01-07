pacman::p_load(rayshader, tidyverse, ggplot2)


#First, create simulated lat/long data
set.seed(2009)
moss_landing_coord = c(36.806807, -121.793332)
x_vel_out = -0.001 + rnorm(1000)[1:300]/1000
y_vel_out = rnorm(1000)[1:300]/200
z_out = c(seq(0,2000,length.out = 180), seq(2000,0,length.out=10),
          seq(0,2000,length.out = 100), seq(2000,0,length.out=10))

bird_track_lat = list()
bird_track_long = list()
bird_track_lat[[1]] = moss_landing_coord[1]
bird_track_long[[1]] = moss_landing_coord[2]
for(i in 2:300) {
  bird_track_lat[[i]] = bird_track_lat[[i-1]] + y_vel_out[i]
  bird_track_long[[i]] = bird_track_long[[i-1]] + x_vel_out[i]
}


#Render the 3D map 
montereybay %>%
  sphere_shade() %>%
  plot_3d(montereybay,zscale=50,water=TRUE,
          shadowcolor="#40310a", watercolor="#233aa1", background = "tan",
          theta=210,  phi=22, zoom=0.20, fov=55)


#Pass in the extent of the underlying raster (stored in an attribute for the montereybay
#dataset) and the latitudes, longitudes, and altitudes of the track.
render_path(extent = attr(montereybay,"extent"), 
            lat = unlist(bird_track_lat), long = unlist(bird_track_long), 
            altitude = z_out, zscale=50,color="white", antialias=TRUE)
render_snapshot()
render_path(extent = attr(montereybay,"extent"), 
            lat = unlist(bird_track_lat), long = unlist(bird_track_long), 
            altitude = 10, zscale=50, color="black", antialias=TRUE)
render_camera(theta=30,phi=35,zoom=0.45,fov=70)
render_snapshot()



## ggplot with rayshader
rgl::clear3d()

plotTSNE(sce.sperm, colour_by="pseudotime", point_alpha=0.3) 
plot_gg(gg,multicore=TRUE,width=5,height=5,scale=1) 
render_snapshot()
htmlwidgets::saveWidget(rgl::rglwidget(), "tmp.html")
