
# install.package('remotes')
remotes::install_github('coolbutuseless/devout')
remotes::install_github('coolbutuseless/devoutrgl')
remotes::install_github('coolbutuseless/triangular')
remotes::install_github('coolbutuseless/snowcrash')
remotes::install_github('coolbutuseless/cryogenic')
remotes::install_github('coolbutuseless/ggrgl', ref='main')

library(rgl)
library(devout)
library(devoutrgl)
library(ggrgl)
library(ggplot2)
library(dplyr)

# Map fastTopics results onto t-SNE plot in 3D
x_data = tsne_plot$data$X
y_data = tsne_plot$data$Y
z_data1 = fit$L[,1]
z_data2 = fit$L[,1] + fit$L[,2] 
z_data3 = fit$L[,1] + fit$L[,2] + fit$L[,3]
z_data4 = fit$L[,1] + fit$L[,2] + fit$L[,3] + fit$L[,4]
z_data5 = fit$L[,1] + fit$L[,2] + fit$L[,3] + fit$L[,4] + fit$L[,5]
z_data6 = fit$L[,1] + fit$L[,2] + fit$L[,3] + fit$L[,4] + fit$L[,5] + fit$L[,6]


df = NULL
for(i in 1:6){
  if(i == 6){
    df[[i]] = data.frame(names = rownames(fit$L), cluster = i, x = x_data, y = y_data, z = fit$L[,i], xend = x_data, yend = y_data, zend = 1)   
  }else{
    df[[i]] = data.frame(names = rownames(fit$L),cluster = i, x = x_data, y = y_data, z = fit$L[,i], xend = x_data, yend = y_data, zend = fit$L[,i] + fit$L[,i+1])   
  }
}

df_3d = bind_rows(df)



p <- ggplot(df_3d) +
      geom_segment_3d(
        aes( x = x, y = y, z = z, xend = xend, yend = yend, zend = zend, colour = zend),
        alpha = 0.1,
        size = 2) +
      theme_ggrgl() +
      theme(legend.position = 'none') +
      scale_colour_viridis_c(option = 'A') +
      coord_equal()

devoutrgl::rgldev(fov = 30, view_angle = -30, zscale = 3.5)
p
invisible(dev.off())