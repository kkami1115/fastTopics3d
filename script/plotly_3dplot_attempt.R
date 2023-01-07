library(tidyverse)
library(plotly)
library(rgl)



# calc t_SNE positions
tmp = plotTSNE(sce.sperm, colour_by="pseudotime", point_alpha=0.3) +
  geom_segment(data=grid.df, 
               mapping=aes(x=start.1, y=start.2, xend=end.1, yend=end.2), 
               arrow=arrow(length=unit(0.05, "inches"), type="closed")) +
  theme(legend.position = "none")




# 地図データの取得
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# plotly mesh3d に地図を描きこむために path データを取得
nc_grob <-  nc |>  ggplot2:::sf_grob()
nc_path <- 
  tibble(
    x = nc_grob$x,
    y = nc_grob$y,
    z = 0,
    path_id = nc_grob$id.lengths |> imap(.f = ~ rep(.y, times = .x)) |> flatten_int()
  ) |> 
  group_by(path_id) |> 
  split(f = ~ path_id)



# Map fastTopics results onto t-SNE plot in 3D
tmp2 = cbind(rownames(fit$L), as.data.frame(fit$L)) %>% pivot_longer(cols = c("k1","k2","k3","k4","k5","k6"))
tmp3 = tmp$data %>% rownames_to_column()
data_raw = full_join(tmp3, tmp2, by = c("rowname"="rownames(fit$L)")) %>% as_tibble() %>% group_by(rowname) %>% mutate(sumvalue = cumsum(value))

base_cube <- 
  cube3d() |> 
  as.tmesh3d() |> 
  scale3d(x = 0.05, y = 0.05, z = 0.5) |> 
  translate3d(x = 0, y = 0, z = 0.5)

add_cube <- function(p, cube, ...) {
  add_mesh(
    p = p,
    x = cube$vb[1,],     y = cube$vb[2,],     z = cube$vb[3,],
    i = cube$it[1,] - 1, j = cube$it[2,] - 1, k = cube$it[3,] - 1,
    ...
  )
}


# 
data_prep <- 
  data_raw |> 
  mutate(
    cube  = pmap(
      list(X, Y,  value) , 
      function(X, Y, value) {
        base_cube |> 
          translate3d(x = X, y = Y, z = value)|> 
          scale3d(x = 1, y = 1, z = 10) 
      }
    )
  ) |> 
  full_join(data.frame(cluster= c("k1","k2","k3","k4","k5","k6" ), topic_colors = topic_colors), by = c("name"="cluster"))



fig <- plot_ly(type = "mesh3d")

# 地図の描画
# for (path_tbl in nc_path) {
#   fig <- add_paths(p = fig, x = path_tbl$x, y = path_tbl$y, z = path_tbl$z, line = list(color = "rgb(0,0,0)"))
# }

# 立方体の描画
for (i in 1:nrow(data_prep)) {
  fig <- add_cube(p = fig, cube = data_prep$cube[[i]], facecolor = rep(data_prep$topic_colors[[i]], 12))
  }

fig |> 
  layout(
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE, scaleanchor = "x"),
      aspectmode = "data"
    ),
    showlegend = FALSE
  )  |> 
  saveWidget("plot_3d/3dbarplot.html", selfcontained = FALSE, title = "fastTopics in 3D")

