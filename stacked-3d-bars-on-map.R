library(tidyverse)
library(plotly)
library(rgl)

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

# 各都市で、1週間のうち雨の降った日につき、立方体を1つ積み上げる (色は曜日に対応)
rains <- 
  nc |> 
  select(NAME, geometry) |> 
  sf::st_centroid() |> 
  as_tibble() |> 
  group_by(NAME) |> 
  mutate(
    longitude = map_dbl(geometry, pluck, 1),
    latitude = map_dbl(geometry, pluck, 2)
  ) |> 
  mutate(
    rain_prop = rlogis(1) |> plogis(),
    is_rainy = map(rain_prop, \(p) rbernoulli(n = 7, p = p))
  ) |> 
  unnest(is_rainy) |> 
  mutate(
    date = 1:7, 
    date_color = scales::viridis_pal()(7),
    rain_days = cumsum(is_rainy)
  ) |> 
  filter(is_rainy) |> 
  mutate(
    cube  = pmap(
      list(longitude, latitude, rain_days), 
      \(lon, lat, day) {
        base_cube |> 
          translate3d(x = lon, y = lat, z = day - 1) |> 
          scale3d(x = 1, y = 1, z = 1/7)
      }
    ),
  )

fig <- plot_ly(type = "mesh3d")

# 地図の描画
for (path_tbl in nc_path) {
  fig <- add_paths(p = fig, x = path_tbl$x, y = path_tbl$y, z = path_tbl$z, line = list(color = "rgb(0,0,0)"))
}

# 立方体の描画
for (i in 1:nrow(rains)) {
  fig <- add_cube(p = fig, cube = rains$cube[[i]], facecolor = rep(rains$date_color[i], 12))
}

fig |> 
  layout(
    scene = list(
      xaxis = list(title = "longitude"),
      yaxis = list(title = "latitude"),
      zaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE, scaleanchor = "x"),
      aspectmode = "data"
    ),
    showlegend = FALSE
  )