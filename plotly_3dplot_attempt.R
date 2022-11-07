# calc t_SNE positions
tmp = plotTSNE(sce.sperm, colour_by="pseudotime", point_alpha=0.3) +
  geom_segment(data=grid.df, 
               mapping=aes(x=start.1, y=start.2, xend=end.1, yend=end.2), 
               arrow=arrow(length=unit(0.05, "inches"), type="closed")) +
  theme(legend.position = "none")

# draw t_SNE using plotly
plotly::plot_ly(tmp$data,  
                marker=list(
                    color=tmp$data$colour_by,
                    colorbar=list(title='Colorbar'), colorscale='Viridis')
                ) %>%
  add_markers(~X, ~Y, z =0) %>%
  add_annotations( x = grid.df$end.1,
                   y = grid.df$end.2,
                   z = 0,
                   xref = "x", yref = "y",
                   axref = "x", ayref = "y",
                   text = "",
                   showarrow = T,
                   ax = grid.df$start.1,
                   ay = grid.df$start.2)


