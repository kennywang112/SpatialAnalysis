library(sp)
library(tmap)
library(leaflet)
source('LoadData.R')

# 2D scatter plot
plot(houses$oseast1m, houses$osnrth1m)

tm_shape(OA.Census) + tm_borders(fill_alpha=.4)
# creates a coloured dot map
tm_shape(OA.Census) +
  tm_borders(fill_alpha = 0.4) +
  tm_shape(House.Points_sf) +
  tm_dots(col = "Price", 
          fill.scale = tm_scale_intervals(values = "brewer.reds", style = "quantile"))

# creates a coloured dot map
tm_shape(OA.Census) + 
  tm_borders(fill_alpha = 0.4) +
  tm_shape(House.Points_sf) + 
  tm_dots(
    col = "Price",
    fill.scale = tm_scale_intervals(values = "brewer.reds", style = "quantile"),
    size.scale = tm_scale_continuous(values = 1.5),
    fill.legend = tm_legend(title = "Price Paid (£)")
  )

# creates a coloured dot map
tm_shape(OA.Census) + 
  tm_borders(fill_alpha = 0.4) +
  tm_shape(House.Points_sf) + 
  tm_dots(
    col = "Price",
    fill.scale = tm_scale_intervals(values = "brewer.purples", style = "quantile"),
    size.scale = tm_scale_continuous(values = 1.5),
    fill.legend = tm_legend(title = "Price Paid (£)")
  ) +
  tm_compass() + # 指北針
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)

# creates a proportional symbol map
tm_shape(OA.Census) + 
  tm_borders(fill_alpha = 0.4) + 
  tm_shape(House.Points_sf) +
  tm_bubbles(
    size = "Price",
    col = "Price",
    fill.scale = tm_scale_intervals(values = "brewer.blues", style = "quantile"),
    size.legend.show = FALSE,
    fill.legend = tm_legend(title = "Price Paid (£)")
  ) +
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)


# creates a proportional symbol map
tm_shape(OA.Census_sf) +
  tm_fill(
    col = "Qualification",
    fill.scale = tm_scale_intervals(values = "brewer.reds", style = "quantile"),
    fill.legend = tm_legend(title = "% Qualification")
  ) +
  tm_borders(fill_alpha = 0.4) +
  tm_shape(House.Points_sf) + 
  tm_bubbles(
    size = "Price",
    col = "Price",
    fill.scale = tm_scale_intervals(values = "brewer.blues", style = "quantile"),
    size.legend.show = FALSE,
    fill.legend = tm_legend(title = "Price Paid (£)"),
    border.col = "black",
    col_alpha = 0.1,
    border.lwd = 0.1
  ) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)

