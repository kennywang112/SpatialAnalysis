library(tmap)
library(leaflet)
library(sp)
library(raster) 
library(adehabitatHR)

source('LoadData.R')

kde.output <- kernelUD(House.Points, h="href", grid = 1000) 
plot(kde.output)

kde <- raster(kde.output)
projection(kde) <- CRS("+init=EPSG:27700")

# maps the raster in tmap, "ud" is the density variable
tm_shape(kde) + tm_raster()

# Zoom
# creates a bounding box based on the extents of the Output.Areas polygon
bounding_box <- st_bbox(Output.Areas)
# maps the raster within the bounding box
tm_shape(kde, bbox = bounding_box) + tm_raster("ud")

# mask the raster by the output area polygon
# 只保留Areas區域內的結果
masked_kde <- mask(kde, Output.Areas)
# maps the masked raster, also maps white output area boundaries
tm_shape(masked_kde, bbox = bounding_box) + 
  tm_raster(
    col = "ud",
    col.scale = tm_scale_intervals(values = "brewer.yl_gn_bu", style = "quantile", n = 100),
    col.legend.show = FALSE
  ) + 
  tm_shape(Output.Areas) + 
  tm_borders(
    col = "white",
    fill_alpha = 0.3
  ) +
  tm_layout(frame = FALSE)

# compute homeranges for 75%, 50%, 25% of points,
# objects are returned as spatial polygon data frames 
range75 <- getverticeshr(kde.output, percent = 75) 
range50 <- getverticeshr(kde.output, percent = 50) 
range25 <- getverticeshr(kde.output, percent = 25)

range75 <- st_as_sf(range75)
range50 <- st_as_sf(range50)
range25 <- st_as_sf(range25)

# the code below creates a map of several layers using tmap
tm_shape(Output.Areas_sf) + 
  tm_fill(col = "#f0f0f0") + tm_borders(fill_alpha=.8, col = "white") + 
  tm_shape(House.Points_sf) + tm_dots(col = "blue") +
  tm_shape(range75) + tm_borders(fill_alpha=.7, col = "#fb6a4a", lwd = 2) + 
  tm_fill(fill_alpha=.1, col = "#fb6a4a") +
  tm_shape(range50) + tm_borders(fill_alpha=.7, col = "#de2d26", lwd = 2) +
  tm_fill(fill_alpha=.1, col = "#de2d26") +
  tm_shape(range25) + tm_borders(fill_alpha=.7, col = "#a50f15", lwd = 2) +
  tm_fill(fill_alpha=.1, col = "#a50f15") + 
  tm_layout(frame = FALSE)




