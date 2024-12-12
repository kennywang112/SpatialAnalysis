library(tmap)
library(leaflet)
library(sf)

Census.Data <-read.csv("practical_data.csv")

Output.Areas <- st_read("Data/Camden_oa11/Camden_oa11.shp")

# OA是區域
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

houses <- read.csv("Data/CamdenHouseSales15.csv")
houses <- houses[,c(1,2,8,9)]

# 2D scatter plot
plot(houses$oseast1m, houses$osnrth1m)

library(sp)
# create a House.Points SpatialPointsDataFrame
# houses[,3:4]為座標軸
House.Points <- SpatialPointsDataFrame(houses[,3:4], houses,
                                      proj4string = CRS("+init=EPSG:27700"))
tm_shape(OA.Census) + tm_borders(alpha=.4)
# creates a coloured dot map
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", palette = "Reds", style = "quantile")
# creates a coloured dot map
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", scale = 1.5, palette = "Reds",
                                   style = "quantile", title = "Price Paid (£)")
# creates a coloured dot map
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", scale = 1.5, palette = "Purples",
                                   style = "quantile", title = "Price Paid (£)") + 
  tm_compass()+ # 指北針
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)
# creates a proportional symbol map
tm_shape(OA.Census) + tm_borders(alpha=.4) + tm_shape(House.Points) +
  tm_bubbles(size = "Price", col = "Price",
             palette = "Blues", style = "quantile", legend.size.show = FALSE,
             title.col = "Price Paid (£)") +
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)

# creates a proportional symbol map
tm_shape(OA.Census) +
  tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% Qualification") +
  tm_borders(alpha=.4) +
  tm_shape(House.Points) + 
  tm_bubbles(size = "Price", col = "Price",
            palette = "Blues", style = "quantile", legend.size.show = FALSE,
            title.col = "Price Paid (£)", border.col = "black", border.lwd = 0.1, border.alpha = 0.1) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)


