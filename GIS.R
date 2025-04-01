library(tmap)
library(leaflet)
library(sp)
library(raster) 
library(dismo)
source('LoadData.R')

pip <- st_join(House.Points_sf, OA.Census_sf) # 依照空間位置將房屋資料與人口資料合併，具體位置以及polygon範圍

House.Points@data <- cbind(House.Points@data, pip)
plot(log(House.Points@data$Price), House.Points@data$Unemployed)

# 獲取每個區域的平均房價
OA <- aggregate(House.Points@data$Price, by = list(House.Points@data$OA11CD), mean)
# change the column names of the aggregated data
names(OA) <- c("OA11CD", "Price")
# join the aggregated data back to the OA.Census polygon
# 加回原本資料
OA.Census@data <- merge(OA.Census@data, OA, by = "OA11CD", all.x = TRUE)

tm_shape(House.Points_sf) +
  tm_fill(
    col = "Price",
    fill.scale = tm_scale_intervals(style = "quantile"),
    fill.legend = tm_legend(title = "Mean House Price (£)")
  )

model <- lm(House.Points@data$Price ~ House.Points@data$osnrth1m)
summary(model)

# create 200m buffers for each house point
house_buffers <- st_buffer(House.Points_sf, dist = 200)
house_buffers_sf <- st_as_sf(house_buffers)

tm_shape(House.Points_sf) + tm_borders() + 
  tm_shape(house_buffers_sf) + tm_borders(col = "blue") + 
  tm_shape(House.Points_sf) + tm_dots(col = "red")

# tmap_options(check.and.fix = TRUE)

union.buffers <- st_union(house_buffers_sf)
tm_shape(OA.Census_sf) + tm_borders() +
  tm_shape(union.buffers) + tm_fill(col = "blue", fill_alpha = .4) + 
  tm_borders(col = "blue") + tm_shape(House.Points_sf) + tm_dots(col = "red")

tmap_mode("view")
tm_shape(House.Points_sf) + 
  tm_dots(
    col = "Price",
    fill.scale = tm_scale_intervals(values = "brewer.reds", style = "quantile"),
    fill.legend = tm_legend(title = "House Prices (£)"),
    border.col = "black",
    border.lwd = 0.1,
    col_alpha = 0.2 
  )


tm_shape(House.Points_sf) + 
  tm_bubbles(size = "Price", title.size = "House Prices (£)", 
             border.col = "black", border.lwd = 0.1,
             col_alpha = 0.4, legend.size.show = TRUE)

tm_shape(OA.Census_sf) +
  tm_fill(
    col = "Qualification",
    fill.scale = tm_scale_intervals(values = "brewer.reds", style = "quantile"),
    fill.legend = tm_legend(title = "% with a Qualification")
  ) +
  tm_borders(fill_alpha = 0.4)




