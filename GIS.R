library(tmap)
library(leaflet)
library(sp)
source('LoadData.R')

House.Points_sf <- st_as_sf(House.Points) # 增加地理資訊後轉回sf格式

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

tm_shape(OA.Census) + tm_fill(col = "Price", style = "quantile", title = "Mean House Price (£)")

model <- lm(OA.Census@data$Price ~ OA.Census@data$Unemployed)
summary(model)

# create 200m buffers for each house point
house_buffers <- st_buffer(House.Points_sf, dist = 200)
house_buffers_sf <- st_as_sf(house_buffers)

tm_shape(OA.Census) + tm_borders() + 
  tm_shape(house_buffers) + tm_borders(col = "blue") + 
  tm_shape(House.Points) + tm_dots(col = "red")

tmap_options(check.and.fix = TRUE)

union.buffers <- st_union(house_buffers_sf)
tm_shape(OA.Census) + tm_borders() +
  tm_shape(union.buffers) + tm_fill(col = "blue", alpha = .4) + 
  tm_borders(col = "blue") + tm_shape(House.Points) + tm_dots(col = "red")

library(raster) 
library(dismo)
tmap_mode("view")
tm_shape(House.Points) + 
  tm_dots(title = "House Prices (£)", 
          border.col = "black", border.lwd = 0.1, 
          border.alpha = 0.2, col = "Price",
          style = "quantile", palette = "Reds")
tm_shape(House.Points) + 
  tm_bubbles(size = "Price", title.size = "House Prices (£)", 
             border.col = "black", border.lwd = 0.1,
              border.alpha = 0.4, legend.size.show = TRUE)

tm_shape(OA.Census) +
  tm_fill("Qualification", palette = "Reds", 
          style = "quantile", title = "% with a Qualification") + 
  tm_borders(alpha=.4)




