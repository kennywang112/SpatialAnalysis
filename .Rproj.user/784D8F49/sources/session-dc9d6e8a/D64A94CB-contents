# Geographically Weighted Regression 地理加權回歸
library(sf)
library(tmap)

Census.Data <- read.csv("Data/practical_data.csv")
Output.Areas <- st_read("Data/Camden_oa11/Camden_oa11.shp")%>%st_as_sf()
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

# runs a linear model
model <- lm(OA.Census$Qualification ~ OA.Census$Unemployed+OA.Census$White_British)
summary(model)
# we can use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2)) 
plot(model)

resids<-residuals(model)
map.resids <- cbind(OA.Census, resids)
# we need to rename the column header from the resids file # in this case its the 6th column of map.resids names(map.resids)[6] <- "resids"
# maps the residuals using the quickmap function from tmap
qtm(map.resids, fill = "resids")
# GWR
library(spgwr)
coords <- st_coordinates(st_centroid(OA.Census))
#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(OA.Census$Qualification ~ OA.Census$Unemployed +
                          OA.Census$White_British, coords=coords, data=OA.Census, adapt =TRUE)
# run the gwr model
gwr.model = gwr(OA.Census$Qualification ~ 
                  OA.Census$Unemployed+OA.Census$White_British, 
                data = OA.Census, adapt=GWRbandwidth, coords=coords, hatmatrix=TRUE, se.fit=TRUE)

# 每個地理位置結果
results <-as.data.frame(gwr.model$SDF)
names(results) 
# 依照GWR結果進行做圖
gwr.map <- cbind(OA.Census, as.matrix(results))
qtm(gwr.map, fill = "localR2")

# create tmap objects
map1 <- tm_shape(gwr.map) + tm_fill("White_British", n = 5, style = "quantile", title = "White British") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map2 <- tm_shape(gwr.map) + tm_fill("OA.Census.White_British", n = 5, style = "quantile", title = "WB Coefficient") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map3 <- tm_shape(gwr.map) + tm_fill("Unemployed", n = 5, style = "quantile", title = "Unemployed") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map4 <- tm_shape(gwr.map) + tm_fill("OA.Census.Unemployed", n = 5, style = "quantile", title = "Ue Coefficient") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)

library(grid)
library(gridExtra)
# creates a clear grid
grid.newpage()
# assigns the cell size of the grid, in this case 2 by 2 pushViewport(viewport(layout=grid.layout(2,2)))
# prints a map object into a defined cell
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1)) 
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1)) 
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2)) 
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
