library(sf)
library(tmap)
# 空間自相關套件
library(spdep)
source('LoadData.R')

House.Points <- House.Points%>%as("Spatial")

tm_shape(OA.Census) + 
  tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification") + 
  tm_borders(alpha=.4)

# 找出鄰近的資料點
neighbours <- poly2nb(OA.Census, queen = TRUE)
# Calculate the Rook's case neighbours
neighbours2 <- poly2nb(OA.Census, queen = FALSE)

coords <- st_coordinates(st_centroid(OA.Census))

par(mfrow=c(1,1)) 

plot(OA.Census, border = 'lightgrey')
plot(OA.Census$geometry, col = 'lightgrey', border = 'white')
plot(neighbours, coords, add=TRUE, col='red')

# compares different types of neighbours
plot(OA.Census, border = 'lightgrey')
plot(OA.Census$geometry, col = 'lightgrey', border = 'white')
plot(neighbours, coords, add=TRUE, col='red') 
plot(neighbours2, coords, add=TRUE, col='blue')

# Convert the neighbour data to a listw object
listw <- nb2listw(neighbours2, style="W") 
listw
# global spatial autocorrelation
moran.test(OA.Census$Qualification, listw)
# creates a moran plot
moran <- moran.plot(OA.Census$Qualification, listw = listw)

# creates a local moran output
# 找出polygon內的局部maran index
local <- localmoran(x = OA.Census$Qualification, listw = nb2listw(neighbours2, style = "W"))

# binds results to our polygon shapefile
# 合併人口資質以及該地區的局部moran index
moran.map <- cbind(OA.Census, local)
# maps the results
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic")

### to create LISA cluster map ###
# Local Indicators of Spatial Association (LISA) 局部空間自相關
# 先建立佔位符
quadrant <- vector(mode="numeric",length=nrow(local))
# centers the variable of interest around its mean
m.qualification <- OA.Census$Qualification - mean(OA.Census$Qualification)
# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])
# significance threshold
signif <- 0.1
# builds a data quadrant
# 一個判斷全局一個判斷局部
quadrant[m.qualification>0 & m.local>0] <- 4
quadrant[m.qualification<0 & m.local<0] <- 1
quadrant[m.qualification<0 & m.local>0] <- 2
quadrant[m.qualification>0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(OA.Census['OA11CD'],border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
# box()
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

## Getis-Ord GI
# creates centroid and joins neighbours within 0 and x units
coords <- st_coordinates(st_centroid(OA.Census))

nb <- dnearneigh(coords,0,800) 
# creates listw
nb_lw <- nb2listw(nb, style = 'B')

# plot the data and neighbours
plot(OA.Census['White_British'], border = 'lightgrey')

plot(OA.Census$geometry, col = 'lightgrey', border = 'white')
plot(nb, coords, add=TRUE, col = 'green')

# compute Getis-Ord Gi statistic
local_g <- localG(OA.Census$Qualification, nb_lw)
local_g <- cbind(OA.Census, as.matrix(local_g))
names(local_g)[6] <- "gstat"
# map the results
tm_shape(local_g) + tm_fill("gstat", palette = "RdBu", style = "pretty") + tm_borders(alpha=.4)

# Moran's I 可用於全局或局部分析，局部表示為LISA
# Getis-Ord 則是用於局部分析，用於找出高低值的集群
# GI 首先構建鄰居，然後計算每個點的值與其鄰居的值之間的相關性，以Z值表示







