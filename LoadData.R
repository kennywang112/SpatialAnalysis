library(tidyverse)
library(sf)

Ethnicity <- read.csv("./Data/Camden/tables/KS201EW_oa11.csv")
Rooms <- read.csv("./Data/Camden/tables/KS403EW_oa11.csv")
Qualifications <-read.csv("./Data/Camden/tables/KS501EW_oa11.csv")
Employment <-read.csv("./Data/Camden/tables/KS601EW_oa11.csv")

Ethnicity <- Ethnicity[, c(1, 21)]
Rooms <- Rooms[, c(1, 13)]
Employment <- Employment[, c(1, 20)]
Qualifications <- Qualifications[, c(1, 20)]

names(Ethnicity)<- c("OA", "White_British") 
names(Rooms)<- c("OA", "Low_Occupancy") 
names(Employment)<- c("OA", "Unemployed") 
names(Qualifications)<- c("OA", "Qualification")


Ethnicity%>%
  merge(Rooms, by="OA")%>%
  merge(Employment, by="OA")%>%
  merge(Qualifications, by="OA") -> Census.Data


Output.Areas <- st_read("Data/Camden_oa11/Camden_oa11.shp")
Output.Areas_sf <- st_as_sf(Output.Areas)
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")%>%as("Spatial")
OA.Census_sf <- st_as_sf(OA.Census)

houses <- read.csv("Data/CamdenHouseSales15.csv")[,c(1,2,8,9)]
# create a House.Points SpatialPointsDataFrame
# houses[,3:4]為座標軸
House.Points <- SpatialPointsDataFrame(houses[,3:4], houses,
                                       proj4string = CRS("+init=EPSG:27700"))
House.Points_sf <- st_as_sf(House.Points)