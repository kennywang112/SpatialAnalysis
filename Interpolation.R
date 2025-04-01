library(tmap)
library(spatstat)
library(maptools)
library(raster)
library(gstat) 
library(xts)
source('LoadData.R')

# Create a tessellated surface
dat.pp <- as(dirichlet(as.ppp(House.Points)), "SpatialPolygons") 
dat.pp <- as(dat.pp,"SpatialPolygons")
# House.Points <- as(House.Points, "Spatial")

# Sets the projection to British National Grid
proj4string(dat.pp) <- CRS("+init=EPSG:27700") 
proj4string(House.Points) <- CRS("+init=EPSG:27700")
# Assign to each polygon the data from House.Points
int.Z <- over(dat.pp,House.Points, fn=mean) # Create a SpatialPolygonsDataFrame
thiessen <- SpatialPolygonsDataFrame(dat.pp, int.Z)%>%st_as_sf()
# maps the thiessen polygons and House.Points
tm_shape(Output.Areas) +
  tm_fill(fill = "grey", fill_alpha = 0.3) + 
  tm_shape(thiessen) +
  tm_borders(col = "black", fill_alpha = 0.5) + 
  tm_shape(House.Points_sf) +
  tm_dots(
    col = "blue",
    size.scale = tm_scale_continuous(values = 0.5)
  )

# crops the polygon by our output area shapefile 
thiessen.crop <- st_intersection(thiessen, Output.Areas)
# maps cropped thiessen polygons and House.Points
tm_shape(Output.Areas) + 
  tm_fill(col = "grey", fill_alpha = 0.3) + 
  tm_shape(thiessen.crop) + 
  tm_borders(col = "black", fill_alpha = 0.5) + 
  tm_shape(House.Points_sf) + 
  tm_dots(
    col = "blue",
    size.scale = tm_scale_continuous(values = 0.5)
  )

# maps house prices across thiessen polygons
tm_shape(thiessen.crop) + 
  tm_fill(
    col = "Price", #palette = "brewer.reds", 
    fill.scale = tm_scale_intervals(values = "brewer.reds", style = "quantile"),
    fill.legend = tm_legend(title = "Price Paid (£)")) +
  tm_borders(col = "black", fill_alpha = 0.3) +
  tm_shape(House.Points_sf) + 
  tm_dots(col = "black", size.scale = tm_scale_continuous(values = 0.5)) +
  tm_layout(legend.position = c("left", "bottom"), 
            legend.text.size = 1.05, legend.title.size = 1.2, frame = FALSE)

# define sample grid based on the extent of the House.Points file
grid <-spsample(House.Points, type = 'regular', n = 10000)
# runs the idw for the Price variable of House.Points

idw <- idw(House.Points$Price ~ 1, House.Points, newdata= grid)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("long", "lat", "prediction")

# create spatial points data frame
spg <- idw.output 
coordinates(spg) <- ~ long + lat
# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster 
raster_idw <- raster(spg)
# sets projection to British National Grid
projection(raster_idw) <- CRS("+init=EPSG:27700")
# we can quickly plot the raster to check its okay
plot(raster_idw)
persp(raster_idw)

tm_shape(raster_idw) + 
  tm_raster(
    col = "prediction",
    col.scale = tm_scale_intervals(values = "brewer.reds", style = "quantile", n = 100),
    col.legend.show = FALSE) +
  tm_shape(Output.Areas) + 
  tm_borders(fill_alpha = 0.5)

tm_shape(raster_idw) + 
  tm_raster(
    col = "prediction",
    col.scale = tm_scale_intervals(values = "brewer.reds", style = "quantile", n = 100),
    col.legend.show = FALSE) +
  tm_shape(Output.Areas) + 
  tm_borders(col = "black", fill_alpha = 0.5) +
  tm_shape(House.Points_sf) + 
  tm_bubbles(
    size = "Price", col = "Price",
    fill.scale = tm_scale_intervals(values = "brewer.blues", style = "quantile"),
    size.legend.show = FALSE,
    fill.legend = tm_legend(title = "Price Paid (£)")) +
  tm_layout(legend.position = c("left", "bottom"), 
            legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE, 
            legend.bg.color = "white", legend.bg.alpha = 0.5)

# masks our raster by our output areas polygon file
masked_idw <- mask(raster_idw, Output.Areas)
# plots the masked raster
tm_shape(masked_idw) + 
  tm_raster(
    col = "prediction",
    col.scale = tm_scale_intervals(style = "quantile", n = 100),
    col.legend.show = FALSE) +
  tm_shape(House.Points_sf) + 
  tm_bubbles(
    size = "Price",
    col = "Price",
    fill.scale = tm_scale_intervals(values = "brewer.blues", style = "quantile"),
    size.legend.show = FALSE,
    fill.legend = tm_legend(title = "Price Paid (£)")) +
  tm_layout(legend.position = c("left", "bottom"), legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)


