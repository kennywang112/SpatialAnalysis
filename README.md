# Spatial Analysis in R

This repository contains a suite of R scripts designed for spatial data analysis, visualization, and modeling, focusing on socioeconomic and environmental datasets. Below is a description of each file and its purpose:

------------------------------------------------------------------------

### `LoadData.R`

Handles data import and preprocessing: 
- Loads census and house price data. 
- Merges multiple demographic datasets. 
- Creates `SpatialPointsDataFrame` and `sf` objects for further spatial analysis.

------------------------------------------------------------------------

### `Basic.R`

Performs basic exploratory data analysis: 
- Summary statistics (mean, median, correlation). 
- Violin plots and bubble plots. 
- Linear regression and confidence intervals. 
- Plot correlation matrix heatmap.

------------------------------------------------------------------------

### `Densities.R`

Conducts Kernel Density Estimation (KDE): 
- Computes KDE for house point data. 
- Visualizes density using `tmap` and `raster`. 
- Extracts 75%, 50%, and 25% home range contours using `adehabitatHR`.

------------------------------------------------------------------------

### `GIS.R`

Spatial integration and basic geographic operations: 
- Spatial join between house points and census polygons. 
- Computes average house price by area. 
- Creates buffer zones around points. 
- Visualizes spatial layers with various tmap layers.

------------------------------------------------------------------------

### `GWR.R`

Geographically Weighted Regression (GWR): 
- Fits linear and **GWR** models. 
- Maps local regression coefficients and R-squared. 
- Includes side-by-side map grid visualizations for multiple variables.

------------------------------------------------------------------------

### `Interpolation.R`

Performs spatial interpolation: 
- Generates **Thiessen** (Voronoi) polygons. 
- Uses **IDW (Inverse Distance Weighting)** to interpolate house prices. 
- Visualizes interpolation outputs as rasters and bubble overlays.

------------------------------------------------------------------------

### `Maps.R`

Choropleth mapping of census data: 
- Maps variables like qualification using different color schemes. 
- Adds legends, compass, layout adjustments. 
- Demonstrates quantile-based classification.

------------------------------------------------------------------------

### `Maps2.R`

Advanced mapping with point symbols: 
- Dot and bubble maps of house price. 
- Combined visualizations with census polygons. 
- Shows proportional symbol maps with clean layouts.

------------------------------------------------------------------------

### `Pollution.R`

Spatiotemporal air pollution analysis (Taiwan PM10): 
- Processes hourly pollution data and monitoring station metadata. 
- Constructs a `STFDF` object for space-time analysis. 
- Fits spatiotemporal **variogram** and performs **kriging**. 
- Visualizes predictions and base maps.

------------------------------------------------------------------------

### `SpatialAutocorrelation.R`

Spatial autocorrelation and clustering: 
- Global and local **Moran's I** computations. 
- **LISA** cluster maps (High-High, Low-Low, etc.). 
- **Getis-Ord Gi\*** statistic for hotspot detection. 
- Visualizes autocorrelation using `tmap` and `spdep`.
