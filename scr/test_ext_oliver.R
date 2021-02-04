library(sf)
library(raster)
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
UpperLeft <- c(2580000.000, 5470000.000) #( 33d12'41.84"W, 67d12'40.29"N)
LowerLeft <- c( 2580000.000, 1330000.000) ##(  8d36'11.52"W, 33d 6'30.79"N)
UpperRight <- c( 6140000.000, 5470000.000) #( 54d37'45.89"E, 66d47'29.93"N)
LowerRight <- c( 6140000.000, 1330000.000) #( 29d24'43.30"E, 32d56' 8.20"N)
center <- data.frame(x=4360000.000,y=3400000.000)

x_min <- UpperLeft[1]
x_max <- UpperRight[1]
y_min <- LowerRight[2]
y_max <- UpperRight[2]

ext_m <- extent(matrix(c(x_min, x_max, y_min, y_max), 2, 2, byrow = T))
center_p <- SpatialPointsDataFrame(data=center, coords = center, proj4string = crs(eu_bnd))
matrix(ext_m)-matrix(extent(eu_bnd))
ext_r <- raster(ext_m)
values(ext_r) <- 1
crs(ext_r) <- crs(eu_bnd)
writeRaster(x = ext_r, "../QGIS/bnd_oliver_r.tif")
plot(ext_m)
plot(eu_bnd, add=T)
plot(center_p, add=T)
