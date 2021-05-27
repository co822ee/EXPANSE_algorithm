library(sp)
library(sf)
library(raster)
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
cellsize <- 200000

xmin <- extent(eu_bnd)[1]
ymin <- extent(eu_bnd)[3]
xmax <- extent(eu_bnd)[2]
ymax <- extent(eu_bnd)[4]

grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                 c(cellsize,cellsize),
                                 c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)),
                    proj4string = crs(eu_bnd))  ##crs('+init=epsg:3035')
grd2_r <- raster(grd2)
values(grd2_r) <- seq_along(grd2_r)
plot(grd2_r)
plot(eu_bnd[1], col='transparent', add=T)
if(!dir.exists('data/processed/')) dir.create('data/processed')
writeRaster(grd2_r, 'data/processed/grd_gwr_200km.grd')
writeRaster(grd2_r, 'data/processed/grd_gwr_200km.tif')


sta <- read.csv("../EXPANSE_predictor/data/processed/airbase_station_climate.csv")
id_gwr <- extract(grd2_r, st_as_sf(sta, coords = c('xcoord', 'ycoord'), crs=crs(eu_bnd)))
sta <- cbind(sta, id_gwr=id_gwr)
write.csv(sta, 'data/processed/airbase_station_climate_gwrGrd.csv', row.names=F)
