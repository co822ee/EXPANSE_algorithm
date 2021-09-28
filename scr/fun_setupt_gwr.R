setup_gwr <- function(train_data, eu_bnd, cellsize, local_crs, xcoord="Xcoord", ycoord="Ycoord"){
   ## eu_bnd gives the extent of your study area
   sp_train <- sp::SpatialPointsDataFrame(data = train_data,
                                          coords = cbind(train_data[, xcoord], train_data[, ycoord]),
                                          proj4string = local_crs)
   
   xmin <- extent(eu_bnd)[1]
   ymin <- extent(eu_bnd)[3]
   xmax <- extent(eu_bnd)[2]
   ymax <- extent(eu_bnd)[4]
   
   grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                    c(cellsize,cellsize),
                                    c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)),
                       proj4string = local_crs)
   
   # plot(grd2)
   # plot(eu_bnd[1], pch=16, col='firebrick',add=TRUE)
   DM <- gw.dist(dp.locat=coordinates(sp_train),
                 rp.locat=coordinates(grd2))
   list(sp_train, grd2, DM)

}