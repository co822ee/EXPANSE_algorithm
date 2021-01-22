setup_gwr <- function(train_data, eu_bnd, cellsize, local_crs){
   sp_train <- sp::SpatialPointsDataFrame(data = train_data,
                                          coords = cbind(train_data$Xcoord, train_data$Ycoord),
                                          proj4string = local_crs)
   
   xmin <- extent(eu_bnd)[1]
   ymin <- extent(eu_bnd)[3]
   xmax <- extent(eu_bnd)[2]
   ymax <- extent(eu_bnd)[4]
   
   grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                    c(cellsize,cellsize),
                                    c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)))
   
   plot(grd2)
   plot(eu_bnd[1], pch=16, col='firebrick',add=TRUE)
   DM <- gw.dist(dp.locat=coordinates(sp_train),
                 rp.locat=coordinates(grd2))
   list(sp_train, grd2, DM)

}