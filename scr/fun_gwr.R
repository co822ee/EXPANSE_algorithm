gwr <- function(train_data, test_data, eu_bnd, cellsize, slr_csvname, local_crs){

   sp_train <- sp::SpatialPointsDataFrame(data = train_data,
                                          coords = cbind(train_data$Xcoord, train_data$Ycoord),
                                          proj4string = local_crs)
   sp_test <- sp::SpatialPointsDataFrame(data = test_data,
                                         coords = cbind(test_data$Xcoord, test_data$Ycoord),
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
   DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                   rp.locat=coordinates(sp_train))
   
   # Calibrate bandwidth using CV
   
   # Build GWR
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_", slr_csvname, '.csv'))
   eq <- as.formula(paste0('NO2_2010~',  paste(slr$variables[-1], collapse = "+")))
   summary(lm(eq, data=train_data))
   nngb <- bw.gwr(eq, data=sp_train, approach = "CV",
                  kernel = "exponential", adaptive = T, dMat = DM_1)
   gwr.res.t <- gwr.basic(eq,
                          data=sp_train, 
                          regression.points=grd2, 
                          adaptive = T,
                          bw=nngb, 
                          dMat=DM,
                          kernel='exponential')
   
   gwr.res.t
}

