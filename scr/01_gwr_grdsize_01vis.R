source("scr/fun_call_lib.R")
source("scr/fun_read_data.R")
source("scr/fun_setupt_gwr.R")
# train_expanse <- stratified(no2[no2$year==2010,], c('type_of_st', 'zoneID'), 0.8)
# test_expanse <- no2[no2$year==2010,][-train_expanse$index, ]
# setup <- setup_gwr(train_expanse, eu_bnd, 
#                    cellsize = reg_grdsize[i], local_crs = local_crs)
setup_test <- setup_gwr(train_sub, eu_bnd, 
                     cellsize = reg_grdsize[8], local_crs = local_crs) #600km
dm_test <- setup_test[[3]]
grd_test <- setup_test[[2]]
coordinates(grd_test)[1,]

setup_test2 <- setup_gwr(train_sub, eu_bnd, 
                        cellsize = reg_grdsize[6], local_crs = local_crs) #200km
dm_test2 <- setup_test2[[3]]
grd_test2 <- setup_test2[[2]]
coordinates(grd_test2)[1,]
crs(grd_test) <- crs(eu_bnd)
crs(grd_test2) <- crs(eu_bnd)


# Visualize overall
plot(grd_test)
plot(grd_test2, add=T, col='blue')
# points(coordinates(grd_test), pch=5)
text(coordinates(grd_test)[,1], coordinates(grd_test)[,2], seq_along(grd_test))
text(coordinates(grd_test2)[,1], coordinates(grd_test2)[,2], seq_along(grd_test2), cex=0.5)
# points(coordinates(grd_test2), col='blue', pch=16)
plot(eu_bnd[1], pch=16, col='transparent', border='grey', lambda=0.2, add=TRUE)
text(coordinates(grd_test)[,1], coordinates(grd_test)[,2], seq_along(grd_test),
     col='dark green')
plot(grd_test, add=T, col='dark green', lty=2)

#38 for grd_test
#292 for grd_test2
grd_i <- 38
grd_i2 <- 292
coordinates(grd_test)[grd_i,]
coordinates(grd_test2)[grd_i2,]
gw.weight(dm_test[,grd_i], nngb, kernel_type[i], T) %>% hist  #grd_i is the regression point index.
gw.weight(dm_test2[,grd_i2], nngb, kernel_type[i], T) %>% hist  #grd_i is the regression point index.
hist(gw.weight(dm_test[,grd_i], nngb, kernel_type[i], T)-gw.weight(dm_test2[,grd_i2], nngb, kernel_type[i], T))
boxplot(gw.weight(dm_test[,grd_i], nngb, kernel_type[i], T)-gw.weight(dm_test2[,grd_i2], nngb, kernel_type[i], T))

# Visualize (France) # not working
# plot(grd_test)
# plot(eu_bnd[which(eu_bnd[1]$CNTR_ID=="FR"), 1], pch=16, col='transparent', border='grey', main=NULL)
# plot(eu_bnd[eu_bnd$CNTR_ID=="FR",], pch=16, col='transparent', border='grey', main=NULL)
# plot(grd_test2, add=T, col='blue')

DM_test <- setup_test[[3]]
max_w <- lapply(1:ncol(DM_expanse), function(grd_i) gw.weight(DM_expanse[,grd_i], nngb, kernel_type[i], T) %>% max)
hist(unlist(max_w))
