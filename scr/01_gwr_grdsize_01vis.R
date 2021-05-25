source("scr/fun_call_lib.R")
source("scr/fun_read_data.R")
source("scr/fun_setupt_gwr.R")

regression_grd_cellsize <- c(10, 20, 50, 80, 100, 200, 500, 600, 1000, 1500, 2000)   #km
reg_grdsize <- regression_grd_cellsize*1000
kernels <- c('exponential')
target_yr <- 2009
n_fold <- 1
no2_e_09_11 <- subset_df_yrs(no2_e_all, target_yr)
# data_all <- no2_e_09_11
print(paste0("year: ", unique(no2_e_09_11$year)))
source("../expanse_multiyear/src/00_fun_create_fold.R")
data_all1 <- create_fold(no2_e_09_11, seed)
test_sub <- data_all1[data_all1$nfold==n_fold,]
train_sub <- data_all1[-test_sub$index, ] #data_all1$index starts from 1 to the length.

#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)

setup_test <- setup_gwr(train_sub, eu_bnd, 
                     cellsize = reg_grdsize[6], local_crs = local_crs) #200
dm_test <- setup_test[[3]]
grd_test <- setup_test[[2]]
coordinates(grd_test)[1,]

setup_test2 <- setup_gwr(train_sub, eu_bnd, 
                        cellsize = reg_grdsize[3], local_crs = local_crs) #50
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

#From 0318_gwr_grdcell we
grd_mp1 <- raster(grd_test) %>% rasterToPolygons() %>% st_as_sf(crs=3035) %>% st_cast("MULTIPOLYGON")
grd_mp2 <- raster(grd_test2) %>% rasterToPolygons() %>% st_as_sf(crs=3035) %>% st_cast("MULTIPOLYGON")
grd_i <- 256
grd_i2 <- st_intersection(grd_mp2, st_as_sf(grd_test)[grd_i, ]) %>% rownames %>% as.numeric#3464
coordinates(grd_test)[grd_i,]
coordinates(grd_test2)[grd_i2,]
nngb <- read.table(paste0("data/workingData/GWR_nngb_run2_", 
                          target_yr, "_fold_", n_fold, ".txt"))[,1]
gw.weight(dm_test[,grd_i], nngb, kernels[1], T) %>% hist  #grd_i is the regression point index.
gw.weight(dm_test2[,grd_i2], nngb, kernels[1], T) %>% hist  #grd_i is the regression point index.
# hist(gw.weight(dm_test[,grd_i], nngb, kernels[1], T)-gw.weight(dm_test2[,grd_i2], nngb, kernels[1], T))
# boxplot(gw.weight(dm_test[,grd_i], nngb, kernels[1], T)-gw.weight(dm_test2[,grd_i2], nngb, kernels[1], T))

# Visualize (France) # not working
# plot(grd_test)
# plot(eu_bnd[which(eu_bnd[1]$CNTR_ID=="FR"), 1], pch=16, col='transparent', border='grey', main=NULL)
# plot(eu_bnd[eu_bnd$CNTR_ID=="FR",], pch=16, col='transparent', border='grey', main=NULL)
# plot(grd_test2, add=T, col='blue')

DM_test <- setup_test[[3]]
max_w <- lapply(1:ncol(DM_expanse), function(grd_i) gw.weight(DM_expanse[,grd_i], nngb, kernel[1], T) %>% max)
hist(unlist(max_w))
