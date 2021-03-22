# This script is used for generating the coefficient surfaces 
source("scr/fun_call_lib.R")
source("scr/fun_read_data.R")
seed <- 123
local_crs <- CRS("+init=EPSG:3035")

eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
## Read in data (elapse NO2 2010 with climate zones included)
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                       encoding = "utf-8")
## Read in data (airbase observations 1990s-2012)
no2 <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_no2.csv")
# rename data
elapse_no2 <- rename(elapse_no2, station_european_code=ï..Station)
# reduce airbase data
no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value)
## subset stations that are included in the elapse (cause at this stage, we don't have the predictor maps...)
no2_e <- no2 %>% filter(no2$station_european_code%in%unique(elapse_no2$station_european_code))
no2_e_all <- left_join(no2_e, elapse_no2, by="station_european_code")

## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   no2_e_sub
}
#---------Test the bandwidth----------
# Test the kernel function:
regression_grd_cellsize <-  50  #km
kernel_type <- 'exponential'
year_target <- 2010
out_dir <- "data/workingData/gwr_coef_surface/"
reg_grdsize <- regression_grd_cellsize*1000
# csv_names <- comb$csv_name
csv_name <- paste0('testGWR_', regression_grd_cellsize, '_', kernel_type, '_', year_target)
print(csv_name)
no2_e_09_11 <- subset_df_yrs(no2_e_all, year_target)
data_all <- no2_e_09_11

#f# stratified by station types, climate zones and/or years
set.seed(seed)
data_all$index <- 1:nrow(data_all)
train_sub <- stratified(data_all, c('type_of_st', 'zoneID'), 0.8)
test_sub <- data_all[-train_sub$index, ]

all_sub <- rbind(train_sub %>% mutate(df_type='train'), 
                 test_sub %>% mutate(df_type='test'))

all_sp <- SpatialPointsDataFrame(coords = cbind(all_sub$Xcoord, all_sub$Ycoord),
                                 all_sub, proj4string = local_crs)

# Check whether every station serve as the same type of data over years
any(unique(train_sub$station_european_code)%in%unique(test_sub$station_european_code))
# --> yes indeed the stations are not the same in training and test data
#TODO we need to look at the groups separately or in combined?
#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)
test_sub <- proc_in_data(test_sub, neg_pred)
#------------------Above code is needed for all algorithms----------------------
#---------#f# SLR: train SLR -----------
slr <- read.csv(paste0("data/workingData/SLR_summary_model_run2_", year_target,".csv"))
slr_poll <- read.csv(paste0('data/workingData/SLR_result_all_run2_', year_target,".csv"))
eq <- as.formula(paste0('obs~',  paste(slr$variables[-1], collapse = "+")))

#f# SLR: perform cross-validation

#-----------#f# GWR: train GWR----------
# the fixed/adaptive calibrated bandwidth does NOT change with regression grid cellsize.
print("GWR")
source("scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd, 
                   cellsize = reg_grdsize, local_crs = local_crs)
sp_train <- setup[[1]]
grd <- setup[[2]]
DM <- setup[[3]]
plot(coordinates(grd), col='red', pch=18)
plot(grd, add=T, col='dark green')
plot(eu_bnd[1], pch=16, col='transparent',add=TRUE)
plot(sp_train, add=T)
# tiff(paste0("graph/gwr_coef/coef_grid_", regression_grd_cellsize[i], '.tif'), 
#      width = 5, height=4, units='in', res=100)
# plot(grd)
# Calibrate bandwidth using CV
# The calibration is not influenced by the regression grid cell size
# if(!file.exists(paste0("data/workingData/GWR_dist_", 
#                        kernel_type, "_", year_target, ".txt"))){
#    DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
#                    rp.locat=coordinates(sp_train))
#    # 
#    bandwidth_calibr <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
#                               adaptive = F, dMat = DM_1)
#    write.table(bandwidth_calibr, paste0("data/workingData/GWR_dist_", 
#                                         kernel_type, "_", year_target, ".txt"))
# }

if((!file.exists(paste0("data/workingData/GWR_nngb_", 
                       kernel_type, "_", year_target, ".txt")))&
   (!file.exists(paste0("data/workingData/GWR_nngb_run2_", 
                         year_target, ".txt")))){
   DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                   rp.locat=coordinates(sp_train))
   # 
   nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
                  adaptive = T, dMat = DM_1)
   write.table(nngb, paste0("data/workingData/GWR_nngb_", 
                            kernel_type, "_", year_target, ".txt"))
}else{
   if(file.exists(paste0("data/workingData/GWR_nngb_", 
                           kernel_type, "_", year_target, ".txt"))){
      nngb <- read.table(paste0("data/workingData/GWR_nngb_", 
                                kernel_type, "_", year_target, ".txt"))[,1]
   }else{
      nngb <- read.table(paste0("data/workingData/GWR_nngb_run2_", 
                                year_target, ".txt"))[,1]
   }
   
}

# nngb %>% print()
# source("scr/fun_gwr.R")
# bandwidth_calibr <- read.table(paste0("data/workingData/GWR_dist_", 
#                                       kernel_type, "_", year_target, ".txt"))[,1]
# gwr_model <- tryCatch(gwr.basic(eq,
#                                 data=sp_train,
#                                 regression.points=grd,
#                                 adaptive = F,
#                                 bw=bandwidth_calibr,
#                                 dMat=DM,
#                                 kernel=kernel_type), 
#                       error=function(e) T)
gwr_model_ad <- tryCatch(gwr.basic(eq,
                                   data=sp_train,
                                   regression.points=grd,
                                   adaptive = T,
                                   bw=nngb,
                                   dMat=DM,
                                   kernel=kernel_type),
                         error=function(e) T)
coef_stack <- stack(gwr_model_ad$SDF)
#------output coef surface--------
if(!dir.exists(out_dir)) dir.create(paste0(out_dir))
writeRaster(coef_stack, paste0(out_dir, csv_name, ".tif"))
write.table(names(coef_stack), paste0(out_dir, csv_name, ".txt"), row.names = F)
plot(raster(paste0(out_dir, csv_name, ".tif")))

# # mixed GWR
# # global_vars contain the variables that we want it to have gloabl coef values.
# # intercept can also have a global value, instead of being locally varying
# # (the bandwidth is used as the same when these global_vars are locally varying)
# if(any(slr$variables[-1]%in%global_vars)){
#    gwr_model2 <- tryCatch(gwr.mixed(eq,
#                                     data=sp_train,
#                                     regression.points=grd,
#                                     fixed.vars = global_vars,
#                                     adaptive = F,
#                                     bw=bandwidth_calibr,
#                                     dMat=DM,
#                                     kernel=kernel_type), 
#                           error=function(e) T)
#    gwr_model_ad2 <- tryCatch(gwr.mixed(eq,
#                                        data=sp_train,
#                                        regression.points=grd,
#                                        fixed.vars = global_vars,
#                                        adaptive = T,
#                                        bw=nngb,
#                                        dMat=DM,
#                                        kernel=kernel_type),
#                              error=function(e) T)
# }
# 

# error: inv(): matrix seems singular
if(!((typeof(gwr_model)=='logical')|(typeof(gwr_model_ad)=='logical'))){
   #f# GWR: perform cross-validation
   source("scr/fun_output_gwr_result.R")
   gwr_df <- output_gwr_result(gwr_model, train_sub, test_sub, local_crs,
                               output_filename = csv_name)
   gwr_df_ad <- output_gwr_result(gwr_model_ad, train_sub, test_sub, local_crs,
                                  output_filename = paste0(csv_name, "_ad"))
   # gwr_df2 <- output_gwr_result(gwr_model2, train_sub, test_sub, local_crs,
   #                              output_filename = paste0(csv_name, '_mixed'), mixedGWR = T)
   # gwr_df_ad2 <- output_gwr_result(gwr_model_ad2, train_sub, test_sub, local_crs,
   #                                 output_filename = paste0(csv_name, "_ad_mixed"), mixedGWR = T)
   # output all models' performance matrix
   output_em <- function(pred_df, csv_name, model, year){
      error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', 'gwr'])
      
      em <- rbind(error_matrix(pred_df[pred_df$df_type=='test', 'obs'], pred_df[pred_df$df_type=='test', model]) , 
                  error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', model])) %>% 
         as.data.frame()
      
      perf_matrix <- em[, c(1, 5, 7)] %>% mutate(df_type=c('test','train'), model=model, year=year, csv_name=csv_name)
      perf_matrix
   }
   # output_em(slr_poll, paste0('slr_', year_target), 'slr', year_target)
   # output_em(gwr_df, csv_name, 'gwr', year_target)
   # output_em(gwr_df_ad, paste0(csv_name, '_ad'), 'gwr', year_target)
   em <- rbind(output_em(slr_poll, paste0('slr_', year_target), 'slr', year_target),   #slr model performance will be duplicated
               output_em(gwr_df, csv_name, 'gwr', year_target),
               output_em(gwr_df_ad, paste0(csv_name, '_ad'), 'gwr', year_target),
               output_em(gwr_df2, paste0(csv_name, '_mixed'), 'gwr', year_target),
               output_em(gwr_df_ad2, paste0(csv_name, '_ad_mixed'), 'gwr', year_target))
   em <- em  %>% arrange(df_type)
   em
   write.csv(em, paste0('data/workingData/model_perf_', csv_name, '.csv'), row.names = F)
   
   # plot gwr surface
   ncol(gwr_model$SDF) %>% print()  # the number of predictors selected
   source('scr/fun_plot_gwr_coef.R')
   plot_gwr_coef(i, gwr_model, csv_name = csv_name, 
                 n_row = 2, n_col = 3, eu_bnd=eu_bnd)
   plot_gwr_coef(i, gwr_model_ad, csv_name = paste0(csv_name, "_ad"), 
                 n_row = 2, n_col = 3, eu_bnd=eu_bnd)
   plot_gwr_coef(i, gwr_model2, csv_name = paste0(csv_name, "_mixed"), 
                 n_row = 2, n_col = 3, eu_bnd=eu_bnd)
   plot_gwr_coef(i, gwr_model_ad2, csv_name = paste0(csv_name, "_ad_mixed"), 
                 n_row = 2, n_col = 3, eu_bnd=eu_bnd)
}else{
   if(!file.exists('data/workingData/gwr_failed.txt')){
      write.table(csv_name, "data/workingData/gwr_failed.txt",
                  sep = ",", row.names = F)
   }else{
      write.table(csv_name, "data/workingData/gwr_failed.txt",
                  sep = ",", row.names = F, col.names = F, append = T)
   }
}
