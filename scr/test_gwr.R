# This script is intended to test GWR models with different parameter settings
# Bandwidth (adaptive or not)
# kernel type (just Gaussian and exponential)
# regression grid cellsize
library(dplyr)
library(raster)
library(sf)
library(car)  # for running slr
library(GWmodel)  #gwr
library(viridis)  #palette for raster
library(ranger) # Random forests
library(caret)  #data partition
library(tmap)
library(splitstackshape)   #stratified function in this library is better than createDataPartition in library caret
library(splitTools)
library(APMtools)
library(lme4) # linear mixed effect models
library(CAST) # For dividing training and test data (CreateSpacetimeFolds)
library(performance) #extract model performance matrix for lme

seed <- 123
local_crs <- CRS("+init=EPSG:3035")

eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
## Read in data (elapse NO2 2010 with climate zones included)
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                       encoding = "utf-8")
## Read in data (airbase observations 1990s-2012)
no2 <- read.csv("../airbase/EXPANSE_APM/data/processed/ab_v8_yr_no2.csv")
# rename data
elapse_no2 <- rename(elapse_no2, station_european_code=ï..Station)
# reduce airbase data
no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value)
## subset stations that are included in the elapse (cause at this stage, we don't have the predictor maps...)
no2_e <- no2 %>% filter(no2$station_european_code%in%unique(elapse_no2$station_european_code))
no2_e_all <- left_join(no2_e, elapse_no2, by="station_european_code")
source("../airbase/EXPANSE_APM/src/fun_eda_spatial_distribution_annualobs_laea.R")
annual_spatial_dist(poll_conc = no2_e_all, eu_bnd = eu_bnd, 
                    folder_subnote = 'elapse_ab')
## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   no2_e_sub
}
#o# multiple years

#---------Test the bandwidth----------
global_vars <- "no2_10MACC"

regression_grd_cellsize <- c(80, 100, 200, 600, 1000, 1500, 2000)   #km
kernels <- c('gaussian', 'exponential', 'bisquare', 'tricube')
year_target <- 2009

comb <- expand.grid(regression_grd_cellsize=regression_grd_cellsize, kernel_type=kernels) %>% 
   mutate(csv_name = paste0('testGWR_', regression_grd_cellsize, '_', kernel_type, '_', year_target))
# perf_matrix <- data.frame(RMSE=0, RRMSE=0, IQR=0, rIQR=0, MAE=0, rMAE=0, rsq=0, explained_var=0,
#                           csv_name="0", datatype="0")
library(doParallel)
library(foreach)
cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)
foreach(i=seq_len(nrow(comb))) %dopar% {
   library(dplyr)
   library(raster)
   library(sf)
   library(car)  # for running slr
   library(GWmodel)  #gwr
   library(viridis)  #palette for raster
   library(ranger) # Random forests
   library(caret)  #data partition
   library(splitstackshape)   #stratified function in this library is better than createDataPartition in library caret
   library(splitTools)
   library(APMtools)
   library(lme4) # linear mixed effect models
   library(CAST) # For dividing training and test data (CreateSpacetimeFolds)
   library(performance) #extract model performance matrix for lme
   library(tmap)
   seed <- 123
   local_crs <- CRS("+init=EPSG:3035")
   
   eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
   ## Read in data (elapse NO2 2010 with climate zones included)
   elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                          encoding = "utf-8")
   ## Read in data (airbase observations 1990s-2012)
   no2 <- read.csv("../airbase/EXPANSE_APM/data/processed/ab_v8_yr_no2.csv")
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
   #o# multiple years
   # Mixed GWR
   global_vars <- "no2_10MACC"
   
   #---------Test the bandwidth----------
   # Test the kernel function:
   regression_grd_cellsize <- c(80, 100, 200, 600, 1000, 1500, 2000)   #km
   kernels <- c('gaussian', 'exponential', 'bisquare', 'tricube')
   year_target <- 2009
   comb <- expand.grid(regression_grd_cellsize=regression_grd_cellsize, kernel_type=kernels) %>% 
      mutate(csv_name = paste0('testGWR_', regression_grd_cellsize, '_', kernel_type, '_', year_target))
   kernel_type <- comb$kernel_type %>% as.character()
   reg_grdsize <- comb$regression_grd_cellsize*1000
   csv_names <- comb$csv_name
   # csv_names <- paste0('testGWR_', regression_grd_cellsize, "_", year_target)
   years <- as.list(rep(year_target, length(csv_names)))
   
   
   csv_name <- csv_names[i]
   print(csv_name)
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
   data_all <- no2_e_09_11
   
   #f# stratified by station types, climate zones and/or years
   set.seed(seed)
   data_all$index <- 1:nrow(data_all)
   train_sub <- stratified(data_all, c('type_of_st', 'climate_zone'), 0.8)
   test_sub <- data_all[-train_sub$index, ]
   
   all(unique(train_sub$station_european_code)%in%unique(test_sub$station_european_code))
   all_sub <- rbind(train_sub %>% mutate(df_type='train'), 
                    test_sub %>% mutate(df_type='test'))
   
   all_sp <- SpatialPointsDataFrame(coords = cbind(all_sub$Xcoord, all_sub$Ycoord),
                                    all_sub, proj4string = local_crs)
   
   
   sum(train_sub$type_of_st=="Background"&train_sub$climate_zone==1)/nrow(train_sub)
   sum(test_sub$type_of_st=="Background"&test_sub$climate_zone==1)/nrow(test_sub)
   sum(train_sub$type_of_st=="Background")/nrow(train_sub)
   sum(test_sub$type_of_st=="Background")/nrow(test_sub)
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
   
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_run1_train_break_noxy", years[[i]],".csv"))
   slr_poll <- read.csv(paste0('data/workingData/SLR_result_all_run1_train_break_noxy', years[[i]],".csv"))
   eq <- as.formula(paste0('obs~',  paste(slr$variables[-1], collapse = "+")))
   
   #f# SLR: perform cross-validation
   
   #-----------#f# GWR: train GWR----------
   # the fixed/adaptive calibrated bandwidth does NOT change with regression grid cellsize.
   print("GWR")
   source("scr/fun_setupt_gwr.R")
   setup <- setup_gwr(train_sub, eu_bnd, 
                      cellsize = reg_grdsize[i], local_crs = local_crs)
   sp_train <- setup[[1]]
   grd <- setup[[2]]
   DM <- setup[[3]]
   # plot(grd)
   # Calibrate bandwidth using CV
   # The calibration is not influenced by the regression grid cell size
   if(!file.exists(paste0("data/workingData/GWR_dist_", 
                          kernel_type[i], "_", years[[i]], ".txt"))){
      DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                      rp.locat=coordinates(sp_train))
      # 
      bandwidth_calibr <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type[i],
                                 adaptive = F, dMat = DM_1)
      write.table(bandwidth_calibr, paste0("data/workingData/GWR_dist_", 
                                           kernel_type[i], "_", years[[i]], ".txt"))
   }
   
   if(!file.exists(paste0("data/workingData/GWR_nngb_", 
                          kernel_type[i], "_", years[[i]], ".txt"))){
      DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                      rp.locat=coordinates(sp_train))
      # 
      nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type[i],
                                 adaptive = T, dMat = DM_1)
      write.table(nngb, paste0("data/workingData/GWR_nngb_", 
                                           kernel_type[i], "_", years[[i]], ".txt"))
   }
   
   # nngb %>% print()
   # source("scr/fun_gwr.R")
   bandwidth_calibr <- read.table(paste0("data/workingData/GWR_dist_", 
                                         kernel_type[i], "_", years[[i]], ".txt"))[,1]
   # nngb <- read.table(paste0("data/workingData/GWR_nngb_run1_train_break_noxy", 
   #                           years[[i]],".txt"))[,1]
   
   nngb <- read.table(paste0("data/workingData/GWR_nngb_", 
                             kernel_type[i], "_", years[[i]], ".txt"))[,1]
   gwr_model <- tryCatch(gwr.basic(eq,
                                   data=sp_train,
                                   regression.points=grd,
                                   adaptive = F,
                                   bw=bandwidth_calibr,
                                   dMat=DM,
                                   kernel=kernel_type[i]), 
                         error=function(e) T)
   gwr_model_ad <- tryCatch(gwr.basic(eq,
                                      data=sp_train,
                                      regression.points=grd,
                                      adaptive = T,
                                      bw=nngb,
                                      dMat=DM,
                                      kernel=kernel_type[i]),
                            error=function(e) T)
   # mixed GWR
   # global_vars contain the variables that we want it to have gloabl coef values.
   # intercept can also have a global value, instead of being locally varying
   # (the bandwidth is used as the same when these global_vars are locally varying)
   if(any(slr$variables[-1]%in%global_vars)){
      gwr_model2 <- tryCatch(gwr.mixed(eq,
                                      data=sp_train,
                                      regression.points=grd,
                                      fixed.vars = global_vars,
                                      adaptive = F,
                                      bw=bandwidth_calibr,
                                      dMat=DM,
                                      kernel=kernel_type[i]), 
                            error=function(e) T)
      gwr_model_ad2 <- tryCatch(gwr.mixed(eq,
                                         data=sp_train,
                                         regression.points=grd,
                                         fixed.vars = global_vars,
                                         adaptive = T,
                                         bw=nngb,
                                         dMat=DM,
                                         kernel=kernel_type[i]),
                               error=function(e) T)
   }
   
   
   # error: inv(): matrix seems singular
   if(!((typeof(gwr_model)=='logical')|(typeof(gwr_model_ad)=='logical'))){
      #f# GWR: perform cross-validation
      source("scr/fun_output_gwr_result.R")
      gwr_df <- output_gwr_result(gwr_model, train_sub, test_sub, local_crs,
                                  output_filename = csv_name)
      gwr_df_ad <- output_gwr_result(gwr_model_ad, train_sub, test_sub, local_crs,
                                     output_filename = paste0(csv_name, "_ad"))
      gwr_df2 <- output_gwr_result(gwr_model2, train_sub, test_sub, local_crs,
                                  output_filename = paste0(csv_name, '_mixed'), mixedGWR = T)
      gwr_df_ad2 <- output_gwr_result(gwr_model_ad2, train_sub, test_sub, local_crs,
                                     output_filename = paste0(csv_name, "_ad_mixed"), mixedGWR = T)
      # output all models' performance matrix
      output_em <- function(pred_df, csv_name, model, year){
         error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[gwr_df$df_type=='train', 'gwr'])
         
         em <- rbind(error_matrix(pred_df[pred_df$df_type=='test', 'obs'], pred_df[pred_df$df_type=='test', model]) , 
                     error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', model])) %>% 
            as.data.frame()
         
         perf_matrix <- em[, c(1, 5, 7)] %>% mutate(df_type=c('test','train'), model=model, year=year, csv_name=csv_name)
         perf_matrix
      }
      # output_em(slr_poll, paste0('slr_', years[[i]]), 'slr', years[[i]])
      # output_em(gwr_df, csv_name, 'gwr', years[[i]])
      # output_em(gwr_df_ad, paste0(csv_name, '_ad'), 'gwr', years[[i]])
      em <- rbind(output_em(slr_poll, paste0('slr_', years[[i]]), 'slr', years[[i]]),   #slr model performance will be duplicated
                  output_em(gwr_df, csv_name, 'gwr', years[[i]]),
                  output_em(gwr_df_ad, paste0(csv_name, '_ad'), 'gwr', years[[i]]),
                  output_em(gwr_df2, paste0(csv_name, '_mixed'), 'gwr', years[[i]]),
                  output_em(gwr_df_ad2, paste0(csv_name, '_ad_mixed'), 'gwr', years[[i]]))
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
   
   
}
parallel::stopCluster(cl)


perfm <- lapply(paste0('data/workingData/', list.files('data/workingData/', 'model_perf_')), 
                function(filename) read.csv(filename, header=T) )
perfm <- do.call(rbind, perfm)
perfm <- perfm[!(duplicated(perfm$csv_name)&perfm$model=='slr'), ]
perfm$reg_grdsize <- c(NA, lapply(perfm$csv_name[2:nrow(perfm)], function(f) strsplit(f, '_')[[1]][2]) %>% unlist() %>% as.numeric())
perfm$kernel <- c(NA, lapply(perfm$csv_name[2:nrow(perfm)], function(f) strsplit(f, '_')[[1]][3]) %>% unlist() %>% as.character())
perfm$adaptive <- c(NA, lapply(perfm$csv_name[2:nrow(perfm)], function(f){
   ifelse((length(strsplit(f, '_')[[1]])>4)&(strsplit(f, '_')[[1]][5]=='ad'),
          'adaptive bandwidth', 'fixed bandwidth')
}) %>% unlist())
perfm$global <- c(NA, lapply(perfm$csv_name[2:nrow(perfm)], function(f){
   ifelse(((length(strsplit(f, '_')[[1]])==5)&(strsplit(f, '_')[[1]][5]=='mixed'))|
             ((length(strsplit(f, '_')[[1]])==6)&(strsplit(f, '_')[[1]][6]=='mixed')),
          paste0('global ', global_vars), paste0('local ', global_vars))
}) %>% unlist())
perfm %>% dplyr::filter(kernel=='exponential') %>% View
perfm %>% dplyr::filter(kernel=='gaussian') %>% View

read.csv(paste0("data/workingData/SLR_summary_model_run1_train_break_noxy", 2009,".csv"))

#------------Test the kernel function:------------
# names <- paste0('run1_train_', c('2010', '09-11', '08-12'))
# years <- list(2010, 2009:2011, 2008:2012)
kernels <- c("gaussian", "exponential", "bisquare","tricube","boxcar")
names <- paste0('testGWR_', kernels, "_2010")
years <- as.list(rep(2010, length(names)))
i=1
for(i in seq_along(kernels)){
   csv_name <- names[i]
   print(csv_name)
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
   data_all <- no2_e_09_11
   #f# subset cross-validation data (5-fold cross-validation)
   #f# stratified by station types, climate zones and/or years
   set.seed(seed)
   data_all$index <- 1:nrow(data_all)
   train_sub <- stratified(data_all, c('type_of_st', 'climate_zone'), 0.8)
   test_sub <- data_all[-train_sub$index, ]
   
   # Test only leave location out first 
   # Method 1: (easier to use)
   # folds=CreateSpacetimeFolds(
   #    data_all,
   #    spacevar = "station_european_code",     # leave location out
   #    timevar = NA,
   #    k = 5,
   #    class = NA,
   #    seed = seed
   # )
   # Method 2:
   # all_stations <- unique(data_all$station_european_code)
   # #we create cross validation folds using caret's createFolds
   # num_folds <- 5
   # set.seed(seed)  # for reproducibility
   # folds <- createFolds(all_stations,k=num_folds,list=TRUE,returnTrain=TRUE)
   # train_sub <- data_all[folds$index[[1]], ]
   # test_sub <- data_all[folds$indexOut[[1]], ]
   # Check whether the stratification works
   all(unique(train_sub$station_european_code)%in%unique(test_sub$station_european_code))
   all_sub <- rbind(train_sub %>% mutate(df_type='train'), 
                    test_sub %>% mutate(df_type='test'))
   
   all_sp <- SpatialPointsDataFrame(coords = cbind(all_sub$Xcoord, all_sub$Ycoord),
                                    all_sub, proj4string = local_crs)
   tmap_mode('plot')
   # year_s <- all_sp$year %>% unique
   # maps_l <- lapply(year_s, function(year_i){
   #    map_1 <- tm_shape(all_sp[all_sp$year==year_i, ]) +
   #       tm_dots(size = 0.05, col="df_type",   # col = "NO2",          # popup.vars for showing values
   #               title = paste0('region'),
   #               palette=c('red','blue'))+
   #       tm_shape(eu_bnd)+
   #       tm_borders()+
   #       tm_layout(title=year_i)
   #    map_1
   # })
   # do.call(tmap_arrange, maps_l)
   
   sum(train_sub$type_of_st=="Background"&train_sub$climate_zone==1)/nrow(train_sub)
   sum(test_sub$type_of_st=="Background"&test_sub$climate_zone==1)/nrow(test_sub)
   sum(train_sub$type_of_st=="Background")/nrow(train_sub)
   sum(test_sub$type_of_st=="Background")/nrow(test_sub)
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
   
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_run1_train_2010.csv"))
   slr_poll <- read.csv(paste0('data/workingData/SLR_result_all_run1_train_2010.csv'))
   eq <- as.formula(paste0('obs~',  paste(slr$variables[-1], collapse = "+")))
   
   slr_poll$eval_train %>% print()
   slr_poll$eval_test %>% print()
   slr_df <- slr_poll[[1]]
   
   #f# SLR: perform cross-validation
   
   #-----------#f# GWR: train GWR----------
   print("GWR")
   source("scr/fun_setupt_gwr.R")
   setup <- setup_gwr(train_sub, eu_bnd, 
                      cellsize = 200000, local_crs = local_crs)
   sp_train <- setup[[1]]
   grd <- setup[[2]]
   DM <- setup[[3]]
   source("scr/fun_calibr_gwr.R")
   # Calibrate bandwidth using CV
   DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                   rp.locat=coordinates(sp_train))
   
   nngb <- bw.gwr(eq, data=sp_train, approach = "CV",
                  kernel = kernels[i], adaptive = T, dMat = DM_1)
   write.table(nngb, paste0("data/workingData/GWR_nngb_", csv_name, ".txt"))
   nngb %>% print()
   # source("scr/fun_gwr.R")
   (lapply(paste0("data/workingData/GWR_nngb_", csv_name, ".txt"), read.table) %>% Reduce(rbind,.))[,1]
   gwr_model <- gwr.basic(eq,
                          data=sp_train, 
                          regression.points=grd, 
                          adaptive = T,
                          bw=nngb, 
                          dMat=DM,
                          kernel=kernels[i])
   #f# GWR: perform cross-validation
   source("scr/fun_output_gwr_result.R")
   gwr_df <- output_gwr_result(gwr_model, train_sub, test_sub, local_crs,
                               output_filename = csv_name)
   error_matrix(gwr_df[gwr_df$df_type=='train', 'obs'], gwr_df[gwr_df$df_type=='train', 'gwr']) %>% 
      print()
   error_matrix(gwr_df[gwr_df$df_type=='test', 'obs'], gwr_df[gwr_df$df_type=='test', 'gwr']) %>% 
      print()
   # plot gwr surface
   ncol(gwr_model$SDF) %>% print()  # the number of predictors selected
   source('scr/fun_plot_gwr_coef.R')
   plot_gwr_coef(i, n_row = 3, n_col = 4)
}
