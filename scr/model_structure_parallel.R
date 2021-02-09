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

# names <- paste0('run1_train_', c('2010', '09-11', '08-12'))
# years <- list(2010, 2009:2011, 2008:2012)
names <- paste0('run1_train_', 2008:2012)
years <- as.list(seq(2008, 2012))
for(i in seq_along(names)){
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
   source("scr/fun_slr.R")
   # POLL=train_sub$obs
   # pred <- train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame()
   # cv_n = csv_name
   slr_result <- slr(train_sub$obs, train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame(), 
                     cv_n = csv_name)
   slr_model <- slr_result[[3]]
   #f# SLR: test SLR
   source("scr/fun_output_slr_result.R")
   slr_poll <- output_slr_result(slr_model, test_df = test_sub, train_df = train_sub,
                                 output_filename = csv_name, obs_varname = 'obs')
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
   nngb <- calibr_gwr(sp_train, csv_name)
   nngb %>% print()
   source("scr/fun_gwr.R")
   gwr_model <- gwr(sp_train, grd, DM, nngb, csv_name)
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
   ##--------- RF: split data into train, validation, and test data--------
   print("RF")
   set.seed(seed)
   # index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
   # train_df <- data_all[index$train, ]
   # valid_df <- data_all[index$valid, ]
   # test_df <- data_all[index$test, ]
   train_df <- train_sub
   test_df <- test_sub
   x_varname = names(data_all %>% dplyr::select(matches(pred_c)))
   # LLO CV (small test for multiple years)
   indices <- CreateSpacetimeFolds(data_all,spacevar = "station_european_code",
                                   k=3, seed=seed)
   model_LLO <- train(data_all[,x_varname],data_all$obs,
                      method="rf",tuneGrid=data.frame("mtry"=80), importance=TRUE,
                      trControl=trainControl(method="cv",
                                             index = indices$index))
   model_LLO
   # --> Rsquared 0.5720142 (for 2009-2011) so indeed the previous good performance 
   #     is due to the information leakage ("mtry"=14)
   # seq(30, length(x_varname), by=10) --> best mtry = 110
   # seq(30, length(x_varname), by=50) --> best mtry:80
   #f# RF: tune hyperparameter
   hyper_grid <- expand.grid(
      mtry = seq(30, length(x_varname), by=10),
      ntrees = seq(500,1500, by=200),
      OOB_RMSE = 0,
      OOB_R2 = 0,
      valid_RMSE = 0,
      valid_R2 = 0
   )
   source("scr/fun_tune_rf.R")
   hyper_grid <- tune_rf(train_df, test_df, #valid_df,
                         y_varname='obs', 
                         x_varname,
                         csv_name, hyper_grid)
   
   #f# RF: train the model
   hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name, '.csv'))
   source("scr/fun_opt_rf.R")
   rf_result <- opt_rf(train_df, test_df, 
                       y_varname='obs', 
                       x_varname = x_varname,
                       csv_name, hyper_grid)
   rf_result$eval_train %>% print()
   rf_result$eval_test %>% print()
   source("scr/fun_plot_rf_vi.R")
   plot_rf_vi(csv_name, var_no = 10)
   #f# RF: perform cross-validation
   
}


