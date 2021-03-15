# This script run the three models for multiple single years or multiple years
# With Met available
source("scr/fun_call_lib.R")
source("scr/fun_read_data.R")
#o# multiple years
subset_df_yrs(no2_e_all, 2010) %>% dim
# [1] 2375  171
# csv_names <- paste0('run1_train_break_noxy', c(2002, 2004, 2006, 2008:2012))   #2008:2012
# Multiple single years:
# csv_names <- paste0('run2_met_', c(2002, 2004, 2006, 2008:2012))   #2008:2012
# years <- as.list(c(2002, 2004, 2006, 2008:2012))
# Multiple years:
csv_names <- paste0('run2_met_',c('08-10', '09-11', '10-12', '08-12'))   #2008:2012
years <- list(2008:2010, 2009:2011, 2010:2012, 2008:2012)

library(doParallel)
library(foreach)
cluster_no <- 4
cl <- parallel::makeCluster(cluster_no)
doParallel::registerDoParallel(cl)
foreach(i = seq_along(csv_names)) %dopar% {
   source("scr/fun_call_lib.R")
   csv_name <- csv_names[i]
   print("********************************************")
   print(csv_name)
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
   data_all <- no2_e_09_11
   if(length(years[[i]]>1)){
      years[[i]]
      met_l <- lapply(paste0("../EXPANSE_predictor/data/raw/gee/elapse_no2_met_", years[[i]], ".csv"), read.csv)
      met <- do.call(rbind, met_l)
   }else{
      met <- read.csv(paste0("../EXPANSE_predictor/data/raw/gee/elapse_no2_met_", years[[i]], ".csv")) 
   }
   met <- rename(met, station_european_code=Station) %>% mutate(wind_speed=(u_wind^2)+(v_wind^2))
   data_all <- inner_join(data_all, met %>% dplyr::select(-system.index, -.geo), by=c("station_european_code", "year"))
   print(paste0("year: ", unique(data_all$year)))
   #f# subset cross-validation data (5-fold cross-validation)
   #f# stratified by station types, climate zones and/or years
   set.seed(seed)
   data_all$index <- 1:nrow(data_all)
   if(length(years[[i]])>1){
      # Test only leave location out first 
      # Method 1: (easier to use)
      folds=CreateSpacetimeFolds(
         data_all,
         spacevar = "station_european_code",     # leave location out
         timevar = NA,
         k = 5,
         class = NA,
         seed = seed
      )
      
      train_sub <- data_all[folds$index[[1]], ]
      test_sub <- data_all[folds$indexOut[[1]], ]
   }else{
      train_sub <- stratified(data_all, c('type_of_st', 'zoneID'), 0.8)
      test_sub <- data_all[-train_sub$index, ]
   }
   
   #f# SLR: select predictors
   source("scr/fun_call_predictor.R")
   pred_c <- c(pred_c, "pressure", "temp",
               "precip", "wind_speed")  #"v_wind", "u_wind"
   neg_pred <- c(neg_pred, "precip", "wind_speed")
   #f# SLR: define/preprocess predictors (direction of effect)
   source("scr/fun_slr_proc_in_data.R")
   train_sub <- proc_in_data(train_sub, neg_pred)
   test_sub <- proc_in_data(test_sub, neg_pred)
   data_all <- rbind(train_sub, test_sub)
   #------------------Above code is needed for all algorithms----------------------
   #---------#f# SLR: train SLR -----------
   source("scr/fun_slr_for.R")
   # source("scr/fun_slr.R")
   # POLL=train_sub$obs
   # pred <- train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame()
   # cv_n = csv_name
   # check the predictor variables
   print("SLR predictors:")
   train_sub %>% dplyr::select(matches(pred_c)) %>% names()
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
   print(paste0("nngb: ", nngb))
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
   plot_gwr_coef(i, gwr_model, csv_name, n_row = 3, n_col = 3, eu_bnd = eu_bnd)
   ##--------- RF: split data into train, validation, and test data--------
   print("--------------- RF ---------------")
   set.seed(seed)
   # index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
   # train_df <- data_all[index$train, ]
   # valid_df <- data_all[index$valid, ]
   # test_df <- data_all[index$test, ]
   train_df <- train_sub
   test_df <- test_sub
   if(length(years[[i]])>1){
      pred_c_rf <- c(pred_c, "x_trun", "y_trun", "u_wind", "v_wind",
                     "year")
   }else{
      pred_c_rf <- c(pred_c, "x_trun", "y_trun", "u_wind", "v_wind")
   }
   x_varname = names(data_all %>% dplyr::select(matches(pred_c_rf)))
   print("RF predictors:")
   print(x_varname)
   ## LLO CV (small test for multiple years)
   
   #f# RF: tune hyperparameter
   hyper_grid <- expand.grid(
      mtry = seq(30, length(x_varname), by=10),
      ntrees = seq(500,1000, by=200),
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
   hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
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
   
} %>% system.time()
parallel::stopCluster(cl)

