# This script run the three models for multiple single years or multiple years
source("scr/fun_call_lib.R")
source("scr/fun_read_data.R")

# Multiple single years
csv_names <- paste0('run2_all_', c(2005:2012))   #2008:2012
years <- as.list(c(2005:2012))


for(yr_i in seq_along(csv_names)){
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[yr_i]])
   # data_all <- no2_e_09_11
   print(paste0("year: ", unique(no2_e_09_11$year)))
   data_all <- no2_e_09_11
   
   #f# SLR: select predictors
   source("scr/fun_call_predictor.R")
   #f# SLR: define/preprocess predictors (direction of effect)
   source("scr/fun_slr_proc_in_data.R")
   data_all <- proc_in_data(data_all, neg_pred)
   train_sub <- data_all
   #------------------Above code is needed for all algorithms----------------------
   #---------#f# SLR: train SLR -----------
   source("scr/fun_slr_for.R")
   # check the predictor variables
   print("SLR predictors:")
   train_sub %>% dplyr::select(matches(pred_c)) %>% names()
   slr_result <- slr(train_sub$obs, train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame(), 
                     cv_n = csv_name)
   slr_model <- slr_result[[3]]
   
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
   
   # plot gwr surface
   ncol(gwr_model$SDF) %>% print()  # the number of predictors selected
   source('scr/fun_plot_gwr_coef.R')
   plot_gwr_coef(yr_i, gwr_model, csv_name, n_row = 3, n_col = 3, eu_bnd = eu_bnd)

} 

