# Finished: SLR, GWR, RF (NO2, PM2.5)
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# Whether to tune RF
target_poll = 'PM10'
tuneRF_b = T
local_crs <- CRS("+init=EPSG:3035")
seed <- 123
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
# Multiple single years
csv_names <- paste0('all_', target_poll, "_", c(2000:2019))   #2008:2012
# o_ and o2_ differences are whether to include data after 2013
years <- as.list(c(2000:2019))
source("../expanse_multiyear/src/00_fun_read_data_gee.R")
run_i <- 1
for(yr_i in seq_along(csv_names)){
   df_all <- read_data(target_poll,  years[[yr_i]])
   source('../EXPANSE_algorithm/scr/fun_select_predictor.R')
   pred_c <- select_predictor(df_all)
   
   
   if(nrow(df_all)>200){
      
      csv_name <- csv_names[yr_i]
      print("********************************************")
      print(csv_name)
      print(target_poll)
      source("../EXPANSE_algorithm/scr/fun_slr_for.R")
      # check the predictor variables
      print("SLR predictors:")
      # source("../EXPANSE_algorithm/scr/fun_slr_proc_in_data.R")
      # df_all2 <- proc_in_data(df_all, neg_pred, "xcoord", "ycoord")
      df_all %>% dplyr::select(pred_c) %>% names()
      
      slr_result <- slr(df_all$obs, as.data.frame(df_all[, c( c(pred_c))]),
                        cv_n = csv_name,
                        R2thres = ifelse(target_poll=='PM2.5', 0.0, 0.01))
      slr_model <- slr_result[[3]]


      slr_model %>% summary

      #f# SLR: test SLR
      source("../EXPANSE_algorithm/scr/fun_gen_pred_df.R")
      slr_df <- gen_pred_df(slr_model, df_all, 'obs')[, c("sta_code", "slr", "obs", "res",
                                                              "year")]
      # 
      # #-----------#f# GWR: train GWR----------
      print("GWR")
      source("../EXPANSE_algorithm/scr/fun_setupt_gwr.R")
      setup <- setup_gwr(df_all, eu_bnd,
                         cellsize = 200000, local_crs = local_crs, xcoord="xcoord", ycoord="ycoord")
      sp_train <- setup[[1]]
      grd <- setup[[2]]
      DM <- setup[[3]]
      source("../EXPANSE_algorithm/scr/fun_calibr_gwr.R")
      nngb <- calibr_gwr(sp_train, csv_name)
      print(paste0("nngb: ", nngb))
      source("../EXPANSE_algorithm/scr/fun_gwr.R")
      gwr_model <- gwr(sp_train, grd, DM, nngb, csv_name)
      #f# GWR: perform cross-validation

      coef_stack <- stack(gwr_model$SDF)
      source("../EXPANSE_algorithm/scr/fun_gen_df_gwr.R")
      gwr_train_df <- gen_df_gwr(coef_stack, sp_train, df_all)
      gwr_train_df <- (gwr_train_df %>% mutate(res=obs-gwr))[, c("sta_code", "gwr", "obs", "res",
                                                                 "year")]
      names(coef_stack) <- paste0('X', years[[yr_i]], '_', seq_along(names(coef_stack)))
      # 
      #--------- RF: split data into train, validation, and test data--------
      print("--------------- RF ---------------")
      set.seed(seed)
      # index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
      # train_df <- data_all[index$train, ]
      # valid_df <- data_all[index$valid, ]
      # test_df <- data_all[index$test, ]
      train_df <- df_all
      pred_c_rf <- c(pred_c) #"x_trun", "y_trun"  ,  "cntr_code"
      x_varname = names(train_df %>% dplyr::select(pred_c_rf))
      print("RF predictors:")
      print(x_varname)
      ## LLO CV (small test for multiple years)

      if(tuneRF_b){
         #f# RF: tune hyperparameter
         hyper_grid <- expand.grid(
            mtry = seq(floor(sqrt(length(x_varname))), length(x_varname), by=20),
            ntrees = seq(200,800, by=200),
            OOB_RMSE = 0,
            OOB_R2 = 0,
            valid_RMSE = 0,
            valid_R2 = 0
         )
         source("../EXPANSE_algorithm/scr/fun_tune_rf.R")
         hyper_grid <- tune_rf(train_df, train_df, #valid_df,
                               y_varname='obs',
                               x_varname,
                               csv_name, hyper_grid)

         #f# RF: train the model
         hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
      }
      source("../EXPANSE_algorithm/scr/fun_opt_rf.R")
      # If tuneRF is False, a 500 number of trees and sqrt(x_varname no) of mtry woul be used
      rf_result <- opt_rf(train_df, train_df,
                          y_varname='obs',
                          x_varname = x_varname,
                          csv_name, hyper_grid, tuneRF_b,
                          outputselect = c("sta_code", "rf", "obs", "res",
                                           "df_type", "year"))[[1]] %>%
         filter(df_type=='train') %>%
         dplyr::select(-df_type)

      source("../EXPANSE_algorithm/scr/fun_plot_rf_vi.R")
      plot_rf_vi(csv_name, var_no = 15)
      hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
      hyper_grid2 <- hyper_grid[hyper_grid$ntree==min(hyper_grid$ntree),]%>% 
         mutate(year=years[[yr_i]], csv_name=csv_name, poll=target_poll, yr_str=years[[yr_i]])
      slr_summary <- read.csv(paste0("data/workingData/SLR_summary_model_", csv_name, ".csv") )[, c('variables', 'beta')]
      slr_summary <- slr_summary %>% mutate(year=years[[yr_i]], csv_name = csv_name, 
                                            poll=target_poll, yr_str=years[[yr_i]])
      
      if(run_i==1){
         gwrCoefAll <- coef_stack
         hyperAll <- hyper_grid2[which.min(hyper_grid2$OOB_RMSE),] 
         slrCoefAll <- slr_summary
      }else{
         gwrCoefAll <- stack(gwrCoefAll, coef_stack)
         hyperAll <- rbind(hyperAll, hyper_grid2[which.min(hyper_grid2$OOB_RMSE),] %>% mutate(year=years[[yr_i]], csv_name=csv_name))
         slrCoefAll <- rbind(slrCoefAll, slr_summary)
      }
      run_i <- run_i+1
   }
}

write.csv(hyperAll, paste0('data/processed/RFhyper_', 'all_', target_poll, "_", 
                           min(unique(hyperAll$year)), "_", max(unique(hyperAll$year)), '.csv'), 
          row.names = F)
slrCoefAll$index <- 1:nrow(slrCoefAll)
write.csv(slrCoefAll, paste0('data/processed/SLRcoef_', 'all_', target_poll, "_", 
                              min(unique(slrCoefAll$year)), "_", max(unique(slrCoefAll$year)), '.csv'), 
          row.names = F)

writeRaster(gwrCoefAll, paste0('data/processed/GWRcoef_all_', target_poll, "_", 
                               min(unique(slrCoefAll$year)), "_", max(unique(slrCoefAll$year)), '.tiff'),
            overwrite=T)

## combine air pollutants and create new variables in the table
rfFiles <- list.files('data/processed/', 'RFhyper_all_')
gwrFiles <- list.files('data/processed/', 'GWRcoef_all_')[!grepl('.aux', list.files('data/processed/', 'GWRcoef_all_'))]
slrFiles <- list.files('data/processed/', 'SLRcoef_all_')

hyper <- lapply(paste0('data/processed/', rfFiles), function(fileName){
   hyper <- read.csv(fileName)
   # hyper$poll <- strsplit(hyper$csv_name, '_') %>% lapply(., `[[`, 2) %>% unlist
   # hyper$yr_str <- strsplit(hyper$csv_name, '_') %>% lapply(., `[[`, 3) %>% unlist
   hyper
}) %>% do.call(rbind,. )

gwr_coefs <- lapply(seq_along(paste0('data/processed/', gwrFiles)), function(i){
   gwr <- stack(paste0('data/processed/', gwrFiles[i]))
   # gwr[1]
   # paste0('data/processed/', slrFiles[i])
   gwr
}) %>% do.call(stack, .)

slrCoefs <- lapply(paste0('data/processed/', slrFiles), function(fileName){
   slrCoefs <- read.csv(fileName)
   # slrCoefs$poll <- strsplit(slrCoefs$csv_name, '_') %>% lapply(., `[[`, 2) %>% unlist
   # slrCoefs$yr_str <- strsplit(slrCoefs$csv_name, '_') %>% lapply(., `[[`, 3) %>% unlist
   slrCoefs
}) %>% do.call(rbind,. )
slrCoefs$index <- 1:nrow(slrCoefs)
if(!dir.exists('data/processed/combined/')) dir.create('data/processed/combined/')
write.csv(slrCoefs, 'data/processed/combined/SLRcoef_all_singleYears.csv', row.names=F)
write.csv(hyper, 'data/processed/combined/RFhyper_all_singleYears.csv', row.names=F)
writeRaster(gwr_coefs, 'data/processed/combined/GWRcoef_all_singleYears.tiff', 
            row.names=F, overwrite=T)
