# This script run the three models for multiple single years or multiple years
source("scr/fun_call_lib.R")
source("scr/o_00_00_read_data.R")
# Whether to tune RF
tuneRF_b = F
# Multiple single years
csv_names <- paste0('o_', c(2005:2012))   #2008:2012
years <- as.list(c(2005:2012))
# Multiple years
# csv_names <- paste0('run2_',c('08-10', '09-11', '10-12', '08-12'))   #2008:2012
# years <- list(2008:2010, 2009:2011, 2010:2012, 2008:2012)
library(doParallel)
library(foreach)

for(yr_i in seq_along(csv_names)){
   csv_name <- csv_names[yr_i]
   print("********************************************")
   print(csv_name)
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[yr_i]])
   # data_all <- no2_e_09_11
   print(paste0("year: ", unique(no2_e_09_11$year)))
   source("scr/fun_create_fold.R")
   data_all1 <- create_fold(no2_e_09_11, seed, strt_group=c("sta_type", "zoneID"), nfold = 5)
   
   cluster_no <- 5
   cl <- parallel::makeCluster(cluster_no)
   doParallel::registerDoParallel(cl)
   foreach(fold_i = seq_len(length(unique(data_all1$nfold))))  %dopar%  {
      source('scr/fun_call_lib.R')
      source("scr/o_00_00_read_data.R")
      source("scr/o_00_01_split_data.R")
      #f# SLR: select predictors
      source("scr/o_00_01_call_predictor.R")
      #f# SLR: define/preprocess predictors (direction of effect)
      source("scr/fun_slr_proc_in_data.R")
      train_sub <- proc_in_data(train_sub, neg_pred, "xcoord", "ycoord")
      test_sub <- proc_in_data(test_sub, neg_pred, "xcoord", "ycoord")
      data_all <- rbind(train_sub, test_sub)
      #------------------Above code is needed for all algorithms----------------------
      #---------#f# SLR: train SLR -----------
      source("scr/fun_slr_for.R")
      # check the predictor variables
      print("SLR predictors:")
      train_sub %>% dplyr::select(matches(pred_c)) %>% names()
      slr_result <- slr(train_sub$obs, as.data.frame(train_sub[, pred_c]),
                        cv_n = csv_name_fold)
      slr_model <- slr_result[[3]]
      # debug (why macc is not included)
      slr_model %>% summary
      lm(paste0("obs~", paste(names(slr_model$coefficients)[-1], collapse = "+"), "+macc") %>% as.formula(),
         train_sub) %>% summary
      
      # Lasso: although the perf of lassso is slightly better than slr, its vif and p-values are high.
      # library(glmnet)
      # set.seed(seed)
      # fit.cv <- glmnet::cv.glmnet(as.matrix(train_sub[,pred_c]), 
      #                             as.matrix(train_sub[,"obs"]),
      #                             type.measure = "mse",
      #                             lower.limits = 0,
      #                             alpha=1, family="gaussian", nlambda=100) 
      # 
      # plot(fit.cv, xvar="lambda", label=TRUE)
      # set.seed(seed)
      # fit <- glmnet::glmnet(as.matrix(train_sub[,pred_c]), 
      #                       as.matrix(train_sub[,"obs"]),
      #                       lower.limits = 0,      #there is no sink for NO2 in the variables.
      #                       # type.measure = "mse",
      #                       alpha=1, family="gaussian", nlambda=100)
      # plot(fit, xvar="lambda", label=TRUE)
      # lassoResult <- data.frame(df=fit$df, dev=fit$dev.ratio, lambda=fit$lambda)
      # # targetDF <- (lassoResult %>% filter(df==3))
      # targetDF <- (lassoResult[which.max(lassoResult$dev),])
      # 
      # set.seed(seed)
      # best_lam <- targetDF[which.min(targetDF$lambda),]$lambda
      # userLasso <- glmnet::glmnet(as.matrix(train_sub[,pred_c]), 
      #                             as.matrix(train_sub[,"obs"]),
      #                             lower.limits = 0,      #there is no sink for NO2 in the variables.
      #                             # type.measure = "mse",
      #                             alpha=1, family="gaussian", lambda = best_lam)
      # predName <- coef(userLasso)[-1,][which(coef(userLasso)[-1]!=0)] %>% names()
      # predCoef <- coef(userLasso)[-1][which(coef(userLasso)[-1]!=0)]
      # lur <- data.frame(predName=factor(predName, levels = predName), predCoef=predCoef)
      # intercept <- coef(userLasso)[1]
      # lm_lasso <- lm(as.formula(paste0("obs~", paste(predName, collapse = "+"))), train_sub)
      # summary(lm_lasso)
      # error_matrix(as.matrix(train_sub[, "obs"]),predict(userLasso, as.matrix(train_sub[,pred_c])))
      
      #f# SLR: test SLR
      source("scr/fun_output_slr_result.R")
      slr_poll <- output_slr_result(slr_model, test_df = test_sub, train_df = train_sub,
                                    output_filename = csv_name_fold, obs_varname = 'obs')
      
      slr_df <- slr_poll[[1]]
      
      #f# SLR: perform cross-validation
      
      #-----------#f# GWR: train GWR----------
      print("GWR")
      source("scr/fun_setupt_gwr.R")
      setup <- setup_gwr(train_sub, eu_bnd,
                         cellsize = 200000, local_crs = local_crs, xcoord="xcoord", ycoord="ycoord")
      sp_train <- setup[[1]]
      grd <- setup[[2]]
      DM <- setup[[3]]
      source("scr/fun_calibr_gwr.R")
      nngb <- calibr_gwr(sp_train, csv_name_fold)
      print(paste0("nngb: ", nngb))
      source("scr/fun_gwr.R")
      gwr_model <- gwr(sp_train, grd, DM, nngb, csv_name_fold)
      #f# GWR: perform cross-validation
      source("scr/fun_output_gwr_result.R")
      gwr_df <- output_gwr_result(gwr_model, train_sub, test_sub, local_crs,
                                  output_filename = csv_name_fold, xcoord="xcoord", ycoord="ycoord")
      if(fold_i==1){
         # Only plot the parameter surface for the first fold for every year
         source('scr/fun_plot_gwr_coef.R')
         plot_gwr_coef(yr_i, gwr_model, csv_name_fold, n_row = 3, n_col = 3, eu_bnd = eu_bnd)
      }
      # plot gwr surface
      ncol(gwr_model$SDF) %>% print()  # the number of predictors selected
      #--------- RF: split data into train, validation, and test data--------
      print("--------------- RF ---------------")
      set.seed(seed)
      # index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
      # train_df <- data_all[index$train, ]
      # valid_df <- data_all[index$valid, ]
      # test_df <- data_all[index$test, ]
      train_df <- train_sub
      test_df <- test_sub
      pred_c_rf <- c(pred_c) #"x_trun", "y_trun"
      x_varname = names(data_all %>% dplyr::select(matches(pred_c_rf)))
      print("RF predictors:")
      print(x_varname)
      ## LLO CV (small test for multiple years)
      
      if(tuneRF_b){
         #f# RF: tune hyperparameter
         hyper_grid <- expand.grid(
            mtry = seq(floor(sqrt(length(x_varname))), length(x_varname), by=20),
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
                               csv_name_fold, hyper_grid)
         
         #f# RF: train the model
         hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name_fold,".csv"))
      }
      source("scr/fun_opt_rf.R")
      # If tuneRF is False, a 500 number of trees and sqrt(x_varname no) of mtry woul be used
      rf_result <- opt_rf(train_df, test_df,
                          y_varname='obs',
                          x_varname = x_varname,
                          csv_name_fold, hyper_grid, tuneRF_b)
      source("scr/fun_plot_rf_vi.R")
      plot_rf_vi(csv_name_fold, var_no = 10)
      # Model Performance evaluation:
      # slr_poll$eval_train %>% print()
      # slr_poll$eval_test %>% print()
      # error_matrix(gwr_df[gwr_df$df_type=='train', 'obs'], gwr_df[gwr_df$df_type=='train', 'gwr']) %>%
      #    print()
      # error_matrix(gwr_df[gwr_df$df_type=='test', 'obs'], gwr_df[gwr_df$df_type=='test', 'gwr']) %>%
      #    print()
      rf_result$eval_train
      rf_result$eval_test
      # rf_result$rf_result %>% names
      # output all models' performance matrix
      output_em <- function(pred_df, csv_name, model, year, obs_name, pred_name){
         error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', 'gwr'])

         em <- rbind(error_matrix(pred_df[pred_df$df_type=='test', 'obs'], pred_df[pred_df$df_type=='test', model]) ,
                     error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', model])) %>%
            as.data.frame()
         # em[, c(1, 5, 7)]

         perf_matrix <- em %>% mutate(df_type=c('test','train'), model=model, n_fold=fold_i,
                                      year=year, csv_name=csv_name)
         perf_matrix
      }
      out_pm <- rbind(output_em(slr_df, csv_name_fold, 'slr', years[[yr_i]], "obs", "slr"),
                      output_em(gwr_df, csv_name_fold, 'gwr', years[[yr_i]], "obs", "gwr"),
                      output_em(rf_result$rf_result, csv_name_fold, 'rf', years[[yr_i]], "obs", "rf")
      )
      write.csv(out_pm, paste0("data/workingData/perf_m_",csv_name_fold, '.csv'), row.names = F)
   }
   
   parallel::stopCluster(cl)
   
} 
