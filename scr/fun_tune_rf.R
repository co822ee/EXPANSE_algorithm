tune_rf <- function(df_train, df_valid, y_varname, x_varname, csv_name, 
                    hyper_grid, seed=123){
   for(i in 1:nrow(hyper_grid)){
      eq <- as.formula(paste(y_varname, "~", paste(x_varname,collapse='+'), sep = ""))
      model <- ranger(
         formula = eq,
         data = df_train,
         num.trees = hyper_grid$ntrees[i],
         mtry = hyper_grid$mtry[i],
         seed = seed                        #keep the bootstrapping samples the same
      )
      
      # add OOB error to grid
      hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)   #prediction.error is the mean squared error.
      hyper_grid$OOB_R2[i] <- model$r.squared
      
      # add test RMSE
      pred_valid <- predict(model, df_valid) %>% predictions()
      obs_valid <- (df_valid[, y_varname] %>% as.data.frame())[,1]
      hyper_grid$valid_RMSE[i] <- (obs_valid-pred_valid)^2 %>%
         mean() %>% sqrt()
      hyper_grid$valid_R2[i] <- 1-mean((pred_valid-obs_valid)^2)/var(obs_valid)
   }
   
   print(paste0('output csv file: data/workingData/rf_hyper_grid_', csv_name, '.csv'))
   write.csv(hyper_grid, paste0('data/workingData/rf_hyper_grid_', csv_name, '.csv'),
             row.names = F)
   hyper_grid
}

