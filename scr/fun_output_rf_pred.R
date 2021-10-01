## This code outputs predictions of random forests built using training data
## at validation points 
output_rf_pred <- function(df_train, df_test, y_varname, x_varname, 
                   hyper_grid, seed=123, tuneRF=T){
   if(tuneRF){
      mtry <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$mtry
      ntrees <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$ntrees
      
      eq <- as.formula(paste(y_varname, "~", paste(x_varname,collapse='+'), sep = ""))
      rf_model <- ranger(
         formula = eq,
         data = df_train,
         num.trees = ntrees,
         mtry = mtry,
         seed = seed,
         importance = 'impurity'          # 'permutation'
      )
   }else{
      
      eq <- as.formula(paste(y_varname, "~", paste(x_varname,collapse='+'), sep = ""))
      rf_model <- ranger(
         formula = eq,
         data = df_train,
         num.trees = 500,
         mtry = floor(sqrt(length(x_varname))),
         seed = seed,
         importance = 'impurity'          # 'permutation'
      )
   }
   

   rf_result <- data.frame(rf = predict(rf_model, df_test) %>% predictions()) %>% 
      cbind(df_test %>% dplyr::select(-all_of(x_varname[x_varname!='year']))) 
   
   rf_result
   
}