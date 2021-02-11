gen_pred_df <- function(model, data_df, obs_varname){
   # model is the final linear regression model 
   # data_df is the data_df in which predictor values and observations are stored (as well as the station info)
   # This function return the observations (obs) and predictions (pred) as well as station info.
   poll <- data.frame(slr = predict(model, data_df) %>% as.numeric(),
                      obs = data_df[, obs_varname]) %>% 
      mutate(res = obs - slr) %>% 
      cbind(data_df %>% dplyr::select(-all_of(obs_varname)))
   
   return(poll)
}