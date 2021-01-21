gen_pred_df <- function(model, df, obs_varname){
   # model is the final linear regression model 
   # df is the df in which predictor values and observations are stored (as well as the station info)
   # This function return the observations (obs) and predictions (pred) as well as station info.
   poll <- data.frame(slr = predict(model, df) %>% as.numeric(),
                      obs = df[, obs_varname]) %>% 
      mutate(res = obs - slr) %>% 
      cbind(df %>% dplyr::select(-all_of(obs_varname)))
   
   return(poll)
}