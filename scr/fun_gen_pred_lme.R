gen_pred_lme <- function(model, data_df, obs_varname){
   data.frame(lme = predict(model, data_df, allow.new.levels = T) %>% as.numeric()) %>% 
      cbind(obs = data_df[, obs_varname]) %>% 
      mutate(res = obs - lme) %>% 
      cbind(data_df %>% dplyr::select(-all_of(obs_varname)))
}