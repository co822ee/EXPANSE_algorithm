source("scr/fun_gen_pred_df.R")
output_slr_result <- function(model, test_df, train_df, output_filename, obs_varname){
   slr_poll_test <- gen_pred_df(model, test_df, obs_varname)
   slr_poll_train <- gen_pred_df(model, train_df, obs_varname)
   eval_test <- error_matrix(slr_poll_test$obs, slr_poll_test$slr)
   eval_train <- error_matrix(slr_poll_train$obs, slr_poll_train$slr)

   slr_poll <- rbind(slr_poll_train %>% mutate(df_type = 'train'),
                     slr_poll_test %>% mutate(df_type = 'test'))
   write.csv(slr_poll, 
             paste0('data/workingData/SLR_result_all_', output_filename, '.csv'), 
             row.names = F)
   return(list(slr_poll, eval_train=eval_train, eval_test=eval_test))
}