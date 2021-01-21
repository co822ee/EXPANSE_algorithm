source("scr/fun_gen_pred_df.R")
slr_LUR <- function(df, y_varname, pred_varname, training = training, test = test, cv_n){
   # Run the SLR and return the error matrix
   slr_result <- slr(POLL = df[training, y_varname], 
                     pred = df[training, ] %>% dplyr::select(matches(pred_varname)), 
                     cv_n = cv_n)
   slr_model <- slr_result[[3]]
   
   poll_test <- gen_pred_df_slr(slr_model, df[test, ], station_info)
   poll_train <- gen_pred_df(slr_model,  df[training, ], station_info)
   eval_test <- error_matrix(poll_test$obs, poll_test$pred)
   eval_train <- error_matrix(poll_train$obs, poll_train$pred)
   
   return(eval_test)
}




