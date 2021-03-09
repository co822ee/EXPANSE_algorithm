gen_df_gwr <- function(coef_stack, sp_p, df_p, forstepwise=F){
   # extract coefficient values for each point
   coef_df <- lapply(seq_along(sp_p), function(loc_i) raster::extract(coef_stack, sp_p[loc_i,]))
   coef_df <- Reduce(rbind, coef_df)
   
   predictor_test <- cbind(Intercept=1, df_p %>% dplyr::select(colnames(coef_df)[-1]))
   gwr_test_pred <- (predictor_test * coef_df) %>% apply(., 1, sum)
   if(!forstepwise){
      gwr_test_df <- cbind(data.frame(gwr=gwr_test_pred), df_p)
   }else{
      gwr_test_df <- gwr_test_pred
   }
   gwr_test_df
}