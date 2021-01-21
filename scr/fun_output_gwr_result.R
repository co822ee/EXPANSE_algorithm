output_gwr_result <- function(gwr.res.t, train_data, test_data, local_crs,
                              output_filename = csv_name){
   sp_train <- sp::SpatialPointsDataFrame(data = train_data,
                                          coords = cbind(train_data$Xcoord, train_data$Ycoord),
                                          proj4string = local_crs)
   sp_test <- sp::SpatialPointsDataFrame(data = test_data,
                                         coords = cbind(test_data$Xcoord, test_data$Ycoord),
                                         proj4string = local_crs)
   # Extract the pred coef values
   coef_stack <- stack(gwr.res.t$SDF)
   source("scr/fun_gen_df_gwr.R")
   gwr_test_df <- gen_df_gwr(coef_stack, sp_test, test_data)
   gwr_train_df <- gen_df_gwr(coef_stack, sp_train, train_data)
   gwr_train_df$df_type <- 'train'
   gwr_test_df$df_type <- 'test'
   gwr_df <- rbind(gwr_train_df, gwr_test_df)
   gwr_df$res <- gwr_df$obs - gwr_df$gwr
   write.csv(gwr_df, 
             paste0('data/workingData/GWR_result_all_', output_filename, '.csv'), 
             row.names = F)
   gwr_df
   
   # plot(gwr_df$gwr, gwr_df$obs)
   # abline(0, 1)
}
