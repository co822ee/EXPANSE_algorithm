output_gwr_result <- function(gwr.res.t, train_data, test_data, local_crs,
                              output_filename = csv_name, mixedGWR=F, outputcsv=T, 
                              xcoord="Xcoord", ycoord="Ycoord",
                              outputselect = c("station_european_code", "gwr", "obs", "res",
                                               "nfold", "df_type", "year", "index"), 
                              obs_varname = 'obs'){
   sp_train <- sp::SpatialPointsDataFrame(data = train_data,
                                          coords = cbind(train_data[, xcoord], train_data[, ycoord]),
                                          proj4string = local_crs)
   sp_test <- sp::SpatialPointsDataFrame(data = test_data,
                                         coords = cbind(test_data[, xcoord], test_data[, ycoord]),
                                         proj4string = local_crs)
   # Extract the pred coef values
   if(mixedGWR){
      coef_stack <- stack(gwr.res.t$SDF)
      names(coef_stack) <- substr(names(coef_stack), 1, nchar(names(coef_stack))-2)
   }else{
      coef_stack <- stack(gwr.res.t$SDF)
   }
   source("scr/fun_gen_df_gwr.R")
   gwr_test_df <- gen_df_gwr(coef_stack, sp_test, test_data)
   gwr_train_df <- gen_df_gwr(coef_stack, sp_train, train_data)
   gwr_train_df$df_type <- 'train'
   gwr_test_df$df_type <- 'test'
   gwr_df <- rbind(gwr_train_df, gwr_test_df)
   gwr_df$res <- gwr_df[, obs_varname] - gwr_df$gwr
   gwr_df <- gwr_df %>% rename(obs=obs_varname)
   gwr_df <- gwr_df[, outputselect]
   if(outputcsv){
      write.csv(gwr_df, 
                paste0('data/workingData/GWR_result_all_', output_filename, '.csv'), 
                row.names = F)
   }
   gwr_df
   
   # plot(gwr_df$gwr, gwr_df$obs)
   # abline(0, 1)
}
