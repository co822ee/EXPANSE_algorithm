read_validation <- function(target_poll, valid_str){
   # Read in observations and predictor values
   if(target_poll=='PM2.5'){
      poll_str <- 'pm25'
      
   }else if(target_poll=='NO2'){
      poll_str <- 'no2'
      
   }else if(target_poll=='O3'){
      poll_str <- 'o3'
      
   }else if(target_poll=='PM10'){
      poll_str <- 'pm10'
      
   }
   if(valid_str=='random'){
      df_all <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', poll_str, '_', valid_str, '.csv'))
   }else{
      df_all <- read.csv(paste0('../EXPANSE_predictor/data/raw/gee/pred_', poll_str, '_', valid_str, '.csv'))
   }
   df_all$zoneID <- as.factor(df_all$zoneID)
   df_all
}
