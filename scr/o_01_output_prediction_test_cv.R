source("scr/fun_call_lib.R")
# source("scr/o_00_00_read_data.R")
source("../expanse_multiyear/src/00_fun_read_data_gee.R")
# Multiple single years
target_poll = c('PM2.5', 'PM10', 'NO2', 'O3')
csv_names <- gsub('SLR_result_all_', '', 
                  list.files('data/workingData/', 
                             'SLR_result_all_o3_')) %>% 
   strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique


# year_i=1
csv_names
nfold=5

write_output_5csv <- function(year_i){
   print(csv_names[year_i])
   # paste0("GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv")
   # list.files("data/workingData/", paste0("GWR_result_all_", csv_names[year_i], "_fold"))
   print('slr')
   slr <- lapply(paste0("data/workingData/SLR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                 read.csv)
   print('gwr')
   # GWR_result_all_o3_PM2.5_2007_fold_1.csv is not available.
   gwr <- lapply(paste0('data/workingData/', list.files('data/workingData/', paste0("GWR_result_all_", csv_names[year_i]))), 
                 read.csv)
   # gwr_rf <- lapply(paste0("data/workingData/GWR_rf_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
   #               read.csv)
   print('rf')
   rf <- lapply(paste0("data/workingData/RF_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                read.csv)
   slr_test <- lapply(slr, function(df_data) df_data %>% filter(df_type=='test'))
   gwr_test <- lapply(gwr, function(df_data) df_data %>% filter(df_type=='test'))
   # gwr_rf_test <- lapply(gwr_rf, function(df_data) df_data %>% filter(df_type=='test'))
   rf_test <- lapply(rf, function(df_data) df_data %>% filter(df_type=='test'))
   
   slr_test <- do.call(rbind, slr_test)
   gwr_test <- do.call(rbind, gwr_test)
   # gwr_rf_test <- do.call(rbind, gwr_rf_test)
   rf_test <- do.call(rbind, rf_test)
   if(length(list.files('data/workingData/', paste0("GWR_result_all_", csv_names[year_i])))!=5){
      all_test <- cbind(gwr=gwr_test$gwr, 
                        rf=rf_test$rf[rf_test$index%in%gwr_test$index], 
                        slr_test[slr_test$index%in%gwr_test$index,])
   }else{
      all_test <- cbind(gwr=gwr_test$gwr, rf=rf_test$rf, slr_test)
   }
   write.csv(all_test, paste0("data/workingData/5cv_", csv_names[year_i], ".csv"), row.names = F)
   
}
lapply(seq_along(csv_names), write_output_5csv)
