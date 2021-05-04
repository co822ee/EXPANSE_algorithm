source("scr/fun_call_lib.R")
# source("scr/o_00_00_read_data.R")
source("scr/o_00_00_read_data_timeVarying.R")
# Multiple single years
csv_names <- paste0('o_', c(2005:2012))   #2008:2012
years <- as.list(c(2005:2012))
# year_i=1
csv_names
nfold=5

write_output_5csv <- function(year_i){
   # paste0("GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv")
   # list.files("data/workingData/", paste0("GWR_result_all_", csv_names[year_i], "_fold"))
   slr <- lapply(paste0("data/workingData/SLR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                 read.csv)
   gwr <- lapply(paste0("data/workingData/GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                 read.csv)
   gwr_rf <- lapply(paste0("data/workingData/GWR_rf_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                 read.csv)
   rf <- lapply(paste0("data/workingData/RF_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                read.csv)
   slr_test <- lapply(slr, function(df_data) df_data %>% filter(df_type=='test'))
   gwr_test <- lapply(gwr, function(df_data) df_data %>% filter(df_type=='test'))
   gwr_rf_test <- lapply(gwr_rf, function(df_data) df_data %>% filter(df_type=='test'))
   rf_test <- lapply(rf, function(df_data) df_data %>% filter(df_type=='test'))
   
   slr_test <- do.call(rbind, slr_test)
   gwr_test <- do.call(rbind, gwr_test)
   gwr_rf_test <- do.call(rbind, gwr_rf_test)
   rf_test <- do.call(rbind, rf_test)
   
   all_test <- cbind(gwr=gwr_test$gwr, gwr_rf=gwr_rf_test$gwr_rf, rf=rf_test$rf, slr_test)
   write.csv(all_test, paste0("data/workingData/o_NO2_5cv_", years[[year_i]], ".csv"))
   
}
lapply(seq_along(csv_names), write_output_5csv)
