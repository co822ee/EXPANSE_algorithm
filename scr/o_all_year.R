source('scr/fun_call_lib.R')
source("scr/o_00_00_read_data.R")
y_var <- "obs"
x_var <- pred_c
data_all <- no2_e_all %>% filter(year==2010)
source("scr/fun_slr_for.R")
print("SLR predictors:")
x_var
neg_pred <- pred_c[grepl("nat|ugr", pred_c)]
data_all <- data_all %>% as_tibble() %>% 
   mutate(across(matches(neg_pred), function(x) -x ))%>% as.data.frame()
slr_result <- slr(data_all[, y_var], data_all %>% dplyr::select(x_var) %>% as.data.frame(),
                  cv_n = "2010_all_o_")
slr_model <- slr_result[[3]]
slr_model %>% summary()

# # only elapse countries
# elapse_no2 <- read.csv("../test_EXPANSE_NO2/data/rawData/AIRBASE_NO2_2010_variables.csv",
#                        encoding = "utf-8")
# names(elapse_no2)
# data_all2 <- data_all %>% filter(sta_code%in%elapse_no2$Station)
# slr_result <- slr(data_all2[, y_var], data_all2 %>% dplyr::select(x_var) %>% as.data.frame(),
#                   cv_n = "2010_all_o_elapse")
# slr_model <- slr_result[[3]]
# slr_model %>% summary()
# source("scr/fun_gen_pred_df.R")
# slr_poll <- gen_pred_df(slr_model, data_all2, y_var)
# write.csv(slr_poll, 
#           paste0('data/workingData/SLR_result_all_', "2010_all_o_elapse", '.csv'), 
#           row.names = F)

