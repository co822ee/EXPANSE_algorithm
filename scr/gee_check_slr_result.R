source('scr/fun_call_lib.R')
elapse_no2 <- read.csv("../test_EXPANSE_NO2/data/rawData/AIRBASE_NO2_2010_variables.csv",
                       encoding = "utf-8")

#f# SLR: select predictors
# pred_c <- c(c('alt_t', 'pop2011'),  #'x_trun', 'y_trun',
#             'clc10',
#             'clc14',
#             'clc3',
#             'clc5',
#             'clc7',
#             'MAJRDS_EU',
#             'ROADS_EU',
#             c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT', 'DEHM_NO2_2010'),
#             'RES') #year
neg_pred <- c('alt_t', 'clc14', 'clc7')
y_var <- "NO2_2010"
station_info <- c(  'PREDNO2LURFULL',
                    'system:index',
                    'country_is',
                    'XY',
                    'REGION',
                    'Station',
                    'strata_run5',
                    'airid',
                    'type_of_st',
                    'Xcoord', 'Ycoord',
                    y_var)
#f# SLR: define/preprocess predictors (direction of effect)
data_all <- elapse_no2 %>% as_tibble() %>% 
   mutate(across(matches(neg_pred), function(x) -x )) %>% as.data.frame()
# TO DO: Truncated altitude;  
source("scr/fun_slr_for.R")

# check the predictor variables
print("SLR predictors:")
x_var <- data_all %>% dplyr::select(-matches(station_info)) %>% names()
x_var
POLL <- data_all[, y_var]
pred <- data_all %>% dplyr::select(x_var) %>% as.data.frame()

slr_model <- finalBestModel

slr_result <- slr(data_all[, y_var], data_all %>% dplyr::select(x_var) %>% as.data.frame(),
                  cv_n = "2010_all")
read.csv("data/workingData/SLR_summary_model_2010_all.csv", header=T)
slr_model <- slr_result[[3]]
slr_model %>% summary()
