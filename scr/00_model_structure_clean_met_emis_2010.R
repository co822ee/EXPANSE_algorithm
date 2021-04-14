# This script run the three models for year 2010
# With Met and emission points available
source("scr/fun_call_lib.R")
seed <- 123
local_crs <- CRS("+init=EPSG:3035")

eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
## Read in data (elapse NO2 2010 with climate zones included)
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                       encoding = "utf-8")
## Read in data (airbase observations 1990s-2012)
airbase <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_checked.csv")
no2 <- airbase %>% filter(component_caption=="NO2")
# rename data
colnames(elapse_no2)[1] <- "station_european_code"
# elapse_no2 <- rename(elapse_no2, station_european_code=ï..Station)
# reduce airbase data
no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value)
## subset stations that are included in the elapse (cause at this stage, we don't have the predictor maps...)
no2_e <- no2 %>% filter(no2$station_european_code%in%unique(elapse_no2$station_european_code))
no2_e_all <- left_join(no2_e, elapse_no2, by="station_european_code")

## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   no2_e_sub
}
no2_2010 <- subset_df_yrs(no2_e_all, 2010) # There are 2399-2375 observations available in elapse but not in airbase

extra_dat <- read.csv("../EXPANSE_predictor/data/raw/gee/elapse_2010_no2_met_geeExtract.csv",
                       encoding = "utf-8")
extra_dat <- rename(extra_dat, station_european_code=station)
extra_dat <- extra_dat %>% mutate(sta_sum = ifelse(is.na(sta_sum), 0, sta_sum))
any(is.na(extra_dat))
data_all <- inner_join(no2_2010, extra_dat %>% dplyr::select("temp", "u_wind", "v_wind", "pressure", "sta_sum",
                                                      "precip", "station_european_code"))
data_all <- data_all %>% mutate(wind_speed=sqrt((u_wind^2)+(v_wind^2)))
pairs(data_all %>% dplyr::select(wind_speed, u_wind, v_wind, pressure, sta_sum, precip, temp, obs))
csv_names <- paste0('run2_met_emis_', 2010)   #2008:2012
years <- as.list(2010)
i=1
# Bug: the performance does not align with the previous one
csv_name <- csv_names[i]
print("********************************************")
print(csv_name)
data_all$year <- years[[i]]
print(paste0("year: ", unique(data_all$year)))
#f# subset cross-validation data (5-fold cross-validation)
#f# stratified by station types, climate zones and/or years
set.seed(seed)
data_all$index <- 1:nrow(data_all)
train_sub <- stratified(data_all, c('type_of_st', 'zoneID'), 0.8)
test_sub <- data_all[-train_sub$index, ]
data_all <- rbind(train_sub, test_sub)
boxplot(data_all$pressure)

#f# SLR: select predictors
source("scr/fun_call_predictor.R")
pred_c <- c(pred_c, "pressure", "temp", "sta_sum",
            "precip", "wind_speed")  #"v_wind", "u_wind"
neg_pred <- c(neg_pred, "precip", "wind_speed") #"v_wind", "u_wind"
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)
test_sub <- proc_in_data(test_sub, neg_pred)
data_all <- rbind(train_sub, test_sub)
#------------------Above code is needed for all algorithms----------------------
#---------#f# SLR: train SLR -----------
source("scr/fun_slr_for.R")
# source("scr/fun_slr.R")
# POLL=train_sub$obs
# pred <- train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame()
# cv_n = csv_name
# check the predictor variables
print("SLR predictors:")
train_sub %>% dplyr::select(matches(pred_c)) %>% names()
slr_result <- slr(train_sub$obs, train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame(), 
                  cv_n = csv_name)
slr_model <- slr_result[[3]]
#f# SLR: test SLR
source("scr/fun_output_slr_result.R")
slr_poll <- output_slr_result(slr_model, test_df = test_sub, train_df = train_sub,
                              output_filename = csv_name, obs_varname = 'obs')
slr_poll$eval_train %>% print()
slr_poll$eval_test %>% print()
slr_df <- slr_poll[[1]]

#f# SLR: perform cross-validation

#-----------#f# GWR: train GWR----------
print("GWR")
source("scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd, 
                   cellsize = 200000, local_crs = local_crs)
sp_train <- setup[[1]]
grd <- setup[[2]]
DM <- setup[[3]]
source("scr/fun_calibr_gwr.R")
nngb <- calibr_gwr(sp_train, csv_name)
print(paste0("nngb: ", nngb))
source("scr/fun_gwr.R")
gwr_model <- gwr(sp_train, grd, DM, nngb, csv_name)
#f# GWR: perform cross-validation
source("scr/fun_output_gwr_result.R")
gwr_df <- output_gwr_result(gwr_model, train_sub, test_sub, local_crs,
                            output_filename = csv_name)
error_matrix(gwr_df[gwr_df$df_type=='train', 'obs'], gwr_df[gwr_df$df_type=='train', 'gwr']) %>% 
   print()
error_matrix(gwr_df[gwr_df$df_type=='test', 'obs'], gwr_df[gwr_df$df_type=='test', 'gwr']) %>% 
   print()
# plot gwr surface
ncol(gwr_model$SDF) %>% print()  # the number of predictors selected
source('scr/fun_plot_gwr_coef.R')
plot_gwr_coef(i, gwr_model, csv_name, n_row = 3, n_col = 3, eu_bnd = eu_bnd)
##--------- RF: split data into train, validation, and test data--------
print("--------------- RF ---------------")
set.seed(seed)
# index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
# train_df <- data_all[index$train, ]
# valid_df <- data_all[index$valid, ]
# test_df <- data_all[index$test, ]
train_df <- train_sub
test_df <- test_sub
pred_c_rf <- c(pred_c, "x_trun", "y_trun")
x_varname = names(data_all %>% dplyr::select(matches(pred_c_rf), c("pressure", "temp",
                                                                   "precip", "v_wind", "u_wind", "wind_speed")))
print("RF predictors:")
print(x_varname)
## LLO CV (small test for multiple years)

#f# RF: tune hyperparameter
hyper_grid <- expand.grid(
   mtry = seq(30, length(x_varname), by=10),
   ntrees = seq(500,1000, by=200),
   OOB_RMSE = 0,
   OOB_R2 = 0,
   valid_RMSE = 0,
   valid_R2 = 0
)
source("scr/fun_tune_rf.R")
hyper_grid <- tune_rf(train_df, test_df, #valid_df,
                      y_varname='obs',
                      x_varname,
                      csv_name, hyper_grid)

#f# RF: train the model
hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
source("scr/fun_opt_rf.R")
rf_result <- opt_rf(train_df, test_df,
                    y_varname='obs',
                    x_varname = x_varname,
                    csv_name, hyper_grid)
rf_result$eval_train %>% print()
rf_result$eval_test %>% print()
source("scr/fun_plot_rf_vi.R")
plot_rf_vi(csv_name, var_no = 10)
#f# RF: perform cross-validation


