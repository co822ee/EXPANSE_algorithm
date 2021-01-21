library(dplyr)
library(raster)
library(sf)
library(car)  # for running slr
library(GWmodel)  #gwr
library(ranger) # Random forests
library(caret)  #data partition
library(splitstackshape)   #stratified function in this library is better than createDataPartition in library caret
library(splitTools)
library(APMtools)

seed <- 123

eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
## Read in data (elapse NO2 2010 with climate zones included)
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                       encoding = "utf-8")
## Read in data (airbase observations 1990s-2012)
no2 <- read.csv("../airbase/EXPANSE_APM/data/processed/ab_v8_yr_no2.csv")
# rename data
elapse_no2 <- rename(elapse_no2, station_european_code=ï..Station)
# reduce airbase data
no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value)
## subset stations that are included in the elapse (cause at this stage, we don't have the predictor maps...)
no2_e <- no2 %>% filter(no2$station_european_code%in%unique(elapse_no2$station_european_code))
no2_e_all <- left_join(no2_e, elapse_no2, by="station_european_code")
source("../airbase/EXPANSE_APM/src/fun_eda_spatial_distribution_annualobs_laea.R")
annual_spatial_dist(poll_conc = no2_e_all, eu_bnd = eu_bnd, 
                    folder_subnote = 'elapse_ab')
## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   no2_e_sub
}
#o# multiple years
csv_name <- 'run1_train_09-11'
no2_e_09_11 <- subset_df_yrs(no2_e_all, 2009:2011)
data_all <- no2_e_09_11
#f# subset cross-validation data (5-fold cross-validation)
#f# stratified by station types, climate zones and/or years
data_all$index <- 1:nrow(data_all)
set.seed(seed)  # for reproducibility
train_sub <- stratified(data_all, c('type_of_st', 'year', 'climate_zone'), 0.8)
test_sub <- data_all[-train_sub$index, ]
# Check whether the stratification works
sum(train_sub$year==2010)/nrow(train_sub)
sum(test_sub$year==2010)/nrow(test_sub)

sum(train_sub$year==2010&train_sub$type_of_st=="Background"&train_sub$climate_zone==1)/nrow(train_sub)
sum(test_sub$year==2010&test_sub$type_of_st=="Background"&test_sub$climate_zone==1)/nrow(test_sub)
#TODO we need to look at the groups separately or in combined?
#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)
test_sub <- proc_in_data(test_sub, neg_pred)

#f# SLR: train SLR
source("scr/fun_slr.R")
slr_result <- slr(train_sub$obs, train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame(), 
    cv_n = csv_name)
slr_model <- slr_result[[3]]
#f# SLR: test SLR
source("scr/fun_output_slr_result.R")
slr_poll <- output_slr_result(slr_model, test_df = test_sub, train_df = train_sub,
                              output_filename = csv_name, obs_varname = 'obs')
slr_poll$eval_train
slr_poll$eval_test
slr_df <- slr_poll[[1]]
#f# SLR: perform cross-validation

#f# GWR: train GWR
source("scr/fun_gwr.R")
gwr_model <- gwr(train_sub, test_sub, eu_bnd, 200000, csv_name, CRS("+init=EPSG:3035"))
#f# GWR: perform cross-validation
source("scr/fun_output_gwr_result.R")
gwr_df <- output_gwr_result(gwr_model, train_sub, test_sub, CRS("+init=EPSG:3035"),
                            output_filename = csv_name)
error_matrix(gwr_df[gwr_df$df_type=='train', 'obs'], gwr_df[gwr_df$df_type=='train', 'gwr'])
error_matrix(gwr_df[gwr_df$df_type=='test', 'obs'], gwr_df[gwr_df$df_type=='test', 'gwr'])
## RF: split data into train, validation, and test data
set.seed(123)
index <- partition(data_all$country_code, p=c(train=0.6, valid=0.2, test=0.2))
# train_df <- data_all[index$train, ]
# valid_df <- data_all[index$valid, ]
# test_df <- data_all[index$test, ]
train_df <- train_sub
test_df <- test_sub
#f# RF: tune hyperparameter
hyper_grid <- expand.grid(
   mtry = seq(30, length(x_varname), by=10),
   ntrees = seq(500,1500, by=200),
   OOB_RMSE = 0,
   OOB_R2 = 0,
   valid_RMSE = 0,
   valid_R2 = 0
)
source("scr/fun_tune_rf.R")
hyper_grid <- tune_rf(train_df, test_df, #valid_df,
                      y_varname='obs', 
                      x_varname = names(df_train %>% dplyr::select(matches(pred_c))),
                      csv_name, hyper_grid)

#f# RF: train the model
hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name, '.csv'))
source("scr/fun_opt_rf.R")
rf_result <- opt_rf(train_df, test_df, 
                    y_varname='obs', 
                    x_varname = names(df_train %>% dplyr::select(matches(pred_c))),
                    csv_name, hyper_grid)
rf_result$eval_train
rf_result$eval_test
source("scr/fun_plot_rf_vi.R")
plot_rf_vi(csv_name, var_no = 10)
#f# RF: perform cross-validation


