library(dplyr)
library(sf)
library(caret)  #data partition
library(splitstackshape)   #stratified function in this library is better than createDataPartition in library caret
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
## Read in data (elapse NO2 2010 with climate zones included)
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                       encoding = "utf-8")
## Read in data (airbase observations 1990s-2012)
no2 <- read.csv("../airbase/EXPANSE_APM/data/processed/ab_v8_yr_no2.csv")
# rename data
elapse_no2 <- rename(elapse_no2, station_european_code=ï..Station)
names(elapse_no2)
names(no2)
# reduce airbase data
no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value)
## subset stations that are included in the elapse (cause at this stage, we don't have the predictor maps...)
no2_e <- no2 %>% filter(no2$station_european_code%in%unique(elapse_no2$station_european_code))
no2_e 
no2_e_all <- left_join(no2_e, elapse_no2, by="station_european_code")
source("../airbase/EXPANSE_APM/src/fun_eda_spatial_distribution_annualobs_laea.R")
annual_spatial_dist(poll_conc = no2_e_all, eu_bnd = eu_bnd, 
                    folder_subnote = 'elapse_ab')
## subset samples (for multiple years or each year)
names(no2_e_all)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   no2_e_sub
}
#o# multiple years
no2_e_09_11 <- subset_df_yrs(no2_e_all, 2009:2011)
# #f# subset cross-validation data (5-fold cross-validation)
# #f# stratified by station types, climate zones and/or years
# no2_e_09_11$index <- 1:nrow(no2_e_09_11) 
# set.seed(123)  # good idea to set the random seed for reproducibility
# train_sub <- stratified(no2_e_09_11, c('type_of_st', 'year', 'climate_zone'), 0.8)
# test_sub <- no2_e_09_11[-train_sub$index, ]
# # Check whether the stratification works
# sum(train_sub$year==2010)/nrow(train_sub)
# sum(test_sub$year==2010)/nrow(test_sub)
# 
# sum(train_sub$type_of_st=="Background")/nrow(train_sub)
# sum(test_sub$type_of_st=="Background")/nrow(test_sub)


#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_change_dir.R")
change_dir(train_sub, neg_pred)
change_dir(test_sub, neg_pred)

#f# SLR: perform cross-validation
#f# GWR: define/preprocess predictors (direction of effect)
#f# GWR: read in SLR's selected predictors
#f# GWR: perform cross-validation

#f# RF: tune hyperparameter
#f# RF: train the model
#f# RF: perform cross-validation


