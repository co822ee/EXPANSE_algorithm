## Read in data (elapse NO2 2010 with climate zones included)
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv")
## Read in data (airbase observations 1990s-2012)
no2 <- read.csv("../airbase/EXPANSE_APM/data/processed/ab_v8_yr_no2.csv")
## subset samples (for multiple years or each year)
#f# subset cross-validation data (5-fold cross-validation)
#f# stratified by station types and/or years

#f# SLR: define/preprocess predictors (direction of effect)
#f# SLR: select predictors
#f# SLR: perform cross-validation
#f# GWR: define/preprocess predictors (direction of effect)
#f# GWR: read in SLR's selected predictors
#f# GWR: perform cross-validation

#f# RF: tune hyperparameter
#f# RF: train the model
#f# RF: perform cross-validation


