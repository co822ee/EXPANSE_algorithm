library(dplyr)
library(raster)
library(sf)
library(car)  # for running slr
library(GWmodel)  #gwr
library(viridis)  #palette for raster
library(ranger) # Random forests
library(caret)  #data partition
library(splitstackshape)   #stratified function in this library is better than createDataPartition in library caret
library(splitTools)
library(APMtools)
library(lme4) # linear mixed effect models
library(CAST) # For dividing training and test data (CreateSpacetimeFolds)
library(performance) #extract model performance matrix for lme
seed <- 123
local_crs <- CRS("+init=EPSG:3035")

eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
## Read in data (elapse NO2 2010 with climate zones included)
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                       encoding = "utf-8")
## Read in data (airbase observations 1990s-2012)
no2 <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_no2.csv")
# rename data
elapse_no2 <- rename(elapse_no2, station_european_code=ï..Station)
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
#o# multiple years

# names <- paste0('run1_train_', c('2010', '09-11', '08-12'))
# years <- list(2010, 2009:2011, 2008:2012)
names <- paste0('run1_train_', 2008:2012)
years <- as.list(seq(2008, 2012))
i=3
csv_name <- names[i]
print(csv_name)
no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
data_all <- no2_e_09_11
#f# subset cross-validation data (5-fold cross-validation)
#f# stratified by station types, climate zones and/or years
set.seed(seed)
data_all$index <- 1:nrow(data_all)
train_sub <- stratified(data_all, c('type_of_st', 'climate_zone'), 0.8)
test_sub <- data_all[-train_sub$index, ]


#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)
test_sub <- proc_in_data(test_sub, neg_pred)
source("scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd, 
                   cellsize = 200000, local_crs = local_crs)
sp_train <- setup[[1]]
grd <- setup[[2]]
DM <- setup[[3]]
DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                rp.locat=coordinates(sp_train))
x_varnames = names(data_all %>% dplyr::select(matches(pred_c)))
# eq <- as.formula(paste('obs~',x_varnames[i]))
# nngb <- bw.gwr(eq, data=sp_train, approach = "CV",
#                kernel = "exponential", adaptive = T, dMat = DM_1)
# 
# nngb %>% print()

# gwr_model <- gwr.basic(eq,
#           data=sp_train, 
#           regression.points=grd, 
#           adaptive = T,
#           bw=nngb, 
#           dMat=DM,
#           kernel='exponential')
DeVar = "obs"
InDeVars = x_varnames
#14:49 run the model
gwr_model_s <- model.selection.gwr(DeVar, InDeVars, data = sp_train, 
                                   bw=48, adaptive = T, kernel = "exponential", dMat = DM_1)
