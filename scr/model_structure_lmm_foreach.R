library(foreach)
library(doParallel)
#o# multiple years
c1 <- makeCluster(5)
registerDoParallel(c1)
names <- paste0('run1_train_', c('09-11', '08-12', '06-10', '06-12'))
years <- list(2009:2011, 2008:2012, 2006:2010, 2006:2012)
foreach(i = 1:4) %dopar% {
   names <- paste0('run1_train_', c('09-11', '08-12', '06-10', '06-12'))
   years <- list(2009:2011, 2008:2012, 2006:2010, 2006:2012)
   library(dplyr)
   library(raster)
   library(sf)
   library(tmap)
   library(car)  # for running slr
   library(GWmodel)  #gwr
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
   no2 <- read.csv("../airbase/EXPANSE_APM/data/processed/ab_v8_yr_no2.csv")
   # rename data
   elapse_no2 <- dplyr::rename(elapse_no2, station_european_code=ï..Station)
   # reduce airbase data
   no2 <- no2 %>% dplyr::rename(year=statistics_year, obs=statistic_value)
   ## subset stations that are included in the elapse (cause at this stage, we don't have the predictor maps...)
   no2_e <- no2 %>% filter(no2$station_european_code%in%unique(elapse_no2$station_european_code))
   no2_e_all <- left_join(no2_e, elapse_no2, by="station_european_code")
   
   ## subset samples (for multiple years or each year)
   subset_df_yrs <- function(obs_df, yr_target){
      no2_e_sub <- obs_df %>% filter(year%in%yr_target)
      no2_e_sub
   }
   csv_name <- names[i]
   csv_name
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
   data_all <- no2_e_09_11
   #f# subset cross-validation data (5-fold cross-validation)
   #f# stratified by station types, climate zones and/or years
   set.seed(seed)
   data_all$index <- 1:nrow(data_all)
   # train_sub <- stratified(data_all, c('type_of_st', 'climate_zone'), 0.8)
   # test_sub <- data_all[-train_sub$index, ]
   
   # Test only leave location out first 
   # Method 1: (easier to use)
   folds=CreateSpacetimeFolds(
      data_all,
      spacevar = "station_european_code",     # leave location out
      timevar = NA,
      k = 5,
      class = NA,
      seed = seed
   )
   # Method 2:
   # all_stations <- unique(data_all$station_european_code)
   # #we create cross validation folds using caret's createFolds
   # num_folds <- 5
   # set.seed(seed)  # for reproducibility
   # folds <- createFolds(all_stations,k=num_folds,list=TRUE,returnTrain=TRUE)
   
   train_sub <- data_all[folds$index[[1]], ]
   test_sub <- data_all[folds$indexOut[[1]], ]
   
   # # Check whether the leave out location works
   # any(unique(train_sub$station_european_code)%in%unique(test_sub$station_european_code))
   # # --> FALSE: the station IDs in training and test data are not the same
   # sum(train_sub$type_of_st=="Background")/nrow(train_sub)
   # sum(test_sub$type_of_st=="Background")/nrow(test_sub)
   # sum(train_sub$type_of_st=="Industrial")/nrow(train_sub)
   # sum(test_sub$type_of_st=="Industrial")/nrow(test_sub)
   # sum(train_sub$type_of_st=="Traffic")/nrow(train_sub)
   # sum(test_sub$type_of_st=="Traffic")/nrow(test_sub)
   # 
   # all_sub <- rbind(train_sub %>% mutate(df_type='train'), 
   #                  test_sub %>% mutate(df_type='test'))
   # all_sp <- SpatialPointsDataFrame(coords = cbind(all_sub$Xcoord, all_sub$Ycoord),
   #                                  all_sub, proj4string = local_crs)
   # tmap_mode('plot')
   # years <- all_sp$year %>% unique
   # maps_l <- lapply(years, function(year_i){
   #    map_1 <- tm_shape(all_sp[all_sp$year==year_i, ]) +
   #       tm_dots(size = 0.05, col="df_type",   # col = "NO2",          # popup.vars for showing values
   #               title = paste0('region'),
   #               palette=c('red','blue'))+
   #       tm_shape(eu_bnd)+
   #       tm_borders()+
   #       tm_layout(title=year_i)
   #    map_1
   # })
   # maps_l$nrow <- 1
   # do.call(tmap_arrange, maps_l)
   
   #TODO we need to look at the groups separately or in combined?
   #f# SLR: select predictors
   source("scr/fun_call_predictor.R")
   #f# SLR: define/preprocess predictors (direction of effect)
   source("scr/fun_slr_proc_in_data.R")
   train_sub <- proc_in_data(train_sub, neg_pred)
   test_sub <- proc_in_data(test_sub, neg_pred)
   #----------stepwise for lmm-----------
   source("scr/fun_slr_for_lme.R")
   slr_lme(train_sub$obs, train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame(), 
           stations = train_sub$station_european_code,
           years = train_sub$year,
           cv_n = csv_name)
   #------------------------Global-----------
   # LME
   # source("scr/fun_slr_lme.R")
   # lme_result <- slr_lme(train_sub$obs, 
   #                       train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame(), 
   #                       stations = train_sub$station_european_code,
   #                       cv_n = csv_name)
   source("scr/fun_slr_for.R")
   slr_result <- slr(train_sub$obs, train_sub %>% dplyr::select(matches(pred_c)) %>% as.data.frame(), 
                     cv_n = csv_name)
   
}
stopCluster(c1)

