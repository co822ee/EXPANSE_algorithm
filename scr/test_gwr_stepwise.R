library(dplyr)
# stepwise
source("scr/fun_call_predictor.R")
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                       encoding = "utf-8")
x_varname <- c(pred_c, neg_pred)
x_var <- names(elapse_no2 %>% dplyr::select(matches(x_varname)))
R2 <- vector("numeric", length=length(x_var))
output <- data.frame(variables=0, increR2=0)


#
seed <- 123
local_crs <- CRS("+init=EPSG:3035")

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

## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   no2_e_sub
}

regression_grd_cellsize <- 200   #km
year_target <- 2009
kernel_type <- "exponential"
reg_grdsize <- regression_grd_cellsize*1000
csv_name <- paste0('stepGWR_', regression_grd_cellsize, "_", year_target)

print(csv_name)
no2_e_09_11 <- subset_df_yrs(no2_e_all, year_target)
data_all <- no2_e_09_11

#f# stratified by station types, climate zones and/or years
set.seed(seed)
data_all$index <- 1:nrow(data_all)
train_sub <- stratified(data_all, c('type_of_st', 'climate_zone'), 0.8)
test_sub <- data_all[-train_sub$index, ]

# --> yes indeed the stations are not the same in training and test data
#TODO we need to look at the groups separately or in combined?
#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)
test_sub <- proc_in_data(test_sub, neg_pred)
#------------------Above code is needed for all algorithms----------------------
#---------#f# SLR: train SLR -----------

slr <- read.csv(paste0("data/workingData/SLR_summary_model_run1_train_break_noxy", year_target,".csv"))
slr_poll <- read.csv(paste0('data/workingData/SLR_result_all_run1_train_break_noxy', year_target,".csv"))
eq <- as.formula(paste0('obs~',  paste(slr$variables[-1], collapse = "+")))

#f# SLR: perform cross-validation

#-----------#f# GWR: train GWR----------
# set up GWR
print("GWR")
source("scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd, 
                   cellsize = reg_grdsize, local_crs = local_crs)
sp_train <- setup[[1]]
grd <- setup[[2]]
DM <- setup[[3]]
# tiff(paste0("graph/gwr_coef/coef_grid_", regression_grd_cellsize, '.tif'),
#      width = 5, height=4, units='in', res=100)
# plot(coordinates(grd), col='red', pch=18)
# plot(grd, add=T, col='dark green')
# plot(eu_bnd[1], pch=16, col='transparent',add=TRUE)
# plot(sp_train, add=T)
# dev.off()

# Calibrate bandwidth using CV
# The calibration is not influenced by the regression grid cell size
if(!file.exists(paste0("data/workingData/GWR_dist_", 
                       kernel_type, "_", year_target, ".txt"))){
   DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                   rp.locat=coordinates(sp_train))
   # 
   bandwidth_calibr <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
                              adaptive = F, dMat = DM_1)
   write.table(bandwidth_calibr, paste0("data/workingData/GWR_dist_", 
                                        kernel_type, "_", year_target, ".txt"))
}

if(!file.exists(paste0("data/workingData/GWR_nngb_", 
                       kernel_type, "_", year_target, ".txt"))){
   DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                   rp.locat=coordinates(sp_train))
   # 
   nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
                  adaptive = T, dMat = DM_1)
   write.table(nngb, paste0("data/workingData/GWR_nngb_", 
                            kernel_type, "_", year_target, ".txt"))
}

# nngb %>% print()
# source("scr/fun_gwr.R")

calibr_nngb <- T   # Optimize nngb for every selection step
DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                rp.locat=coordinates(sp_train))
#---Step1------------
library(doParallel)
library(foreach)
cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)
R2_l <- foreach(i=seq_along(x_var)) %dopar% {
   # This script is for doing stepwise selection for gwr
   library(dplyr)
   library(raster)
   library(sf)
   # library(car)  # for running slr
   library(GWmodel)  #gwr
   # library(viridis)  #palette for raster
   # library(ranger) # Random forests
   library(caret)  #data partition
   library(splitstackshape)   #stratified function in this library is better than createDataPartition in library caret
   library(splitTools)
   library(APMtools)
   # library(lme4) # linear mixed effect models
   library(CAST) # For dividing training and test data (CreateSpacetimeFolds)
   # library(performance) #extract model performance matrix for lme
   library(tmap)
   
   # stepwise
   x_var <- names(data.frame(sp_train) %>% dplyr::select(matches(x_varname)))
   eq_gwr <- as.formula(paste0("obs~", x_var[i]))
   
   # Optimize nngb (adaptive bandwidth size)
   if(calibr_nngb){
      nngb <- bw.gwr(eq_gwr, data=sp_train, approach = "CV", kernel = kernel_type,
                     adaptive = T, dMat = DM_1)
      print(paste0("nngb: ", nngb))
   }else{
      nngb <- read.table(paste0("data/workingData/GWR_nngb_run1_train_break_noxy",
                                year_target,".txt"))[,1]
   }
   gwr_m <- tryCatch(gwr.basic(eq_gwr,
                               data=sp_train,
                               regression.points=grd,
                               adaptive = T,
                               bw=nngb,
                               dMat=DM,
                               kernel=kernel_type), 
                     error=function(e) T)
   if(typeof(gwr_m)!='logical'){
      coef_stack <- stack(gwr_m$SDF)
      if(!any(minValue(coef_stack)[-1]<0)){  #coef surface values should be larger than zero
         source("scr/fun_gen_df_gwr.R")
         gwr_pred <- gen_df_gwr(coef_stack, sp_train, as.data.frame(sp_train), T)
         R2[i] <- error_matrix(as.data.frame(sp_train)$obs, gwr_pred)[8]  #explained_var
         # error_matrix(as.data.frame(sp_train)$obs, as.numeric(predict(lm(eq_gwr, as.data.frame(sp_train)))))
         # summary(lm(eq_gwr, as.data.frame(sp_train)))$adj.r.squared
         # summary(lm(eq_gwr, as.data.frame(sp_train)))$r.squared # explained_var  in error_matrix
      }
   }
   R2[i]
}
parallel::stopCluster(cl)
R2 <- unlist(R2_l)
x_highest <- x_var[which.max(R2)]
R2_highest <- max(R2)
step_i <- 1
output[step_i,] <- cbind(variables=x_highest, increR2=R2_highest)
#---step2-----
# step_i=2
#for
step_i=2
while(step_i<=10){  #(as.numeric(output[step_i,2])-as.numeric(output[step_i-1,2]))>=0.01
   print(step_i)
   x_var_new <- x_var[!(x_var%in%output$variables[1:(step_i-1)])]
   print(length(x_var_new))
   # write.table(x_var_new, "data/workingData/stepGWR_", step_i)
   R2 <- vector("numeric", length=length(x_var))
   
   cl <- parallel::makeCluster(5)
   doParallel::registerDoParallel(cl)
   R2_l <- foreach(i=seq_along(x_var_new)) %dopar% {
      # This script is for doing stepwise selection for gwr
      library(dplyr)
      library(raster)
      library(sf)
      # library(car)  # for running slr
      library(GWmodel)  #gwr
      # library(viridis)  #palette for raster
      # library(ranger) # Random forests
      library(caret)  #data partition
      library(splitstackshape)   #stratified function in this library is better than createDataPartition in library caret
      library(splitTools)
      library(APMtools)
      # library(lme4) # linear mixed effect models
      library(CAST) # For dividing training and test data (CreateSpacetimeFolds)
      # library(performance) #extract model performance matrix for lme
      library(tmap)
      
      # stepwise
      
      eq_gwr <- as.formula(paste0("obs~", paste(c(output$variables, x_var_new[i]), collapse = "+")))
      # Optimize nngb (adaptive bandwidth)
      if(calibr_nngb){
         nngb <- bw.gwr(eq_gwr, data=sp_train, approach = "CV", kernel = kernel_type,
                        adaptive = T, dMat = DM_1)
         print(paste0("nngb: ", nngb))
      }else{
         nngb <- read.table(paste0("data/workingData/GWR_nngb_run1_train_break_noxy",
                                   year_target,".txt"))[,1]
      }
      gwr_m <- tryCatch(gwr.basic(eq_gwr,
                                  data=sp_train,
                                  regression.points=grd,
                                  adaptive = T,
                                  bw=nngb,
                                  dMat=DM,
                                  kernel=kernel_type), 
                        error=function(e) T)
      if(typeof(gwr_m)!='logical'){
         coef_stack <- stack(gwr_m$SDF)
         if(!any(minValue(coef_stack)[-1]<0)){ #coef surface values should be larger than zero
            source("scr/fun_gen_df_gwr.R")
            gwr_pred <- gen_df_gwr(coef_stack, sp_train, as.data.frame(sp_train), T)
            R2[i] <- error_matrix(as.data.frame(sp_train)$obs, gwr_pred)[8]  #explained_var
            # error_matrix(as.data.frame(sp_train)$obs, as.numeric(predict(lm(eq_gwr, as.data.frame(sp_train)))))
            # summary(lm(eq_gwr, as.data.frame(sp_train)))$adj.r.squared
            # summary(lm(eq_gwr, as.data.frame(sp_train)))$r.squared # explained_var  in error_matrix
         }
      }
      R2[i]
   }
   parallel::stopCluster(cl)
   R2 <- unlist(R2_l)
   x_highest <- x_var_new[which.max(R2)]
   R2_highest <- max(R2)
   if(R2_highest!=0){
      output[step_i,] <- cbind(variables=x_highest, increR2=R2_highest)
      print(output)
      step_i <- step_i+1
   }else{
      break
   }
}
output
write.table(output, paste0("data/workingData/stepGWR_", 
                           year_target, ".txt"), row.names = F)
perfm <- lapply(paste0('data/workingData/', list.files('data/workingData/', 'model_perf_')), 
                function(file_name) read.csv(file_name, header=T) )
perfm <- do.call(rbind, perfm)
perfm <- perfm[!(duplicated(perfm$csv_name)&perfm$model=='slr'), ]
perfm$reg_grdsize <- c(NA, lapply(perfm$csv_name[2:nrow(perfm)], function(f) strsplit(f, '_')[[1]][2]) %>% unlist() %>% as.numeric())
perfm$kernel <- c(NA, lapply(perfm$csv_name[2:nrow(perfm)], function(f) strsplit(f, '_')[[1]][3]) %>% unlist() %>% as.character())
perfm$adaptive <- c(NA, lapply(perfm$csv_name[2:nrow(perfm)], function(f){
   ifelse((length(strsplit(f, '_')[[1]])>4)&(strsplit(f, '_')[[1]][5]=='ad'),
          'adaptive bandwidth', 'fixed bandwidth')
}) %>% unlist())
perfm$global <- c(NA, lapply(perfm$csv_name[2:nrow(perfm)], function(f){
   ifelse(((length(strsplit(f, '_')[[1]])==5)&(strsplit(f, '_')[[1]][5]=='mixed'))|
             ((length(strsplit(f, '_')[[1]])==6)&(strsplit(f, '_')[[1]][6]=='mixed')),
          paste0('global ', global_vars), paste0('local ', global_vars))
}) %>% unlist())
perfm %>% dplyr::filter(kernel=='exponential') %>% rbind(perfm[1,]) %>% arrange(rsq) %>% View
perfm %>% dplyr::filter(kernel=='gaussian')%>% rbind(perfm[1,]) %>% arrange(rsq)   %>% View
perfm %>% dplyr::filter(kernel=='gaussian')%>% rbind(perfm[1,]) %>% arrange(rsq)   %>% View

read.csv(paste0("data/workingData/SLR_summary_model_run1_train_break_noxy", 2009,".csv"))
