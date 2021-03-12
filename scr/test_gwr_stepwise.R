library(dplyr)
library(tidyr)
# stepwise
source("scr/fun_call_predictor.R")
source("scr/fun_call_lib.R")
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
                       encoding = "utf-8")
x_varname <- c(pred_c, neg_pred)
x_var <- names(elapse_no2 %>% dplyr::select(matches(x_varname)))
R2 <- vector("numeric", length=length(x_var))
output <- data.frame(variables=0, increR2=0)

source("scr/fun_read_data.R")
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
train_sub <- stratified(data_all, c('type_of_st', 'zoneID'), 0.8)
test_sub <- data_all[-train_sub$index, ]
data_all <- rbind(train_sub, test_sub)
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

# slr <- read.csv(paste0("data/workingData/SLR_summary_model_run1_train_break_noxy", year_target,".csv"))
# slr_poll <- read.csv(paste0('data/workingData/SLR_result_all_run1_train_break_noxy', year_target,".csv"))
# eq <- as.formula(paste0('obs~',  paste(slr$variables[-1], collapse = "+")))

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
# if(!file.exists(paste0("data/workingData/GWR_dist_", 
#                        kernel_type, "_", year_target, ".txt"))){
#    DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
#                    rp.locat=coordinates(sp_train))
#    # 
#    bandwidth_calibr <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
#                               adaptive = F, dMat = DM_1)
#    write.table(bandwidth_calibr, paste0("data/workingData/GWR_dist_", 
#                                         kernel_type, "_", year_target, ".txt"))
# }
# 
# if(!file.exists(paste0("data/workingData/GWR_nngb_", 
#                        kernel_type, "_", year_target, ".txt"))){
#    DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
#                    rp.locat=coordinates(sp_train))
#    # 
#    nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
#                   adaptive = T, dMat = DM_1)
#    write.table(nngb, paste0("data/workingData/GWR_nngb_", 
#                             kernel_type, "_", year_target, ".txt"))
# }

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
      nngb <- read.table(paste0("data/workingData/GWR_nngb_run2_",
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
output
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
if(any(diff(output$increR2)<0.01)) output <- output[-(which(diff(output$increR2)<0.01)+1),]
write.table(output, paste0("data/workingData/stepGWR_", 
                           year_target, ".txt"), row.names = F)
#-----how optimal nngb changes over selection steps-------
output <- read.table(paste0("data/workingData/stepGWR_", 
                            year_target, ".txt"), header = T)

source("scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd, 
                   cellsize = reg_grdsize, local_crs = local_crs)
sp_train <- setup[[1]]
grd <- setup[[2]]
DM <- setup[[3]]
DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                rp.locat=coordinates(sp_train))

var_i <- 1
# nngb_l <- vector("numeric", length=length(output$variables))
# Step-wise
nngb_l <- lapply(seq_along(output$variables), function(var_i){
   eq <- as.formula(paste0('obs~', paste(output$variables[1:var_i], collapse = "+")))
   bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
          adaptive = T, dMat = DM_1)
})
nngb_l %>% unlist()

# one-by-one
lapply(seq_along(output$variables), function(var_i){
   eq <- as.formula(paste0('obs~', paste(output$variables[var_i], collapse = "+")))
   bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
          adaptive = T, dMat = DM_1)
}) %>% unlist()

#-------------p value evaluation----------
output <- read.table(paste0("data/workingData/stepGWR_", 
                            year_target, ".txt"), header = T)
eq <- as.formula(paste0('obs~', paste(output$variables, collapse = "+")))
source("scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd, 
                   cellsize = reg_grdsize, local_crs = local_crs)
sp_train <- setup[[1]]
grd <- setup[[2]]
DM <- setup[[3]]
DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                rp.locat=coordinates(sp_train))
nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
               adaptive = T, dMat = DM_1)
gwr_model <- gwr.basic(eq, sp_train,
                       adaptive = T, bw=nngb, kernel=kernel_type, 
                       F123.test = TRUE)

#----------VIF evaluation----------
output <- read.table(paste0("data/workingData/stepGWR_", 
                            year_target, ".txt"), header = T)
eq <- as.formula(paste0('obs~', paste(output$variables, collapse = "+")))
lm_model <- lm(eq, train_sub)
library(car)
vif(lm_model)<3
# All are below 3 (No collinearity in global regression model)
# Check local collinearity for basic GWR
source("scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd, 
                   cellsize = reg_grdsize, local_crs = local_crs)
sp_train <- setup[[1]]
grd <- setup[[2]]
DM <- setup[[3]]
DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                rp.locat=coordinates(sp_train))
nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
               adaptive = T, dMat = DM_1)
gwr_collin <- gwr.collin.diagno(eq, sp_train, kernel = kernel_type, bw=nngb,
                                adaptive = T, dMat = DM_1)
which(gwr_collin$VIF>=3, arr.ind = T)
which(gwr_collin$VIF>=5, arr.ind = T)
which(gwr_collin$local_CN>=3)
which(gwr_collin$local_CN>=10)
which(gwr_collin$VIF>=10, arr.ind = T)
hist(gwr_collin$local_CN)
length(which(gwr_collin$local_CN>=20))/length(gwr_collin$local_CN)
# For some observation points, the VIFs of some predictors exceed 3
# It is not ideal to remove the predictors, when only a local collinearity effect is present.
# We can do nothing, but such collinearity effects can cause a loss of precision and power in the coefficient estimates.
# Also, studies have shown that GWR may find patterns in the coefficients where no spatial patterns are actually present (ref).
# Therefore, here we try to use locally-compensated GW regression (with ridge term)
# Biased local estimations are only used at locations where collinearity is likely to be an issue
nngb_lcr <- bw.gwr.lcr(eq, sp_train, kernel = kernel_type,
                       lambda.adjust = T, cn.thresh = 20,  #commonly set between 20 and 30 # lambda=1, 
                       adaptive = T, dMat = DM_1)
gwr_lcr <- gwr.lcr(eq, sp_train, regression.points=grd,
                   lambda.adjust = T, cn.thresh = 20,   # lambda=1, 
                   adaptive = T, bw=nngb_lcr, dMat=DM, kernel=kernel_type)
summary(gwr_lcr$SDF$Local_CN)
gwr_model <- gwr.basic(eq, sp_train, regression.points=grd,
                       adaptive = T, bw=nngb, dMat=DM, kernel=kernel_type)
source('scr/fun_plot_gwr_coef.R')
plot_gwr_coef(i, gwr_lcr, paste0(csv_name, "_lcr20"), n_row = 3, n_col = 3, eu_bnd = eu_bnd)
plot_gwr_coef(i, gwr_model, csv_name, n_row = 2, n_col = 3, eu_bnd = eu_bnd)
# compare the two result
# plot(gwr_model$SDF$Intercept, gwr_lcr$SDF$Intercept)
# plot(gwr_model$SDF$ROADS_EU_5p, gwr_lcr$SDF$ROADS_EU_5p)
windows()
par(mfrow=c(2,3))
for(plot_i in seq_along(names(gwr_model$SDF))){
   plot(gwr_model$SDF[[plot_i]], gwr_lcr$SDF[[plot_i]], main=names(raster(gwr_model$SDF[plot_i])))
   abline(a=0, b=1)
}
gwr_plot <- vector(mode = "list", length = ncol(gwr_model$SDF))
for(plot_i in seq_along(names(gwr_model$SDF))){ #seq_along(names(gwr_model$SDF))
   gwr_plot[[plot_i]] <- tm_shape((raster(gwr_lcr$SDF[plot_i])-raster(gwr_model$SDF[plot_i]))/raster(gwr_model$SDF[plot_i])*100)+
      tm_raster(palette = viridis(5), style = "cont", title = '')+
      tm_shape(eu_bnd) +
      tm_borders(col='black')+
      tm_layout(legend.title.size = 1, legend.text.size = 0.8, 
                legend.text.color = 'yellow', 
                title = names(raster(gwr_model$SDF[plot_i])),
                title.color = 'black')
}
gwr_plot$nrow <- 2
gwr_plot$ncol <- 3
mergeMap <- do.call(tmap_arrange, gwr_plot)
tmap_save(mergeMap, filename = paste0('graph/gwr_coef/', csv_name, "_gwr-lcr.tiff"), 
          dpi=100, height=10, width=10, units='in')


#---------evaluate output----------
output <- read.table(paste0("data/workingData/stepGWR_", 
                            year_target, ".txt"), header = T)
read.csv(paste0("data/workingData/SLR_summary_model_run2_", year_target, ".csv"), header=T)[,c(1,2,7)]
eq_gwr <- as.formula(paste0("obs~", paste(output$variables, collapse = "+")))
# Optimize nngb (adaptive bandwidth)
nngb <- bw.gwr(eq_gwr, data=sp_train, approach = "CV", kernel = kernel_type,
               adaptive = T, dMat = DM_1)
gwr_m <- tryCatch(gwr.basic(eq_gwr,
                            data=sp_train,
                            regression.points=grd,
                            adaptive = T,
                            bw=nngb,
                            dMat=DM,
                            kernel=kernel_type), 
                  error=function(e) T)
if(typeof(gwr_m)!='logical'){
   source("scr/fun_output_gwr_result.R")
   gwr_pred <- output_gwr_result(gwr_m, train_sub, test_sub, local_crs,
                               output_filename = csv_name)
}
ncol(gwr_m$SDF)
source('scr/fun_plot_gwr_coef.R')
plot_gwr_coef(1, gwr_m, csv_name = csv_name, 
              n_row = 3, n_col = 3, eu_bnd=eu_bnd)

output_em <- function(pred_df, csv_name, model, year){
   error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', 'gwr'])
   
   em <- rbind(error_matrix(pred_df[pred_df$df_type=='test', 'obs'], pred_df[pred_df$df_type=='test', model]) , 
               error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', model])) %>% 
      as.data.frame()
   
   perf_matrix <- em[, c(1, 5, 7)] %>% mutate(df_type=c('test','train'), model=model, year=year, csv_name=csv_name)
   perf_matrix
}
output_em(gwr_pred, csv_name, 'gwr', year_target)

# error_matrix(as.data.frame(sp_train)$obs, gwr_pred$gwr)  

gwr_pred_step <- gwr_pred
gwr_pred_slr <- read.csv(paste0("data/workingData/GWR_result_all_run2_", 
                                year_target, '.csv'))
gwr_all <- data.frame(gwr_step=gwr_pred_step$gwr, gwr_slr=gwr_pred_slr$gwr,
                      obs=gwr_pred_slr$obs)
ggplot(gwr_all)+
   geom_point(aes(x=gwr_step, gwr_slr))+
   geom_abline(slope=1, intercept=0)
cor(gwr_all$gwr_step, gwr_all$gwr_slr)^2

ggplot(gwr_all %>% pivot_longer(c('gwr_slr', 'gwr_step'), 
                                 names_to = "model", values_to = "prediction"))+
   geom_point(aes(x=prediction, y=obs))+
   facet_grid(.~model)
