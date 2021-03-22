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
# --> yes indeed the stations are not the same in training and test data
#TODO we need to look at the groups separately or in combined?
#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)
test_sub <- proc_in_data(test_sub, neg_pred)
data_all <- rbind(train_sub, test_sub)

#--------global and local---------
output <- read.table(paste0("data/workingData/stepGWR_", 
                            year_target, ".txt"), header = T)
output_slr <- read.csv(paste0("data/workingData/SLR_summary_model_run2_", year_target, ".csv"), header = T)
eq <- as.formula(paste0('obs~', paste(output$variables, collapse = "+")))
eq_slr <- as.formula(paste0('obs~', paste(output_slr$variables[-1], collapse = "+")))
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
#37
nngb_aic <- bw.gwr(eq, data=sp_train, approach = "AIC", kernel = kernel_type,
               adaptive = T, dMat = DM_1)
#34
# Does not differ much

# nngb <- bw.gwr(eq_slr, data=sp_train, approach = "CV", kernel = kernel_type,
#                adaptive = T, dMat = DM_1)
# nngb_aic <- bw.gwr(eq_slr, data=sp_train, approach = "AIC", kernel = kernel_type,
#                    adaptive = T, dMat = DM_1)

output$variables
global_m <- expand.grid(rep(list(0:1), length(output$variables)+1)) # 0: local, 1: global
names(global_m) <- c('Intercept', output$variables)
global_vars <- vector("list", length=nrow(global_m))
for(i in 1:nrow(global_m)){
   if(length(which(global_m[i,]==1))!=0){
      global_vars[[i]]=names(global_m)[which(global_m[i,]==1)]
   }
}
global_vars <- global_vars[-length(global_vars)]  #Remove the situation where all variables are global (lm)
vars_i=3
gwr_mixed <- vector("list", length = length(global_vars))
R2 <- vector("numeric", length=length(global_vars))

library(foreach)
library(doParallel)
cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)
output_l <- foreach(vars_i=1:length(global_vars)) %dopar% {
   source("scr/fun_call_lib.R")
   if(is.null(global_vars[[vars_i]])){
      gwr_mixed[[vars_i]] <- tryCatch(gwr.basic(eq,
                                                data=sp_train,
                                                regression.points=grd,
                                                # fixed.vars = global_vars[[vars_i]],
                                                adaptive = T,
                                                bw=nngb,
                                                dMat=DM,
                                                kernel=kernel_type), 
                                      error=function(e) T)
      
      
      if(typeof(gwr_mixed[[vars_i]])!='logical'){
         # coef_stack <- stack(gwr_mixed[[vars_i]]$SDF)
         
         source("scr/fun_output_gwr_result.R")
         gwr_pred <- output_gwr_result(gwr_mixed[[vars_i]], train_sub, test_sub, local_crs,
                                       output_filename = paste0(csv_name, "_ad"), 
                                       mixedGWR = F, outputcsv = F)
         R2[vars_i] <- error_matrix(as.data.frame(sp_train)$obs, gwr_pred[gwr_pred$df_type=='train','gwr'])[8]  #explained_var
         
      }
   }else{
      gwr_mixed[[vars_i]] <- tryCatch(gwr.mixed(eq,
                                                data=sp_train,
                                                regression.points=grd,
                                                fixed.vars = global_vars[[vars_i]],
                                                adaptive = T,
                                                bw=nngb,
                                                dMat=DM,
                                                kernel=kernel_type), 
                                      error=function(e) T)
      if(typeof(gwr_mixed[[vars_i]])!='logical'){
         coef_stack <- stack(gwr_mixed[[vars_i]]$SDF)
         names(coef_stack)
         source("scr/fun_output_gwr_result.R")
         gwr_pred <- output_gwr_result(gwr_mixed[[vars_i]], train_sub, test_sub, local_crs,
                                       output_filename = paste0(csv_name, "_ad"), mixedGWR = F, outputcsv = F)
         R2[vars_i] <- error_matrix(as.data.frame(sp_train)$obs, gwr_pred[gwr_pred$df_type=='train','gwr'])[8]  #explained_var
         
      }
      
   }
   list(gwr_mixed[[vars_i]], R2[vars_i])
   
}
parallel::stopCluster(cl)
for(i in 1:length(output_l)){
   R2[i] <- output_l[[i]][[2]]
}
max(R2)
global_vars[[which.max(R2)]]
hist(R2)
boxplot(R2)

