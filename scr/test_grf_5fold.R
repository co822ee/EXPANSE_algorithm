year_target <- 2009
csv_name <- paste0('run2_', year_target)
source("scr/fun_call_lib.R")
# Read in and clean no2_e_all data and subset_df_yrs function
source("scr/fun_read_data.R")
no2_e_09_11 <- subset_df_yrs(no2_e_all, year_target)
# data_all <- no2_e_09_11
print(paste0("year: ", unique(no2_e_09_11$year)))
source("scr/fun_create_fold.R")
data_all1 <- create_fold(no2_e_09_11, seed)
fold_i <- 1
source('scr/fun_call_lib.R')
csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
test_sub <- data_all1[data_all1$nfold==fold_i,]
train_sub <- data_all1[-test_sub$index, ] #data_all1$index starts from 1 to the length.

#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)
test_sub <- proc_in_data(test_sub, neg_pred)
data_all <- rbind(train_sub, test_sub)


print("RF")
set.seed(seed)
train_df <- train_sub
test_df <- test_sub
pred_c_rf <- c(pred_c) #"x_trun", "y_trun"
x_varname = names(data_all %>% dplyr::select(matches(pred_c_rf)))
if(file.exists(paste0("data/workingData/rf_hyper_grid_", csv_name_fold,".csv"))){
   hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name_fold,".csv"))
   mtry <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$mtry
   ntrees <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$ntrees
}else{
   mtry =  floor(length(x_varname))
   ntrees <- 500
}


nngb <- read.table(paste0("data/workingData/GWR_nngb_", csv_name_fold, ".txt"))[,1]
print("predictors in RF: ")
print(train_df %>% dplyr::select(x_varname, obs) %>% names)

# the priority belongs to the last library loaded
# Because both library randomForest and ranger have importance()
# it gives the error of no applicable method for importance because the importance function in ranger lib is used
# 
detach("package:ranger", unload=TRUE)
grf_m <- grf(formula=as.formula(paste0('obs~', paste(x_varname, collapse = "+"))), 
    dframe=train_df %>% dplyr::select(x_varname, obs), bw=nngb,
    kernel='adaptive', coords=train_df %>% dplyr::select("Xcoord", "Ycoord"),
    ntree=ntrees)
predict_grf <- function (object, new.data, x.var.name, y.var.name, local.w = 1, 
                         global.w = 0, ...) 
{
   Obs <- nrow(new.data)
   predictions <- vector(mode = "numeric", length = Obs)
   for (i in 1:Obs) {
      x <- new.data[i, which(names(new.data) == x.var.name)]
      y <- new.data[i, which(names(new.data) == y.var.name)]
      locations <- object$Locations
      D <- sqrt((x - locations$Xcoord)^2 + (y - locations$Ycoord)^2)
      local.model.ID <- which.min(D)
      g.prediction <- predict(object[[1]], new.data[i, ])
      l.prediction <- predict(object$Forests[[local.model.ID]], 
                              new.data[i, ])
      predictions[i] <- global.w * g.prediction[1] + local.w * 
         l.prediction[1]
   }
   return(predictions)
}

test_pred <- predict_grf(object=grf_m, new.data=as.data.frame(test_sub), 
                     x.var.name = "Xcoord", y.var.name = 'Ycoord', local.w=1, global.w=0)
error_matrix(test_sub$obs, test_pred)  # Better than GWR (0.5516393) run2_2010_fold_1

