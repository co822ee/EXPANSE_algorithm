year_target <- 2009
csv_name <- paste0('run2_', year_target)
source("scr/fun_call_lib.R")
# Read in and clean no2_e_all data and subset_df_yrs function
source("scr/fun_read_data.R")
names(no2_e_all)
data_all <- subset_df_yrs(no2_e_all, year_target)
#f# stratified by station types, climate zones and/or years
set.seed(seed)
data_all$index <- 1:nrow(data_all)
train_sub <- stratified(data_all, c('type_of_st', 'zoneID'), 0.8)
test_sub <- data_all[-train_sub$index, ]
# process predictor values
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
pred_c_rf <- c(pred_c, neg_pred, "x_trun", "y_trun")
x_varname = names(data_all %>% dplyr::select(matches(pred_c_rf)))
hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name, '.csv'))

mtry <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$mtry
ntrees <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$ntrees
nngb <- read.table(paste0("data/workingData/GWR_nngb_exponential_", year_target, ".txt"))[,1]
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
error_matrix(test_sub$obs, test_pred)
## LLO CV (small test for multiple years)

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
# hyper_grid <- tune_rf(train_df, test_df, #valid_df,
#                       y_varname='obs',
#                       x_varname,
#                       csv_name, hyper_grid)

#f# RF: train the model
hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name, '.csv'))
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