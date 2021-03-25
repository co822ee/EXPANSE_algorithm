csv_name <- paste0('run2_', target_yr)   #2008:2012
no2_e_09_11 <- subset_df_yrs(no2_e_all, target_yr)
# data_all <- no2_e_09_11
print(paste0("year: ", unique(no2_e_09_11$year)))
source("scr/fun_create_fold.R")
data_all1 <- create_fold(no2_e_09_11, seed)
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