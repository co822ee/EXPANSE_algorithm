csv_name <- csv_names[yr_i]
no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[yr_i]])
# For elapse country only
# elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate_omi.csv",
#                        encoding = "utf-8")
# no2_e_09_11 <- no2_e_09_11[no2_e_09_11$sta_code%in%elapse_no2$station_european_code, ]
# subgroup for the multiple year modelling for the availability (not done)
source("scr/fun_create_fold.R")
data_all1 <- create_fold(no2_e_09_11, seed, strt_group=c("sta_type", "zoneID"), 
                         multiyr_vargroup = "sta_code", 
                         nfold = 5)

csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
test_sub <- data_all1[data_all1$nfold==fold_i,]
train_sub <- data_all1[-test_sub$index, ] #data_all1$index starts from 1 to the length.
