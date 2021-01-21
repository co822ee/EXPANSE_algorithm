data_all <- no2_e_09_11
seed <- 123
n_fold <- 5
gen_train_test <- function(data_all, seed, n_fold){
   frac_init <- 1/n_fold
   set.seed(seed)
   # generate training and test data by stratifying the data
   data_all$index <- 1:nrow(data_all)
   id_region <- data_all %>% dplyr::select(index, year, climate_zone, type_of_st)
   train_id <- id_region %>% group_by(year, climate_zone) %>% sample_frac(frac_init) %>% ungroup
   test_id <- id_region %>% anti_join(train_id, by = 'index')
   
   train_data <- data_all %>% filter(index %in% train_id$index)
   test_data <- data_all %>% filter(index %in% test_id$index)
   
   training_id <- train_data$id
   test_id <- test_data$id
   return(list(training_id, test_id,
               train_data %>% dplyr::select(-id), 
               test_data %>% dplyr::select(-id)))
   #!?! should I use EU_data %>% group_by(REGION) %>% sample_frac(0.6) %>% ungroup
   #    instead??? Because for some stations there are fewer years available.
   # to test whether stratification is successful
   # TODO use splitTools library create_folds() to do stratified cross-validation
   # after we group the different comibation of the strata targeted group (climate_zone, year, type_of_st)
}