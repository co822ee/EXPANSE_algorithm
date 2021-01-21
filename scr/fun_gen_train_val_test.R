n_fold <- 5
train_v_sub <- stratified(data_all, c('type_of_st', 'year', 'climate_zone'), 1-1/n_fold)
test_sub <- data_all[-train_v_sub$index, ]

dim(train_v_sub)
dim(test_sub)

train_sub <- stratified(train_v_sub, c('type_of_st', 'year', 'climate_zone'), 1-1/n_fold)
