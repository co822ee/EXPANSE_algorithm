library(dplyr)
library(APMtools)
library(lme4) # linear mixed effect models
library(CAST) # For dividing training and test data (CreateSpacetimeFolds)
seed <- 123

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
model_scenario <- c('09-11', '08-12', '06-10', '06-12')
csv_names <- paste0('run1_train_', c('09-11', '08-12', '06-10', '06-12'))
years <- list(2009:2011, 2008:2012, 2006:2010, 2006:2012)
lme_result <- lapply(paste0("data/workingData/LME_summary_model_", csv_names, ".csv") , read.csv)
slr_result <- lapply(paste0("data/workingData/SLR_summary_model_", csv_names, ".csv") , read.csv)

out_pred <- function(i, model_result, modelname){
   data.frame(pred=model_result[[i]]$variables[-1], 
              period=paste0(substr(csv_names[[i]], 12, nchar(csv_names[[i]])),
                            "_", modelname))
}
slr_pred <- lapply(seq_along(years), out_pred, model_result=slr_result, "slr")
lme_pred <- lapply(seq_along(years), out_pred, model_result=lme_result, "lme")
slr_pred <- do.call(rbind, slr_pred)
lme_pred <- do.call(rbind, lme_pred)
# table(rbind(slr_pred, lme_pred))
tbl_comp <- table(rbind(slr_pred, lme_pred))
tbl_comp <- tbl_comp %>% apply(., 1, sum) %>% cbind(tbl_comp,total=.)
tbl_comp <- tbl_comp[order(tbl_comp[,"total"], row.names(tbl_comp), decreasing = T), ]
tbl_comp


# tbl_comp_a <- as.data.frame(tbl_comp) %>% mutate(model=substr(period, 7, 10)) %>% 
#    group_by(pred, model) %>% 
#    summarise(Freq=sum(Freq)) %>% 
#    arrange(desc(Freq), pred) #%>%
#    # mutate(tmp=which(duplicated(pred))) %>% 
#    # arrange(desc(Freq), pred, tmp)
# tbl_comp_a
# tbl_comp_a[c(which(duplicated(tbl_comp_a$pred)&(Freq=4)), which(duplicated(tbl_comp_a$pred)&(Freq=4))-1),] %>% 
#    mutate(tmp=T) %>% 
#    rbind(mutate(tbl_comp_a[-c(which(duplicated(tbl_comp_a$pred)), which(duplicated(tbl_comp_a$pred))-1),],
#                 tmp=F)) %>% 
#    arrange(desc(Freq), desc(tmp), pred) %>% View


for(i in seq_along(years)){
   
   csv_name <- csv_names[i]
   csv_name
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
   data_all <- no2_e_09_11
   #f# subset cross-validation data (5-fold cross-validation)
   #f# stratified by station types, climate zones and/or years
   set.seed(seed)
   data_all$index <- 1:nrow(data_all)
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
   train_sub <- data_all[folds$index[[1]], ]
   test_sub <- data_all[folds$indexOut[[1]], ]
   source("scr/fun_call_predictor.R")
   #f# SLR: define/preprocess predictors (direction of effect)
   source("scr/fun_slr_proc_in_data.R")
   train_sub <- proc_in_data(train_sub, neg_pred)
   test_sub <- proc_in_data(test_sub, neg_pred)
   
   eq_lme <- as.formula(paste0('obs~',
                               paste(lme_result[[i]]$variables[-1], collapse = "+"),
                               "+ (1|station_european_code) + (1|year)"))
   lme_model <- lmer(eq_lme, train_sub)
   
   eq_slr <- as.formula(paste0('obs~',
                               paste(slr_result[[i]]$variables[-1], collapse = "+")))
   slr_model <- lm(eq_slr, train_sub)
   
   data_all2 <- rbind(train_sub %>% mutate(df_type="train"),
                      test_sub %>% mutate(df_type='test'))
   
   
   # the prediction will use the unconditional (population-level) values for data with previously unobserved levels (or NAs).
   source("scr/fun_gen_pred_lme.R")
   lme_prediction <- gen_pred_lme(lme_model, data_all2, "obs")
   source("scr/fun_gen_pred_df.R")
   slr_prediction <- gen_pred_df(slr_model, data_all2, "obs")
   pred_all <- cbind(lme=lme_prediction$lme, dplyr::select(slr_prediction, -res),
                     period=model_scenario[i])
   write.csv(pred_all, paste0("data/workingData/lme_slr_", model_scenario[i], '.csv'),
             row.names = F)
   # plot(slr_prediction$slr, lme_prediction$lme)
   # 
   # plot(with(slr_prediction, slr[df_type=='test']), with(lme_prediction, lme[df_type=='test']))
   # plot(with(slr_prediction, slr[df_type=='train']), with(lme_prediction, lme[df_type=='train']))
   # plot(with(lme_prediction, obs[df_type=='train']), with(lme_prediction, lme[df_type=='train']))
   # plot(lme_prediction$lme, lme_prediction$obs)
   # plot(with(lme_prediction, lme[df_type=='test']), with(lme_prediction, obs[df_type=='test']))
   # plot(with(slr_prediction, slr[df_type=='test']), with(slr_prediction, obs[df_type=='test']))
   print(paste0("lme: test data ", substr(csv_names[[i]], 12, nchar(csv_names[[i]]))))
   error_matrix(with(lme_prediction, obs[df_type=='test']), with(lme_prediction, lme[df_type=='test']))[c(1,5,7)] %>% print()
   print(paste0("slr: test data ", substr(csv_names[[i]], 12, nchar(csv_names[[i]]))))
   error_matrix(with(slr_prediction, obs[df_type=='test']), with(slr_prediction, slr[df_type=='test']))[c(1,5,7)] %>% print()
   # error_matrix(with(lme_prediction, obs[df_type=='train']), with(lme_prediction, lme[df_type=='train']))
   # error_matrix(with(slr_prediction, obs[df_type=='train']), with(slr_prediction, slr[df_type=='train']))
   
   # add year as a prediction
   eq_lme <- as.formula(paste0('obs~',
                               paste(lme_result[[i]]$variables[-1], collapse = "+"),
                               " + year + (1|station_european_code) + (1|year)"))
   lme_model <- lmer(eq_lme, train_sub)
   
   eq_slr <- as.formula(paste0('obs~',
                               paste(slr_result[[i]]$variables[-1], collapse = "+"),
                               " + year"))
   slr_model <- lm(eq_slr, train_sub)
   source("scr/fun_gen_pred_lme.R")
   lme_prediction <- gen_pred_lme(lme_model, data_all2, "obs")
   source("scr/fun_gen_pred_df.R")
   slr_prediction <- gen_pred_df(slr_model, data_all2, "obs")
   # plot(slr_prediction$slr, lme_prediction$lme)
   # 
   # plot(with(slr_prediction, slr[df_type=='test']), with(lme_prediction, lme[df_type=='test']))
   # plot(with(slr_prediction, slr[df_type=='train']), with(lme_prediction, lme[df_type=='train']))
   # plot(with(lme_prediction, obs[df_type=='train']), with(lme_prediction, lme[df_type=='train']))
   # plot(lme_prediction$lme, lme_prediction$obs)
   # # test data
   # plot(with(lme_prediction, lme[df_type=='test']), with(lme_prediction, obs[df_type=='test']))
   # plot(with(slr_prediction, slr[df_type=='test']), with(slr_prediction, obs[df_type=='test']))
   print(paste0("lme: test data ", substr(csv_names[[i]], 12, nchar(csv_names[[i]]))))
   error_matrix(with(lme_prediction, obs[df_type=='test']), with(lme_prediction, lme[df_type=='test']))[c(1,5,7)] %>% print()
   print(paste0("slr: test data ", substr(csv_names[[i]], 12, nchar(csv_names[[i]]))))
   error_matrix(with(slr_prediction, obs[df_type=='test']), with(slr_prediction, slr[df_type=='test']))[c(1,5,7)] %>% print()
   # error_matrix(with(lme_prediction, obs[df_type=='train']), with(lme_prediction, lme[df_type=='train']))
   # error_matrix(with(slr_prediction, obs[df_type=='train']), with(slr_prediction, slr[df_type=='train']))
   
}

# output_multipleyear <- function(i){
#    csv_name <- csv_names[i]
#    csv_name
#    no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
#    data_all <- no2_e_09_11
#    #f# subset cross-validation data (5-fold cross-validation)
#    #f# stratified by station types, climate zones and/or years
#    set.seed(seed)
#    data_all$index <- 1:nrow(data_all)
#    # Test only leave location out first 
#    # Method 1: (easier to use)
#    folds=CreateSpacetimeFolds(
#       data_all,
#       spacevar = "station_european_code",     # leave location out
#       timevar = NA,
#       k = 5,
#       class = NA,
#       seed = seed
#    )
#    train_sub <- data_all[folds$index[[1]], ]
#    test_sub <- data_all[folds$indexOut[[1]], ]
#    source("scr/fun_call_predictor.R")
#    #f# SLR: define/preprocess predictors (direction of effect)
#    source("scr/fun_slr_proc_in_data.R")
#    train_sub <- proc_in_data(train_sub, neg_pred)
#    test_sub <- proc_in_data(test_sub, neg_pred)
#    
#    eq_lme <- as.formula(paste0('obs~',
#                                paste(lme_result[[i]]$variables[-1], collapse = "+"),
#                                "+ (1|station_european_code) + (1|year)"))
#    lme_model <- lmer(eq_lme, train_sub)
#    
#    eq_slr <- as.formula(paste0('obs~',
#                                paste(slr_result[[i]]$variables[-1], collapse = "+")))
#    slr_model <- lm(eq_slr, train_sub)
#    
#    data_all2 <- rbind(train_sub %>% mutate(df_type="train"),
#                       test_sub %>% mutate(df_type='test'))
#    
#    
#    # the prediction will use the unconditional (population-level) values for data with previously unobserved levels (or NAs).
#    source("scr/fun_gen_pred_lme.R")
#    lme_prediction <- gen_pred_lme(lme_model, data_all2, "obs")
#    source("scr/fun_gen_pred_df.R")
#    slr_prediction <- gen_pred_df(slr_model, data_all2, "obs")
#    pred_all <- cbind(period=model_scenario[i],
#                      lme=lme_prediction$lme, dplyr::select(slr_prediction, -res))
#    pred_all
# }
# pred_all <- lapply(seq_along(csv_names), output_multipleyear)
#----------Compare stability------------
periods <- list.files("data/workingData", "lme_slr") %>% substr(., 9, 13)
pred_all <- lapply(paste0('data/workingData/', 
                          list.files("data/workingData", "lme_slr")), read.csv)

View(pred_all[[2]])
pred_all_clean <- lapply(pred_all, function(pred_df){
   pred_df <- pred_df %>% filter(year%in%(2009:2010)) %>% dplyr::select(slr, lme, year, df_type, period, obs)  #obs, period,
   names(pred_df)[1] <- paste0(names(pred_df)[1], '_', unique(pred_df$period))
   names(pred_df)[2] <- paste0(names(pred_df)[2], '_', unique(pred_df$period))
   pred_df
   # pred_df %>% select(-)
})
pred_all_clean <- do.call(cbind, pred_all_clean)
pred_all_clean <- pred_all_clean[, !duplicated(names(pred_all_clean))]
names(pred_all_clean)
plot_df <- pred_all_clean %>% 
   filter(df_type=='test', year==2010) %>% 
   dplyr::select(-year, -df_type, -period)
plot_df <- plot_df[,names(plot_df) %>% order]
pairs(plot_df, upper.panel=NULL)

ggplot(plot_df)+
   geom_point(aes(x=lme, y=obs, color=period))+
   facet_grid(year~.)

scatterplot()
# set_scenario <- function(i, df_pred_list, model_name, model_sets){
#    df_pred_list[[i]] %>% mutate(model=model_name, period=model_sets[i])
#    
# }
# lme_result <- lapply(seq_along(lme_result), set_scenario, lme_result, "lme", model_scenario)
# slr_result <- lapply(seq_along(slr_result), set_scenario, lme_result, "slr", model_scenario)
