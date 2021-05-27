source('scr/fun_call_lib.R')
# elapse_no2 <- read.csv("../test_EXPANSE_NO2/data/rawData/AIRBASE_NO2_2010_variables.csv",
#                        encoding = "utf-8")
# https://code.earthengine.google.com/ea63b894df693a2d111ad32d430913c6?noload=1
elapse_no2 <- read.csv("~/Downloads/predictorsOnGEE.csv")

#f# SLR: select predictors
# pred_c <- c(c('alt_t', 'pop2011'),  #'x_trun', 'y_trun',
#             'clc10',
#             'clc14',
#             'clc3',
#             'clc5',
#             'clc7',
#             'MAJRDS_EU',
#             'ROADS_EU',
#             c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT', 'DEHM_NO2_2010'),
#             'RES') #year
neg_pred <- c('alt_t', 'clc14', 'clc7')
y_var <- "NO2_2010"
# station_info <- c(  'PREDNO2LURFULL',
#                     'system:index',
#                     'country_is',
#                     'XY',
#                     'REGION',
#                     'Station',
#                     'strata_run5',
#                     'airid',
#                     'type_of_st',
#                     'Xcoord', 'Ycoord',
#                     y_var)
station_info <- c(
                    'system.index',
                    'Station',
                    'strata_run5',
                    '.geo',
                    y_var)
#f# SLR: define/preprocess predictors (direction of effect)
data_all <- elapse_no2 %>% as_tibble() %>% 
   mutate(across(matches(neg_pred), function(x) -x )) %>% as.data.frame()

# TO DO: Truncated altitude;  
source("scr/fun_slr_for.R")

# check the predictor variables
print("SLR predictors:")
x_var <- data_all %>% dplyr::select(-matches(station_info)) %>% names()
x_var
POLL <- data_all[, y_var]
pred <- data_all %>% dplyr::select(x_var) %>% as.data.frame()

# slr_model <- finalBestModel

slr_result <- slr(data_all[, y_var], data_all %>% dplyr::select(x_var) %>% as.data.frame(),
                  cv_n = "2010_all")
read.csv("data/workingData/SLR_summary_model_2010_all.csv", header=T)
slr_model <- slr_result[[3]]
slr_model %>% summary()

# Check VIF
vif(slr_model)
source("scr/fun_gen_pred_df.R")
slr_poll <- gen_pred_df(slr_model, data_all, y_var)
# https://code.earthengine.google.com/720ef0eed3eab3d1e886a0a5f2dc38e8?noload=1
slr_gee <- read.csv("~/Downloads/implementSLR.csv")

comp <- inner_join(slr_poll, slr_gee, by="Station")
plot(comp$slr_no2, comp$slr, xlab='GEE', ylab='Rstudio')
plot(comp$slr_no2, comp$PREDNO2LURFULL, xlab='GEE', ylab='Elapse')
plot(comp$slr, comp$PREDNO2LURFULL, xlab='Rstudio', ylab='Elapse')

write.csv(slr_poll, 
          paste0('data/workingData/SLR_result_all_', "2010_all", '.csv'), 
          row.names = F)

# Fit variogram (universal kriging)
res_vario <- variogram(as.formula(paste0(y_var,"~", paste(names(slr_model$coefficients[-1]), collapse = "+"))), 
                       data=data_all, locations=~Xcoord+Ycoord)
plot(res_vario,  fit.variogram(res_vario, model=vgm(NA, "Sph", NA, NA)))
plot(res_vario,  fit.variogram(res_vario, model=vgm(NA, "Exp", NA, NA)))
fit.variogram(res_vario, model=vgm(NA, "Sph", NA, NA))
as.formula(paste0(y_var,"~", paste(names(slr_model$coefficients[-1]), collapse = "+")))


#-----
# only elapse and new data (only road is new) 
road <- read.csv("../EXPANSE_predictor/data/processed/road_merged.csv")
road1 <- road %>% rename(Station=sta_code) %>% select(Station, matches("Roads"))
df2 <- inner_join(data_all, road1, by="Station")
x_var <- x_var[!grepl("ROADS|MAJRDS", x_var)]
x_var <- c(x_var, road %>% select(matches("Roads")) %>% names())
slr_result <- slr(df2[, y_var], df2 %>% dplyr::select(x_var) %>% as.data.frame(),
                  cv_n = "2010_all_newRoad_")
slr_model <- slr_result[[3]]
slr_model %>% summary()
source("scr/fun_gen_pred_df.R")
slr_poll <- gen_pred_df(slr_model, df2, y_var)
write.csv(slr_poll, 
          paste0('data/workingData/SLR_result_all_', "2010_all_newRoad_", '.csv'), 
          row.names = F)

# --- compare prediction values
elapse <- read.csv(paste0('data/workingData/SLR_result_all_', "2010_all", '.csv')) %>% 
   select(slr, obs, Station, Xcoord, Ycoord) %>% 
   rename(elapse=slr)
expanse <- read.csv(paste0('data/workingData/SLR_result_all_', "2010_all_newRoad_", '.csv')) %>% 
   select(slr, Station) %>% 
   rename(expanse=slr)
alldf <- inner_join(elapse, expanse, by="Station")
ggplot(alldf, aes(x=elapse, y=expanse))+
   geom_point()+
   geom_abline(intercept = 0, slope=1)
alldf <- alldf %>% mutate(diff=expanse-elapse, diff_ex=obs-expanse)

ggplot(alldf, aes(x=diff, y=diff_ex))+
   geom_point()+
   geom_abline(intercept = 0, slope=0)+
   geom_vline(xintercept=0)+
   labs(x='expanse-elapse', y='obs-expanse')
# geom_abline(intercept = 0, slope=1)
#expanse better
sum((alldf$diff*alldf$diff_ex)>=0)/nrow(alldf)
#elapse better
sum((alldf$diff*alldf$diff_ex)<=0)/nrow(alldf)

#--------
# only elapse and new data
source("scr/o_00_00_read_data.R")
new_data <- no2_e_all %>% filter(year==2010)
df2 <- data_all %>% filter(Station%in%new_data$sta_code)
slr_result <- slr(df2[, y_var], df2 %>% dplyr::select(x_var) %>% as.data.frame(),
                  cv_n = "2010_all_new_")
slr_model <- slr_result[[3]]
slr_model %>% summary()
source("scr/fun_gen_pred_df.R")
slr_poll <- gen_pred_df(slr_model, df2, y_var)
write.csv(slr_poll, 
          paste0('data/workingData/SLR_result_all_', "2010_all_new_", '.csv'), 
          row.names = F)

# --- compare prediction values
elapse <- read.csv(paste0('data/workingData/SLR_result_all_', "2010_all_new_", '.csv')) %>% 
   select(slr, obs, Station, Xcoord, Ycoord) %>% 
   rename(elapse=slr)
expanse <- read.csv(paste0('data/workingData/SLR_result_all_', "2010_all_o_elapse", '.csv')) %>% 
   select(slr, sta_code) %>% 
   rename(expanse=slr, Station=sta_code)
alldf <- inner_join(elapse, expanse, by="Station")
ggplot(alldf, aes(x=elapse, y=expanse))+
   geom_point()+
   geom_abline(intercept = 0, slope=1)
alldf <- alldf %>% mutate(diff=expanse-elapse, diff_ex=obs-expanse)

ggplot(alldf, aes(x=diff, y=diff_ex))+
   geom_point()+
   geom_abline(intercept = 0, slope=0)+
   geom_vline(xintercept=0)+
   labs(x='expanse-elapse', y='obs-expanse')
   # geom_abline(intercept = 0, slope=1)
#expanse better
sum((alldf$diff*alldf$diff_ex)>=0)#/nrow(alldf)
#elapse better
sum((alldf$diff*alldf$diff_ex)<=0)#/nrow(alldf)
