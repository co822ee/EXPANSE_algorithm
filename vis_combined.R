met_str <- 'precip|temp|wind|pressure'
library(gtools)
source('scr/fun_call_lib.R')
####### Functions #######
read_perfm <- function(poll){
   if(poll=='PM2.5'){
      predictor <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', 
                                tolower('pm25'),'All.csv'))
   }else{
      predictor <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', 
                                tolower(poll),'All.csv'))
   }
   
   predictor <- predictor[!duplicated(predictor$sta_code), ] %>% 
      dplyr::select(zoneID, sta_code) %>% 
      inner_join(
         read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
         .,
         by='zoneID')
   
   csv_names <- gsub('SLR_result_all_', '', 
                     list.files('data/workingData/', 
                                paste0('SLR_result_all_o3_', poll))) %>% 
      strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
   # years <- csv_names %>% substr(., nchar(csv_names)-3, nchar(csv_names)) %>% 
   #    as.numeric() %>% as.list()
   all_test <- lapply(paste0("data/workingData/5cv_", csv_names, ".csv"), read.csv)
   
   all_test <- lapply(all_test, function(df_all){
      df_all$poll <- poll
      df_all$period <- df_all$year
      df_all %>% inner_join(., predictor, by='sta_code')
   })
   all_test
}
read_perfm2 <- function(poll){
   if(poll=='PM2.5'){
      predictor <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', 
                                tolower('pm25'),'All.csv'))
   }else{
      predictor <- fread(paste0('../EXPANSE_predictor/data/raw/gee/pred_', 
                                tolower(poll),'All.csv'))
   }
   predictor <- predictor[!duplicated(predictor$sta_code), ] %>% 
      dplyr::select(zoneID, sta_code) %>% 
      inner_join(
         read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
         .,
         by='zoneID')
   
   csv_names2 <- gsub('SLR_result_all_', '', 
                      list.files('../expanse_multiyear/data/workingData/', 
                                 paste0('SLR_result_all_o3_', poll))) %>% 
      strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
   # # nfold=4 PM2.5 from year 2000-2003 (00-19) there is NA
   all_test2 <- lapply(paste0("../expanse_multiyear/data/workingData/5cv_", csv_names2, ".csv"), 
                       read.csv)
   
   all_test2 <- lapply(seq_along(all_test2), function(i){
      df_all <- all_test2[[i]]
      df_all$poll <- poll
      df_all$period <- strsplit(csv_names2, '_')[[i]][3]
      df_all %>% inner_join(., predictor, by='sta_code')
      # if(poll=='PM2.5'){
      #    # df_all[!is.na(df_all$gtwr), ]
      #    # We just leave out the gtwr 5-CV for 2000-2003 because the GTWR for fold4 for 2000-2003 could not be built
      #    # Also the numbers of observations for 2000-2003 are 11,22,52,82...
      #    df_all
      # }else{
      #    df_all
      # }
   })
   
   all_test2
}
show_EM <- function(all_df_i, all_test, climate_included=F){
   
   df_all <- lapply(sort(unique(all_test[[all_df_i]]$year)), 
                    function(target_yr){
                       all_test_sub <- all_test[[all_df_i]][all_test[[all_df_i]]$year==target_yr,]
                       slr=all_test_sub$slr
                       gwr=all_test_sub$gwr
                       gtwr=all_test_sub$gtwr
                       
                       rf=all_test_sub$rf
                       obs=all_test_sub$obs
                       df_all <- data.frame(slr=error_matrix(obs, slr),
                                            gwr=error_matrix(obs, gwr),
                                            gtwr=error_matrix(obs, gtwr),
                                            rf=error_matrix(obs, rf),
                                            period=unique(all_test[[all_df_i]]$period),
                                            year=target_yr,
                                            poll=unique(all_test[[all_df_i]]$poll))[c(1,5,7),]
                       
                       df_all$EM = row.names(df_all)
                       if(climate_included){
                          df_all$climate_zone <- unique(all_test_sub$climate_zone)
                          df_all$climate_name <- paste0(df_all$climate_zone, '_', nrow(all_test_sub))
                          
                       }
                       df_all
                    }) %>% do.call(rbind, .)
   
   
}
comp_rf_slr <- function(csv_i, poll){
   files <- list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   slr_name <- files[csv_i]
   csv_name <- gsub('.csv', '', substr(slr_name, 19, nchar(slr_name)))
   yr <- strsplit(slr_name, "_")[[1]][6]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("data/workingData/", slr_name), header=T)
   slr_result$yr <- yr
   slr_result$nfold <- nfold
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   p1 <- ggplot(slr_result)+
      geom_col(aes(x=reorder(variables, incredR2), y=incredR2), 
               position = "dodge2", fill='khaki')+
      coord_flip() +
      theme_light()+
      labs(x = 'variable', y = 'increased R2',
           title = paste0(yr, "_fold_", nfold))+
      # facet_grid(EM~., scales ='free')+
      # labs(title=years[[i]])+
      theme(axis.title = element_text(size = 11),
            axis.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 11),
            strip.text.y = element_text(size = 11))
   # rf_vi <- read.csv(paste0("data/workingData/", rf_name))
   source("../EXPANSE_algorithm/scr/fun_plot_rf_vi.R")
   p2 <- plot_rf_vi(csv_name, var_no = 20)
   grid.arrange(p1, p2, nrow=1, ncol=2)
}

extract_slr_var <- function(csv_i, poll){
   files <- list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   slr_name <- files[csv_i]
   
   yr <- strsplit(slr_name, "_")[[1]][6]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("data/workingData/", slr_name), header=T)
   slr_result$yr <- yr
   slr_result$nfold <- nfold
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   slr_var <- slr_result$variables[-1]
   if(any(grepl(met_str, slr_var))){
      slr_var[grepl(met_str, slr_var)] <- gsub('(\\_\\d+).*', '', slr_var[grepl(met_str, slr_var)])  
   }
   data.frame(var_name=slr_var, yr=yr, nfold=nfold)
}
extract_rf_var <- function(csv_i, poll){
   files <- list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   slr_name <- files[csv_i]
   csv_name <- gsub('.csv', '', substr(slr_name, 19, nchar(slr_name)))
   yr <- strsplit(slr_name, "_")[[1]][6]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   
   var_importance <- read.csv(paste0('data/workingData/RF_vi_', csv_name, '.csv'), 
                              header = T)
   rf_var10 <- (var_importance %>% top_n(10, vi))$var_name
   
   if(any(grepl(met_str, rf_var10))){
      rf_var10[grepl(met_str, rf_var10)] <- gsub('(\\_\\d+).*', '', rf_var10[grepl(met_str, rf_var10)])  
   }
   data.frame(var_name=rf_var10, yr=yr, nfold=nfold)
}
extract_slr_var_multi <- function(csv_i, poll){
   files <- list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   slr_name <- files[csv_i]
   
   yr <- strsplit(slr_name, "_")[[1]][6]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("../expanse_multiyear/data/workingData/", slr_name), header=T)
   slr_result$yr <- yr
   slr_result$nfold <- nfold
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   slr_var <- slr_result$variables[-1]
   if(any(grepl(met_str, slr_var))){
      slr_var[grepl(met_str, slr_var)] <- gsub('(\\_\\d+).*', '', slr_var[grepl(met_str, slr_var)])  
   }
   data.frame(var_name=slr_var, yr=yr, nfold=nfold)
}
extract_rf_var_multi <- function(csv_i, poll){
   files <- list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   slr_name <- files[csv_i]
   csv_name <- gsub('.csv', '', substr(slr_name, 19, nchar(slr_name)))
   yr <- strsplit(slr_name, "_")[[1]][6]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   
   var_importance <- read.csv(paste0('../expanse_multiyear/data/workingData/RF_vi_', csv_name, '.csv'), 
                              header = T)
   rf_var10 <- (var_importance %>% top_n(10, vi))$var_name
   
   if(any(grepl(met_str, rf_var10))){
      rf_var10[grepl(met_str, rf_var10)] <- gsub('(\\_\\d+).*', '', rf_var10[grepl(met_str, rf_var10)])  
   }
   data.frame(var_name=rf_var10, yr=yr, nfold=nfold)
}
create_heatmap_slr <- function(poll, only0019=T){
   files <- list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   slr_vars1 <- lapply(seq_along(files), extract_slr_var, poll=poll) %>% do.call(rbind, .)
   # slr_tbl1 <- with(slr_vars, table(var_name, yr))
   # slr_tbl1 <- slr_tbl1[mixedorder(row.names(slr_tbl1), decreasing=T),]
   
   # Multiple-year
   files2 <- list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   slr_vars2 <- lapply(seq_along(files2), extract_slr_var_multi, poll=poll) %>% do.call(rbind, .)
   
   slr_vars <- rbind(slr_vars1, slr_vars2)
   if(only0019==T){
      slr_vars <- slr_vars %>% filter(!yr%in%c('08-10', '09-11', '10-12', 
                                               '08-12', '06-12', '12-19'))
   }
   slr_tbl <- with(slr_vars, table(var_name, yr))
   slr_tbl <- slr_tbl[mixedorder(row.names(slr_tbl), decreasing=T),]
   
   slr_tbl <- as.data.frame(slr_tbl) %>% group_by(var_name)%>%  mutate(sum=sum(Freq))
   ggplot(slr_tbl,aes(x=yr,y=reorder(var_name, sum),fill=as.factor(Freq)))+
      geom_tile()+
      labs(x='year', y='', fill='count', title=paste0('SLR: ', poll))+
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 13),
            axis.text.x = element_text(angle = 90),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            strip.text.y = element_text(size = 15))+
      scale_fill_brewer(palette = 'YlOrRd')
   
}
create_heatmap_rf <- function(poll_i, only0019=T){
   poll=target_poll[poll_i]
   # Single-year
   files <- list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   rf_vars1 <- lapply(seq_along(files), extract_rf_var, poll=poll) %>% do.call(rbind, .)
   # rf_tbl <- with(rf_vars, table(var_name, yr))
   # rf_tbl <- rf_tbl[mixedorder(row.names(rf_tbl), decreasing=T),]
   
   # Multiple-year
   files2 <- list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_o3_", poll))[grepl("fold", list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_o3_", poll)))]
   rf_vars2 <- lapply(seq_along(files2), extract_rf_var_multi, poll=poll) %>% do.call(rbind, .)
   
   rf_vars <- rbind(rf_vars1, rf_vars2)
   if(only0019==T){
      rf_vars <- rf_vars %>% filter(!yr%in%c('08-10', '09-11', '10-12', 
                                             '08-12', '06-12', '12-19'))
   }
   rf_tbl <- with(rf_vars, table(var_name, yr))
   rf_tbl <- rf_tbl[mixedorder(row.names(rf_tbl), decreasing=T),]
   
   
   
   # heatmap(rf_tbl, Colv = NA, Rowv = NA, scale="column", main=paste0('RF: ', poll))
   rf_tbl <- as.data.frame(rf_tbl) %>% group_by(var_name)%>%  mutate(sum=sum(Freq))
   
   ggplot(rf_tbl,aes(x=yr,y=reorder(var_name, sum),fill=as.factor(Freq)))+
      geom_tile()+
      labs(x='year', y='', fill='count', title=paste0('RF: ', poll))+
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 13),
            axis.text.x = element_text(angle = 90),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            strip.text.y = element_text(size = 15))+
      scale_fill_brewer(palette = 'YlOrRd')
}

#--------- Main ----------
source("scr/fun_call_lib.R")
target_poll = c('NO2', 'O3', 'PM10', 'PM2.5')
years <- 2000:2019
calc_perfm <- function(all_test){
   em_df_l <- lapply(seq_along(all_test), show_EM, all_test=all_test)
   do.call(rbind, em_df_l)
}
all_test_l <- lapply(target_poll, read_perfm)
em_df <- lapply(all_test_l, calc_perfm)
em_df <- do.call(rbind, em_df)

all_test_l2 <- lapply(target_poll, read_perfm2)
em_df2 <- lapply(all_test_l2, calc_perfm)
em_df2 <- do.call(rbind, em_df2)


em_df_all <- rbind(em_df, em_df2)
dat_r2 <- em_df_all %>% filter(EM=='rsq', !period%in%c('08-10', '09-11', '10-12', 
                                                       '08-12', '06-12', '12-19'),
                               year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period'))
dat_r2 <- dat_r2[!is.na(dat_r2$values), ] %>% 
   mutate(model=ifelse(period=='00-19'&model!='gtwr', paste0(model, period), model))
          
ggplot(dat_r2, 
       aes(x=reorder(year, year), y=values, fill=model))+
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="R squared", x='year')+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   facet_wrap(poll~.)+
   coord_flip()+
   guides(fill = guide_legend(reverse = TRUE))
# geom_hline(aes(yintercept=0.587))+  # Reverse the legend orders
# geom_hline(aes(yintercept=0.664))
ggsave('graph/R2.tiff', width=8, height=10, units='in', dpi=200)


dat_rmse <- em_df_all %>% filter(EM=='RMSE', !period%in%c('08-10', '09-11', '10-12', 
                                                       '08-12', '06-12', '12-19'),
                               year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period'))
dat_rmse <- dat_rmse[!is.na(dat_rmse$values), ] %>% 
   mutate(model=ifelse(period=='00-19'&model!='gtwr', paste0(model, period), model))

ggplot(dat_rmse, 
       aes(x=reorder(year, year), y=values, fill=model))+
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="RMSE", x='year')+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   facet_wrap(poll~.)+
   coord_flip()+
   guides(fill = guide_legend(reverse = TRUE))
# geom_hline(aes(yintercept=3.3))+
# geom_hline(aes(yintercept=2.97))
ggsave('graph/RMSE.tiff', width=8, height=10, units='in', dpi=200)

# Multiple-year only
dat_r2 <- em_df_all %>% filter(EM=='rsq', !period%in%(2000:2019),
                               year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period'))
dat_r2 <- dat_r2[!is.na(dat_r2$values), ] %>% 
   mutate(model2=paste0(model, period))

ggplot(dat_r2, 
       aes(x=reorder(model, model), y=values, fill=period))+
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="R squared", x='time')+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   facet_grid(poll~year)+
   coord_flip()+
   guides(fill = guide_legend(reverse = TRUE))
# geom_hline(aes(yintercept=0.587))+  # Reverse the legend orders
# geom_hline(aes(yintercept=0.664))
ggsave('graph/R2_multiyrOnly.tiff', 
       width=11, height=8, units='in', dpi=200)

#--------- Main (climate zone) ----------
source("scr/fun_call_lib.R")
target_poll = c('NO2', 'O3', 'PM10', 'PM2.5')
years <- 2000:2019
calc_perfm2 <- function(all_test){
   lapply(all_test, function(all_test_period){
      all_test_period_climate <- lapply(unique(all_test_period$climate_zone), function(climate_id){
         all_test_period %>% filter(climate_zone==climate_id)
      })
      em_df_l <- lapply(seq_along(all_test_period_climate), show_EM, 
                        all_test=all_test_period_climate, 
                        climate_included=T)
      do.call(rbind, em_df_l)
   }) %>% do.call(rbind, .)
}
em_df <- lapply(all_test_l, calc_perfm2)
em_df <- do.call(rbind, em_df)

all_test_l2 <- lapply(target_poll, read_perfm2)
em_df2 <- lapply(all_test_l2, calc_perfm2)
em_df2 <- do.call(rbind, em_df2)


em_df_all <- rbind(em_df, em_df2)
dat_r2 <- em_df_all %>% filter(EM=='rsq', !period%in%c('08-10', '09-11', '10-12', 
                                                       '08-12', '06-12', '12-19'),
                               year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period', 'climate_zone', 'climate_name'))
dat_r2 <- dat_r2[!is.na(dat_r2$values), ] %>% 
   mutate(model=ifelse(period=='00-19', paste0(model, period), model))

ggplot(dat_r2, 
       aes(x=reorder(year, year), y=values, fill=model))+
   geom_bar(stat="identity", position = "dodge2")+
   facet_grid(poll~climate_zone, scales ='fixed')+
   # labs(title=years[[i]])+
   labs(y="R squared", x='year')+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   # facet_wrap(poll~.)+
   coord_flip()+
   guides(fill = guide_legend(reverse = TRUE))
# geom_hline(aes(yintercept=0.587))+  # Reverse the legend orders
# geom_hline(aes(yintercept=0.664))
ggsave('graph/R2_cliamteZone.tiff', width=12, height=10, units='in', dpi=200)


dat_rmse <- em_df_all %>% filter(EM=='RMSE', !period%in%c('08-10', '09-11', '10-12', 
                                                          '08-12', '06-12', '12-19'),
                                 year%in%c(2000, 2005, 2010, 2015, 2019)) %>%  
   gather("model", "values", -c("year", "EM", 'poll', 'period', 'climate_zone', 'climate_name'))
dat_rmse <- dat_rmse[!is.na(dat_rmse$values), ] %>% 
   mutate(model=ifelse(period=='00-19', paste0(model, period), model))

ggplot(dat_rmse, 
       aes(x=reorder(year, year), y=values, fill=model))+
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="RMSE", x='year')+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   facet_grid(poll~climate_zone, scales ='fixed')+
   coord_flip()+
   guides(fill = guide_legend(reverse = TRUE))
# geom_hline(aes(yintercept=3.3))+
# geom_hline(aes(yintercept=2.97))
ggsave('graph/RMSE_cliamteZone.tiff', width=12, height=10, units='in', dpi=200)

#-------- heatmap -----------
slr_l <- lapply(target_poll, create_heatmap_slr)

png('graph/heatmap_slr.png', 
    width = 13, height = 15, units='in', res=200)
do.call(grid.arrange, slr_l)
dev.off()

rf_l <- lapply(seq_along(target_poll), create_heatmap_rf)

png('graph/heatmap_rf.png', 
    width = 13, height = 15, units='in', res=200)
do.call(grid.arrange, rf_l)
dev.off()

#### Final models coefficient values ####
extract_slr_coef <- function(csv_i, poll){
   files <- list.files("data/workingData/", paste0("SLR_summary_model_all_", poll))
   slr_name <- files[csv_i]
   
   yr <- gsub('.csv', '', strsplit(slr_name, "_")[[1]][6])
   
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("data/workingData/", slr_name), header=T)
   slr_result$yr <- yr
   
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   if(any(grepl(met_str, slr_var))){
      slr_var[grepl(met_str, slr_var)] <- gsub('(\\_\\d+).*', '', slr_var[grepl(met_str, slr_var)])  
   }
   slr_result
}
extract_slr_coef_multi <- function(csv_i, poll){
   files <- list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_all_", poll))
   slr_name <- files[csv_i]
   
   yr <- gsub('.csv', '', strsplit(slr_name, "_")[[1]][6])
   
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("../expanse_multiyear/data/workingData/", slr_name), header=T)
   slr_result$yr <- yr
   
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   if(any(grepl(met_str, slr_var))){
      slr_var[grepl(met_str, slr_var)] <- gsub('(\\_\\d+).*', '', slr_var[grepl(met_str, slr_var)])  
   }
   slr_result
}
create_heatmap_slr_all <- function(poll, only0019=T){
   files <- list.files("data/workingData/", paste0("SLR_summary_model_all_", poll))
   files2 <- list.files("../expanse_multiyear/data/workingData/", paste0("SLR_summary_model_all_", poll))
   slr_vars1 <- lapply(seq_along(files), extract_slr_coef, poll=poll) %>% do.call(rbind, .)
   slr_vars2 <- lapply(seq_along(files2), extract_slr_coef_multi, poll=poll) %>% do.call(rbind, .)
   slr_vars <- rbind(slr_vars1, slr_vars2)
   if(only0019==T){
      slr_vars <- slr_vars %>% filter(!yr%in%c('08-10', '09-11', '10-12', 
                                               '08-12', '06-12', '12-19'))
   }
   slr_vars <- slr_vars %>% filter(variables!='Final')
   slr_tbl <- with(slr_vars, table(variables, yr))
   slr_tbl <- slr_tbl[mixedorder(row.names(slr_tbl), decreasing=T),]
   
   slr_tbl <- as.data.frame(slr_tbl) %>% group_by(variables)%>%  mutate(sum=sum(Freq))
   ggplot(slr_tbl,aes(x=yr,y=reorder(variables, sum),fill=as.factor(Freq)))+   ##
      geom_tile()+
      labs(x='year', y='', fill='count', title=paste0('SLR: ', poll))+
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 13),
            axis.text.x = element_text(angle = 90),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            strip.text.y = element_text(size = 15))+
      scale_fill_brewer(palette = 'YlOrRd')
}

slr_l <- lapply(target_poll, create_heatmap_slr_all)

png('graph/heatmap_slr_all.png', 
    width = 12, height = 10.5, units='in', res=200)
do.call(grid.arrange, slr_l)
dev.off()

