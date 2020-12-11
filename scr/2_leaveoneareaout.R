library(dplyr)
EU_data <- read.csv('data/NO2_2010.csv') %>% na.omit()
pred_c <- c(c('alt10_enh','Xcoord', 'Ycoord', 'XY'),
            'clc10',
            'clc14',
            'clc3',
            'clc5',
            'clc7',
            'MAJRDS_EU',
            'ROADS_EU',
            c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT'),
            'RES')
neg_pred <- c('alt10_enh', 'clc14', 'clc7')
station_info <- c('airid', 'Year', 'country_is', 'type_of_st', 'Xcoord', 'Ycoord', 
                  'REGION', 'strata_run5', 'country_full')
EU_data <- EU_data %>% mutate_at(EU_data %>% dplyr::select(matches(neg_pred)) %>% names(), 
                                 function(x) -x)

slr <- read.csv("data/SLR_summary_model.csv", sep='\t',dec = "," )
slr
EU_data %>% names
unique(EU_data$country_is)
# Train data using cntr
if(!dir.exists("data/workingData")) dir.create("data/workingData")
source("../model_test/R/fun_slr.R")
r2_df <- data.frame(cntr="all", r2=as.numeric(slr[nrow(slr),]$increR2), n=nrow(EU_data))
cntr_code <- "NL"
leave_one_cntr <- function(cntr_code){
   train <- EU_data %>% filter(country_is != cntr_code)
   test <- EU_data %>% filter(country_is == cntr_code)
   slr_result <- slr(train$NO2_2010, train %>% dplyr::select(matches(pred_c)), 
                     cv_n = cntr_code)
   summary(slr_result[[3]])
   slr_test_pred <- predict(slr_result[[3]], test) %>% as.vector()
   slr_test_df <- cbind(data.frame(slr=slr_test_pred), test)
   library(ggplot2)
   ggplot(slr_test_df)+
      geom_point(aes(x=slr, y=NO2_2010))
   rmse <- sqrt(mean((slr_test_pred-test$NO2_2010)^2))
   r2 <- summary(lm(NO2_2010~slr, slr_test_df))$adj.r.squared
   r2_df <- rbind(data.frame(cntr=cntr_code, r2=r2, rmse=rmse, n=nrow(test)))
   r2_df
}
r2_df <- lapply(unique(EU_data$country_is), leave_one_cntr)
r2_df2 <- Reduce(rbind, r2_df)
r2_df <- rbind(data.frame(cntr="all", r2=as.numeric(read.csv("data/SLR_summary_model.csv", sep='\t',dec = "," )[nrow(read.csv("data/SLR_summary_model.csv", sep='\t',dec = "," )),]$increR2), n=nrow(EU_data)), 
               r2_df2)
code_tbl <- read.table('../model_test/data/rawData/countryCode.txt') %>% rename(cntr=V1, cntr_name=V2)
r2_df2 <- inner_join(r2_df2, code_tbl, by="cntr")
