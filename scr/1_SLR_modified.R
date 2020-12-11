library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

data <- read.csv('data/NO2_2010.csv') %>% na.omit()
str(data)
exclude <- c('Station', 'airid', 'country_is', 'type_of_st',
             'strata_run5', 'REGION')
# ggplot(data)+
#    geom_point(aes(x=country_is, y=REGION, col=airid))

data_pred <- data %>% select(-all_of(exclude))

#------build a model for 2010-----
data_2010 <- data_pred
x_2010 <- data_2010 %>% select(-NO2_2010)
y_2010 <- data_2010 %>% select(NO2_2010)

x_2010 %>% select(matches(c('clc7'))) %>% dim

#----explanatory----
pred=list(c('alt10_enh','Xcoord', 'Ycoord', 'XY'),
       'clc10',
       'clc14',
       'clc3',
       'clc5',
       'clc7',
       'MAJRDS_EU',
       'ROADS_EU',
       c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT'),
       'RES')

data_2010 %>% na.omit %>% select(matches('clc7|NO2')) %>% 
   cor %>% corrplot(type = "upper", method = "pie", tl.cex = 0.7)
ggplot(data_2010 %>% na.omit %>% select(matches('clc7|NO2')) %>% 
          gather(pred, values,-'NO2_2010'))+
   geom_point(aes(x=NO2_2010, y=values))+
   facet_wrap(pred~., scales = 'free_y')

if(!dir.exists('graph')) dir.create('graph')
if(!dir.exists('graph/predGIS')) dir.create('graph/predGIS')
plotFunction <- function(i){
   
   ggplot(data_2010 %>% select(matches(pred[[i]]),'NO2') %>%
             gather(pred, values,-'NO2'))+
      geom_point(aes(x=NO2, y=values))+
      facet_wrap(pred~., scales = 'free_y')
   ggsave(paste0('../graph/predGIS/', i, '.tiff'), width=8, height=5, dpi=300,units = 'in')
}

plotFunction_trans <- function(i){
   d <- data_2010 %>% 
      mutate_at(data_2010 %>% select(matches(pred_trans[i])) %>% names, 
                function(x) ifelse(x<1, x, -log(x))) %>%         #ifelse(x<=0, x, sqrt(x))
      select(matches(pred_trans[i]),'NO2') %>%
      gather(pred, values,-'NO2')
   ggplot(d)+
      geom_point(aes(x=NO2, y=values))+
      facet_wrap(pred~., scales = 'free_y')
   ggsave(paste0('../graph/predGIS/trans_', i, '.tiff'), width=8, height=5, dpi=300,units = 'in')
}
pred_trans <- c('clc14', 'clc7', 'alt')
#----* ('clc14', 'clc7', 'alt') needs log() transformation (NaNs problem)-----------


lapply(1:length(pred), plotFunction)
lapply(1:length(pred_trans), plotFunction_trans)
data_2010 %>% select(matches(pred[[1]]),'NO2') 
ggplot(data_2010 %>% select(matches(pred[[1]]),'NO2') %>% 
          gather(pred, values,-'NO2'))+
   geom_point(aes(x=NO2, y=values))+
   facet_wrap(pred~., scales = 'free_y')

#---- * problem: NaNs created-----
for(i in 3){
   data_2010 %>% 
      mutate_at(data_2010 %>% select(matches(pred_trans[i])) %>% names, 
                function(x) ifelse(x<1, -x, -sqrt(x))) %>%         #ifelse(x<=0, x, sqrt(x))
      select(matches(pred_trans[i]),'NO2','Station')
   
   data_2010 %>% 
      select(matches(pred_trans[i]),'NO2','Station') %>% 
      filter_at(data_2010 %>% select(matches(pred_trans[i])) %>% 
                   names, all_vars(.<1)) %>% mutate(sqrt(-alt10_enh))
}