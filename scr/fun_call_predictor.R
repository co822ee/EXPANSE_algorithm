# pred <- list(c('alt_t', 'x_trun', 'y_trun', 'pop2011'),     #'alt10_enh','Xcoord', 'Ycoord', 'XY'
#              'clc10',
#              'clc14',
#              'clc3',
#              'clc5',
#              'clc7',
#              'MAJRDS_EU',
#              'ROADS_EU',
#              c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT', 'DEHM_NO2_2010'),
#              'RES')
pred_c <- c(c('alt_t', 'pop2011'),  #'x_trun', 'y_trun',
            'clc10',
            'clc14',
            'clc3',
            'clc5',
            'clc7',
            'MAJRDS_EU',
            'ROADS_EU',
            c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT', 'DEHM_NO2_2010'),
            c("precip", "pressure", "temp", "uwind", "vwind", "wind"),
            'RES', 'omi') #year
neg_pred <- c('alt_t', 'clc14', 'clc7',
              "precip", "wind_speed") #'y_trun'
station_info <- c('airid', 'Year', 'country_is', 'type_of_st', 'Xcoord', 'Ycoord', 
                  'REGION', 'strata_run5', 'country_full')