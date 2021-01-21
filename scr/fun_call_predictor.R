pred <- list(c('alt_t', 'x', 'y', 'pop2011'),     #'alt10_enh','Xcoord', 'Ycoord', 'XY'
             'clc10',
             'clc14',
             'clc3',
             'clc5',
             'clc7',
             'MAJRDS_EU',
             'ROADS_EU',
             c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT', 'DEHM_NO2_2010'),
             'RES')
pred_c <- c(c('alt_t', 'x', 'y', 'pop2011'),
            'clc10',
            'clc14',
            'clc3',
            'clc5',
            'clc7',
            'MAJRDS_EU',
            'ROADS_EU',
            c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT', 'DEHM_NO2_2010'),
            'RES')
neg_pred <- c('alt10_enh', 'clc14', 'clc7')
station_info <- c('airid', 'Year', 'country_is', 'type_of_st', 'Xcoord', 'Ycoord', 
                  'REGION', 'strata_run5', 'country_full')