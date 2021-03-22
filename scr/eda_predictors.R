source("scr/fun_read_data.R")
source("scr/fun_call_lib.R")
library(corrplot)
# no_sub <- strsplit(names(elapse_no2), "_")
# no_sub[is.na(no_sub)]=1
# names(elapse_no2)[order(no_sub)]
# names(elapse_no2)[order(no_sub, names(elapse_no2))]
# parse_number(elapse_no2 %>% filter(country_is=="NL") %>% dplyr::select(matches("MAJRDS|ROADS|RES")) %>%  names)
elapse_no2 %>% filter(country_is=="DE") %>% dplyr::select(matches("MAJRDS|ROADS|RES")) %>% 
   cor %>% corrplot(type = 'upper', method = 'pie', tl.cex = 0.8)
