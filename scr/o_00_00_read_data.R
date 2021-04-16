library(dplyr)
library(raster)
seed <- 123
local_crs <- CRS("+init=EPSG:3035")
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
# Read in station metadata
sta <- read.csv("../EXPANSE_predictor/data/processed/airbase_station_climate.csv")
# There are 3 missing rows and 7 rows with unknown values for sta_type
# Remove them
sta <- sta %>% filter(sta_type!="Unknown", sta_type!="")
# Read in airbase data
airbase <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_checked.csv")
no2 <- airbase %>% filter(component_caption=="NO2")
no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value, sta_code=station_european_code)
# str(no2)
# str(sta)
no2_sta <- inner_join(no2 %>% dplyr::select(-country_code), sta %>% dplyr::select("sta_code", "zoneID"), by="sta_code")  # the amount of data decreases because some stations with unkown station types are removed

# Read in predictor values
road <- read.csv("../EXPANSE_predictor/data/processed/road_merged.csv")
# lc <- read.csv("../EXPANSE_predictor/data/raw/gee/clc_buffer_gee.csv")
# lc <- lc %>% dplyr::select(-"system.index", -".geo")
# lc <- lc %>% rename(sta_code=station_european_code)
lc <- read.csv("../EXPANSE_predictor/data/processed/pred_2006_09.csv")
macc <- read.csv("../EXPANSE_predictor/data/processed/macc2010_pop2000.csv")
# pred <- inner_join(road, lc, by="sta_code")
pred <- inner_join(road, lc, by=names(lc)[names(lc)%in%names(road)])
pred <- inner_join(pred, macc, by="sta_code")
# Predictor used 
pred_c <- names(pred)[!(names(pred)%in%c("id", "sta_code", "cntr_code", "country_name", "sta_type", "area_type", "areaid", "xcoord", "ycoord"))]

no2_e_all <- left_join(no2_sta, pred, by="sta_code")
rm(road, lc, macc, pred, sta, airbase, no2, no2_sta)
## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   no2_e_sub
}