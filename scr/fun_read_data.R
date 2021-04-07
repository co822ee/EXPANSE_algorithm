
#
seed <- 123
local_crs <- CRS("+init=EPSG:3035")

eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
## Read in data (elapse NO2 2010 with climate zones included)
# elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate.csv",
#                        encoding = "utf-8")
elapse_no2 <- read.csv("../EXPANSE_predictor/data/processed/no2_2010_elapse_climate_omi.csv",
                       encoding = "utf-8")
## Read in data (airbase observations 1990s-2012)
airbase <- read.csv("../EXPANSE_APM/data/processed/ab_v8_yr_checked.csv")
no2 <- airbase %>% filter(component_caption=="NO2")
# rename data
# colnames(elapse_no2)[1] <- "station_european_code"
# elapse_no2 <- rename(elapse_no2, station_european_code=ï..Station)
# reduce airbase data
no2 <- no2 %>% rename(year=statistics_year, obs=statistic_value)
## subset stations that are included in the elapse (cause at this stage, we don't have the predictor maps...)
no2_e <- no2 %>% filter(no2$station_european_code%in%unique(elapse_no2$station_european_code))
no2_e_all <- left_join(no2_e, elapse_no2, by="station_european_code")

## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   no2_e_sub
}
