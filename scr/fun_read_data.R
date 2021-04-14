
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

# Read in met data
met <- read.csv("../expanse_multiyear/data/raw/met_00_19.csv")  # Met data from GEE
met <- met %>% 
   dplyr::select(-".geo", -"system.index") %>%
   rename(station_european_code=Station)
met_yr <- met %>% names() %>% 
   strsplit(., "_") %>% 
   lapply(.,  `[[`, 2) %>% 
   unlist() %>% as.numeric()
met_yr[is.na(met_yr)] <- 0


## subset samples (for multiple years or each year)
subset_df_yrs <- function(obs_df, yr_target){
   no2_e_sub <- obs_df %>% filter(year%in%yr_target)
   
   # Single year
   if(length(yr_target)==1){
      # Add omi (if the year include omi)
      if(paste0("omi_", yr_target)%in%names(no2_e_sub)){
         omi <- no2_e_sub[, paste0("omi_",yr_target)]
         no2_e_sub <- no2_e_sub %>% dplyr::select(-matches("omi"))
         no2_e_sub$omi <- omi
      }else{
         no2_e_sub <- no2_e_sub %>% dplyr::select(-matches("omi"))
      }
      # Add met
      if(all(yr_target%in%met_yr)){
         met_target <- met[, met_yr==yr_target]
         # names(met_target) <- lapply(strsplit(names(met_target), "_"), `[[`, 1) %>% unlist()
         met_target$station_european_code <- met$station_european_code
         no2_e_sub <- inner_join(no2_e_sub, met_target, by="station_european_code")
      }
      
   }else{
      # Multiple years
      # Add omi
      if(all(paste0("omi_", yr_target)%in%names(no2_e_sub))){
         # no_omi_year_i <- which(!(paste0("omi_", no2_e_sub$year)%in%names(no2_e_sub)))
         omi_str <- paste0("omi_", no2_e_sub$year)
         # Assign the omi values for each year
         omi <- sapply(seq_along(omi_str), function(i) no2_e_sub[i, omi_str[i]])
         no2_e_sub <- no2_e_sub %>% dplyr::select(-matches("omi"))
         no2_e_sub$omi <- omi
      }else{
         no2_e_sub <- no2_e_sub %>% dplyr::select(-matches("omi"))
      }
      # Add met
      if(all(yr_target%in%met_yr)){
         met_target <- met[, met_yr%in%yr_target]
         met_var <- lapply(strsplit(names(met_target), "_"), `[[`, 1) %>% unlist %>% unique
         met_str <- lapply(met_var, function(met_str) paste0(met_str, "_", no2_e_sub$year))
         no2_e_sub_s <- inner_join(no2_e_sub, met, "station_european_code")
         met_target <- sapply(seq_along(met_str), 
                              function(str_i){
                                 sapply(seq_along(met_str[[str_i]]), function(i) no2_e_sub_s[i, met_str[[str_i]][i] ])
                              }
         ) %>% as.data.frame()
         names(met_target) <- met_var
         # identical(no2_e_sub_s$station_european_code, no2_e_sub$station_european_code)
         no2_e_sub <- cbind(no2_e_sub, met_target)
      }
      
   }
   no2_e_sub
}
