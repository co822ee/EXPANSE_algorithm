library(dplyr)
library(sp)
library(GWmodel)
library(raster)

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
slr_leave_one_cntr <- function(cntr_code){
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
   list(r2_df, slr_test_df[, (1:9)])
}
slr_loao_result <- lapply(unique(EU_data$country_is), slr_leave_one_cntr)
library(purrr)
slr_r2_df <- map(slr_loao_result, 1)
slr_test_df <- map(slr_loao_result, 2)
slr_r2_df2 <- Reduce(rbind, slr_r2_df)
slr_test_df <- Reduce(rbind, slr_test_df)
# slr_r2_df <- rbind(data.frame(cntr="all", r2=as.numeric(read.csv("data/SLR_summary_model.csv", sep='\t',dec = "," )[nrow(read.csv("data/SLR_summary_model.csv", sep='\t',dec = "," )),]$increR2), n=nrow(EU_data)), 
#                r2_df2)
code_tbl <- read.table('../model_test/data/rawData/countryCode.txt') %>% rename(cntr=V1, cntr_name=V2)
slr_r2_df2 <- inner_join(slr_r2_df2, code_tbl, by="cntr")

#----GWR-----
local_crs <- CRS("+init=EPSG:3035")

xmin <- EU_data_sp@bbox[1, 1]
xmax <- EU_data_sp@bbox[1, 2]
ymin <- EU_data_sp@bbox[2, 1]
ymax <- EU_data_sp@bbox[2, 2]
cellsize <- 200000
grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                 c(cellsize,cellsize),
                                 c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)))


# basic GWR analysis
gwr_leave_one_cntr <- function(cntr_code){
   train <- EU_data %>% filter(country_is != cntr_code)
   test <- EU_data %>% filter(country_is == cntr_code)
   EU_data_sp <- sp::SpatialPointsDataFrame(data = EU_data,
                                            coords = cbind(EU_data$Xcoord, EU_data$Ycoord),
                                            proj4string = local_crs)
   sp_train <- sp::SpatialPointsDataFrame(data = train,
                                          coords = cbind(train$Xcoord, train$Ycoord),
                                          proj4string = local_crs)
   sp_test <- sp::SpatialPointsDataFrame(data = test,
                                         coords = cbind(test$Xcoord, test$Ycoord),
                                         proj4string = local_crs)
   DM <- gw.dist(dp.locat=coordinates(sp_train),
                 rp.locat=coordinates(grd2))
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_", cntr_code, ".csv"))
   param <- slr$variables[-1]
   eq <- as.formula(paste0('NO2_2010~',  paste(param, collapse = "+")))
   summary(lm(eq, data=train))
   gwr.res <- gwr.basic(eq,
                        data=sp_train, 
                        regression.points=grd2, bw=50000000, 
                        dMat=DM,kernel='exponential')
   names(gwr.res)
   names(gwr.res$SDF)

   coef_stack <- stack(gwr.res$SDF)
   
   gen_df_gwr <- function(coef_stack, sp_p, df_p){
      # extract coefficient values for each point
      coef_df <- lapply(seq_along(sp_p), function(loc_i) extract(coef_stack, sp_p[loc_i,]))
      coef_df <- Reduce(rbind, coef_df)
      
      predictor_test <- cbind(Intercept=1, df_p %>% dplyr::select(colnames(coef_df)[-1]))
      gwr_test_pred <- (predictor_test * coef_df) %>% apply(., 1, sum)
      gwr_test_df <- cbind(data.frame(gwr=gwr_test_pred), df_p)
      gwr_test_df
   }
   gwr_test_df <- gen_df_gwr(coef_stack, sp_test, test)
   # gwr_train_df <- gen_df_gwr(coef_stack, sp_train, train)
   gwr_train_df <- cbind(data.frame(gwr=(gwr.res$lm)$fitted.values %>% as.vector()), train)

   library(ggplot2)
   ggplot(gwr_test_df)+
      geom_point(aes(x=gwr, y=NO2_2010))  # The result of slr and gwr more or less is the same for NL
   rmse <- sqrt(mean((gwr_test_df$gwr-test$NO2_2010)^2))
   r2 <- summary(lm(NO2_2010~gwr, gwr_test_df))$adj.r.squared
   r2_df <- rbind(data.frame(cntr=cntr_code, r2=r2, rmse=rmse, n=nrow(test)))
   list(r2_df, gwr_test_df[, (1:9)], gwr_train_df[, (1:9)])
}
gwr_loao_result <- lapply(unique(EU_data$country_is), gwr_leave_one_cntr)
gwr_r2_df <- map(gwr_loao_result, 1)
gwr_test_df <- map(gwr_loao_result, 2)
gwr_train_df <- map(gwr_loao_result, 3)
gwr_r2_df2 <- Reduce(rbind, gwr_r2_df)
gwr_test_df <- Reduce(rbind, gwr_test_df)
gwr_train_df <- Reduce(rbind, gwr_train_df)
# r2_df <- rbind(data.frame(cntr="all", r2=as.numeric(read.csv("data/SLR_summary_model.csv", sep='\t',dec = "," )[nrow(read.csv("data/SLR_summary_model.csv", sep='\t',dec = "," )),]$increR2), n=nrow(EU_data)), 
#                r2_df2)
code_tbl <- read.table('../model_test/data/rawData/countryCode.txt') %>% rename(cntr=V1, cntr_name=V2)
gwr_r2_df2 <- inner_join(gwr_r2_df2, code_tbl, by="cntr")
View(gwr_r2_df2)
View(slr_r2_df2)
# The result of gwr and slr is similar...
gwr_uncertain <- gwr_train_df %>% group_by(Station) %>% summarise(gwr_sd=sd(gwr), 
                                                                  gwr_mean=mean(gwr), 
                                                                  type_of_st=unique(type_of_st),
                                                                  x=unique(Xcoord),
                                                                  y=unique(Ycoord))
hist(gwr_uncertain$gwr_sd)
gwr_uncertain <- inner_join(gwr_uncertain, data.frame(Station=EU_data$Station, region=as.factor(EU_data$REGION)), by="Station")

ggplot(gwr_uncertain)+
   geom_point(aes(gwr_mean, gwr_sd, col=type_of_st))
ggplot(gwr_uncertain)+
   geom_point(aes(gwr_mean, gwr_sd, col=region))
library(sf)
library(tmap)
locations_sf = st_as_sf(gwr_uncertain, coords = c("x","y"))
st_crs(locations_sf) <- crs("+init=epsg:3035")
osm_unc = tm_shape(locations_sf) +
   tm_dots( "gwr_sd", col = "gwr_sd", size = "gwr_sd", scale = 0.3,
            title = "NO2 sd in predictions (LOAO)",
            popup.vars = c("gwr_sd", "Station", "type_of_st")) +
   tm_scale_bar(text.size=1.2, text.color='black', 
                position = c('center','bottom'))
osm_unc
tmap_save(osm_unc, "result/NO2sd.html")
osm_mean = tm_shape(locations_sf) +
   tm_dots( "gwr_mean", col = "gwr_mean", size = "gwr_mean", scale = 0.3,
            title = "NO2 mean in predictions (LOAO)",
            popup.vars = c("gwr_mean", "Station", "type_of_st")) +
   tm_scale_bar(text.size=1.2, text.color='black', 
                position = c('center','bottom'))
osm_mean
tmap_save(osm_mean, "result/NO2mean.html")

