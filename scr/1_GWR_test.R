# To be used after running 1_modelsForMultipleYears.R
library(sp)
library(raster)
library(tmap)
library(tmaptools)
library(dplyr)
library(GWmodel)
source('scr/fun_usefulFunctions.R')
library(sf)
eu_bnd <- st_read("../airbase/project/data/raw/expanse_shp/eu_expanse2.shp")

EU_data <- read.csv('data/NO2_2010.csv') %>% na.omit()
# truncate x and y
EU_data <- EU_data %>% mutate(x = Xcoord-min(Xcoord)/(max(Xcoord)-min(Xcoord)),
                        y = Ycoord-min(Ycoord)/(max(Ycoord)-min(Ycoord)))
# transform altitude
EU_data <- EU_data %>% mutate(alt_t = sqrt((alt10_enh-min(alt10_enh))/max(alt10_enh-min(alt10_enh))))

summary(EU_data)
pred <- list(c('alt10_enh','Xcoord', 'Ycoord', 'XY'),
             'clc10',
             'clc14',
             'clc3',
             'clc5',
             'clc7',
             'MAJRDS_EU',
             'ROADS_EU',
             c('no2_10MACC', 'PM25_10_MACC','no2_10SAT', 'PM25_10_SAT'),
             'RES')
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

tt <- gen_train_test(EU_data, 123, 0.8)
train_data <- tt[[3]]
test_data <- tt[[4]]

# train_data <- train_data[, c('MAJRDS_EU_10p', 'NO2',
#                              'Xcoord', 'Ycoord')]
# test_data <- test_data[, c('MAJRDS_EU_10p', 'NO2',
#                              'Xcoord', 'Ycoord')]
# train_data$MAJRDS_EU_10p <- scale(train_data$MAJRDS_EU_10p)
# train_data$NO2 <- scale(train_data$NO2)
# mean(train_data$MAJRDS_EU_10p)
# sd(train_data$MAJRDS_EU_10p)
# mean(train_data$NO2)
# sd(train_data$NO2)
# test_data$MAJRDS_EU_10p <- scale(test_data$MAJRDS_EU_10p)
# test_data$NO2 <- scale(test_data$NO2)


local_crs <- CRS("+init=EPSG:3035")
sp_train <- sp::SpatialPointsDataFrame(data = train_data,
                                       coords = cbind(train_data$Xcoord, train_data$Ycoord),
                                       proj4string = local_crs)
sp_test <- sp::SpatialPointsDataFrame(data = test_data,
                                      coords = cbind(test_data$Xcoord, test_data$Ycoord),
                                      proj4string = local_crs)
names(sp_train)
colours = c("dark blue", "blue", "red", "dark red")

# EDA
linmod <- lm(NO2_2010~MAJRDS_EU_10p,data=sp_train) # Store the regression model to use in a plot later
summary(linmod)
plot(NO2_2010~MAJRDS_EU_10p, data=sp_train,xlab='Major roads (10p neighboring)',ylab='NO2 concentrations (ug/m3)')
abline(linmod)

linmod <- lm(NO2_2010~no2_10MACC,data=sp_train) # Store the regression model to use in a plot later
summary(linmod)
plot(NO2_2010~no2_10MACC, data=sp_train,xlab='no2_10MACC',ylab='NO2 concentrations (ug/m3)')
abline(linmod)
# Is this linear relationship the same everywhere in London area?
panel.lm <- function(x,y,...) {
   points(x, y, pch=16)
   abline(lm(y~x))
}
#the data area divided into subsets on the basis of their locations
coplot(NO2_2010~no2_10MACC|Xcoord*Ycoord,data=data.frame(sp_train),
       panel=panel.lm, overlap=0.5)

xmin <- extent(eu_bnd)[1]
ymin <- extent(eu_bnd)[3]
xmax <- extent(eu_bnd)[2]
ymax <- extent(eu_bnd)[4]
cellsize <- 200000
grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                 c(cellsize,cellsize),
                                 c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)))

plot(grd2)
plot(eu_bnd[1], pch=16, col='firebrick',add=TRUE)


DM <- gw.dist(dp.locat=coordinates(sp_train),
              rp.locat=coordinates(grd2))
#------ (Test) whether using a single predictor in GWR shows significant spatial variation---------
eq <- as.formula(NO2_2010~no2_10MACC+MAJRDS_EU_10p)
gwr.res.t <- gwr.basic(eq,
                     data=sp_train, 
                     regression.points=grd2, bw=50000000, 
                     dMat=DM,kernel='exponential')
image(gwr.res.t$SDF,'no2_10MACC', main="no2_10MACC")
# image(gwr.res.t$SDF,'Intercept')
plot(eu_bnd[1],add=TRUE,pch=16,col='transparent',border='grey', alpha=0.05)
contour(gwr.res.t$SDF,'no2_10MACC',lwd=2, labcex=1.5,add=TRUE)

# Extract the pred coef values
coef_stack <- stack(gwr.res.t$SDF)

gen_df_gwr <- function(coef_stack, sp_p, df_p){
   # extract coefficient values for each point
   coef_df <- lapply(seq_along(sp_p), function(loc_i) extract(coef_stack, sp_p[loc_i,]))
   coef_df <- Reduce(rbind, coef_df)
   
   predictor_test <- cbind(Intercept=1, df_p %>% dplyr::select(colnames(coef_df)[-1]))
   gwr_test_pred <- (predictor_test * coef_df) %>% apply(., 1, sum)
   gwr_test_df <- cbind(data.frame(gwr=gwr_test_pred), df_p)
   gwr_test_df
}
gwr_test_df <- gen_df_gwr(coef_stack, sp_test, test_data)
gwr_test_df <- gwr_test_df %>% mutate(lm=predict(lm(eq, 
                                                    data=train_data), test_data))
# gwr_train_df <- gen_df_gwr(param, coef_stack, sp_train, train)
gwr_train_df <- cbind(data.frame(gwr=(gwr.res.t$lm)$fitted.values %>% as.vector()), train_data)
library(ggplot2)
ggplot(gwr_test_df)+
   geom_point(aes(x=gwr, y=NO2_2010))+
   geom_point(aes(x=lm, y=NO2_2010), col="red")
ggplot(gwr_test_df)+
   geom_point(aes(x=lm, y=gwr))
rmse <- sqrt(mean((gwr_test_df$gwr-test_data$NO2_2010)^2))
rmse.lm <- sqrt(mean((gwr_test_df$lm-test_data$NO2_2010)^2))
# rmse <- sqrt(mean((gwr_train_df$gwr-train_data$NO2_2010)^2))
r2 <- summary(lm(NO2_2010~gwr, gwr_test_df))$adj.r.squared
# r2 <- summary(lm(NO2_2010~gwr, gwr_train_df))$adj.r.squared
r2_df <- rbind(data.frame(r2=r2, rmse=rmse, n=nrow(test_data)))
r2_df

# basic GWR analysis
slr <- read.csv("data/SLR_summary_model.csv", sep='\t',dec = "," )
eq <- as.formula(paste0('NO2_2010~',  paste(slr$variables[-1], collapse = "+")))
summary(lm(eq, data=train_data))
gwr.res <- gwr.basic(eq,
                     data=sp_train, 
                     regression.points=grd2, bw=50000000, 
                     dMat=DM,kernel='exponential')
# bw can be chosen ‘automatically’ by cross-validation. 
# kernel: the functional form of the kernel (the weighting applied in the window) - here it is Gaussian
gwr.res$SDF
names(gwr.res)
names(gwr.res$SDF)
summary(gwr.res)
gwr.res$lm

#(the Spatial* object with the GWR results is in gwr.res$SDF - here the object is a SpatialPixelsDataFrame)
image(gwr.res$SDF,'no2_10MACC', main="no2_10MACC")
# image(gwr.res$SDF,'Intercept')
plot(eu_bnd[1],add=TRUE,pch=16,col='transparent',border='grey', alpha=0.05)
contour(gwr.res$SDF,'no2_10MACC',lwd=2, labcex=1.5,add=TRUE)
# plot(sp_train,add=TRUE,pch=16,col='blueviolet', alpha=0.05)

image(gwr.res$SDF,'ROADS_EU_20p', main="ROADS_EU_20p")
plot(eu_bnd[1],add=TRUE,pch=16,col='transparent',border='grey', alpha=0.05)
contour(gwr.res$SDF,'ROADS_EU_20p',lwd=2, labcex=1.5,add=TRUE)


image(gwr.res$SDF,'Intercept', main='intercept')
plot(eu_bnd[1],add=TRUE,pch=16,col='transparent',border='grey', alpha=0.05)
contour(gwr.res$SDF,'Intercept',lwd=2, labcex=1.5,add=TRUE)
# plot(sp_train,add=TRUE,pch=16,col='blueviolet', alpha=0.05)


head(gwr.res$SDF['Intercept'])
class(gwr.res$SDF['Intercept'])

#the data area divided into subsets on the basis of their locations
coplot(NO2_2010~no2_10MACC|Xcoord*Ycoord,data=data.frame(sp_train),
       panel=panel.lm, overlap=0.5)

coplot(NO2_2010~ROADS_EU_20p|Xcoord*Ycoord,data=data.frame(sp_train),
       panel=panel.lm, overlap=0.5)
# doubt how the predictionr are calculated
# DM_tt <- gw.dist(dp.locat = coordinates(sp_train), 
#                  rp.locat = coordinates(sp_test))
# 
# gwr_test <- gwr.predict(formula = NO2~MAJRDS_EU_10p,
#                         data = sp_train, predictdata = sp_test,
#                         dMat1 = DM_tt, #adaptive = T,
#                         bw = 50000000, kernel = "exponential")
# not working
# gwr_test$SDF %>% names
# image(gwr_test$SDF,'MAJRDS_EU_10p_coef')
# contour(gwr_test$SDF,'MAJRDS_EU_10p_coef',lwd=1,add=TRUE)
# plot(sp_train,add=TRUE,pch=16,col='blueviolet', alpha=0.1)

# gwr_pred <- gwr_test$SDF['prediction'] %>% 
#    data.frame() %>% 
#    rename(Xcoord = coords.x1, Ycoord = coords.x2, pred = prediction)
# gwr_result <- inner_join(gwr_pred, test_data, by = c('Xcoord', 'Ycoord')) %>% 
#    rename(obs = NO2) %>% 
#    mutate(res = obs - pred)
# error_matrix(gwr_result$obs, gwr_result$pred)
# error_matrix(test_data$NO2, predict(linmod, test_data))
########## 1. GWR #######
DM <- gw.dist(dp.locat = coordinates(sp_train), rp.locat = coordinates(sp_reg))
# BW <- bw.gwr(formula = NO2~no2_10MACC, data = sp_train, approach = "CV", kernel = "gaussian",
#              adaptive = T, p = 2, dMat = DM)
gwr_train <- gwr.basic(NO2~no2_10MACC+MAJRDS_EU_1p+Xcoord+ROADS_EUp+ROADS_EU_50p+RES_5, 
                     data = sp_train, dMat = DM, adaptive = T,
                     regression.points = sp_reg, kernel = "gaussian", 
                     bw=10000)
gwr_train
# validate
gwr_train$SDF %>% names
gwr_train$SDF %>% class

DM_tt <- gw.dist(dp.locat = coordinates(sp_train), rp.locat = coordinates(sp_test))

gwr_test <- gwr.predict(formula = NO2~no2_10MACC+MAJRDS_EU_1p+Xcoord+ROADS_EUp+ROADS_EU_50p+RES_5,
            data = sp_train, predictdata = sp_test,
            dMat1 = DM_tt, adaptive = T,
            bw = 10000, kernel = "gaussian")

gwr_test$SDF %>% names
gwr_pred <- gwr_test$SDF['prediction'] %>% 
   data.frame() %>% 
   rename(Xcoord = coords.x1, Ycoord = coords.x2, pred = prediction)
gwr_result <- inner_join(gwr_pred, test_data, by = c('Xcoord', 'Ycoord')) %>% 
   rename(obs = NO2) %>% 
   mutate(res = obs - pred)
error_matrix(gwr_result$obs, gwr_result$pred)

##### 1.1 visualize results####
plot(sp_train,pch=16,
     col=adjustcolor('blueviolet',alpha.f=0.4))
contour(gwr_train$SDF,'no2_10MACC',lwd=3,add=TRUE)
image(gwr_train$SDF,'Intercept')
contour(gwr_train$SDF,'Intercept',lwd=3,add=TRUE)
image(gwr_train$SDF,'no2_10MACC')
contour(gwr_train$SDF,'no2_10MACC',lwd=3,add=TRUE)
# plot(sp_reg, add=T)
# plot(sp_train,pch=16, add=T,
#      col=adjustcolor('blueviolet',alpha.f=0.1))
plot(ctry_bnd_c, border='lightgrey', add=T)
library(rgeos)
library(rgdal)

tm_shape(gwr_train$SDF['Intercept'])+
   tm_dots(shape = 3)

# gwr_train <- gwr.basic(NO2~no2_10MACC, data = sp_train,  #regression.points = sp_reg
#                      bw=10000, kernel='gaussian')

####### 0. visualization #######
panel.lm <- function(x,y,...) {
   points(x, y)
   abline(lm(y~x))
   text(x=2, y=120, paste0(coef(lm(y~x))[2] %>% as.numeric() %>% round(digits = 2)),
        adj=c(0,1))
}
coplot(NO2~no2_10MACC|Xcoord*Ycoord,
       data = train_data,
       panel = panel.lm, overlap=0.4)

coplot(NO2~MAJRDS_EU_1p|Xcoord*Ycoord,
       data = train_data,
       panel = panel.lm, overlap=0.4)

coplot(NO2~Xcoord|Xcoord*Ycoord,
       data = train_data,
       panel = panel.lm, overlap=0.4)
coplot(NO2~ROADS_EUp|Xcoord*Ycoord,
       data = train_data,
       panel = panel.lm, overlap=0.4)

coplot(NO2~ROADS_EU_50p|Xcoord*Ycoord,
       data = train_data,
       panel = panel.lm, overlap=0.4)
coplot(NO2~RES_5|Xcoord*Ycoord,
       data = train_data,
       panel = panel.lm, overlap=0.8)



# spplot(sp_train, 'NO2', col.regions=colours, cex=0.6)

# boxplot(sp_train$NO2)
# plot(slr_model)
# res <- residuals(slr_model)
## European administrative boundaries
# country code
ctry <- read.table("../data/rawData/countryCode.txt")$V1

bndL <- lapply(1:length(ctry), function(i){
   getData(country=ctry[i], name='GADM', level=0, path = '../data/workingData/map_vis/')})
ctry_bnd <- do.call(bind, bndL) 
ctry_bnd_t <- spTransform(ctry_bnd, local_crs)
ctry_bnd_c <- crop(ctry_bnd_t, bnd)
writeOGR(obj=ctry_bnd_c, dsn='../data/workingData/map_vis',
         layer='ctry_bnd_c', driver="ESRI Shapefile")

## for every year
plot_conc_map <- function(i, yr, df1, brks){
   df_subset <- df1 %>% dplyr::filter(Year==yr[i])
   sp_subset <- sp::SpatialPointsDataFrame(data = df_subset,
                                           coords = cbind(df_subset$Xcoord, df_subset$Ycoord),
                                           proj4string = local_crs)
   t <- tm_shape(sp_subset)+
      tm_dots("NO2", breaks=brks, size=0.03,
              palette = rev(get_brewer_pal("Spectral", n = 9)), 
              title=paste0(yr[i], "\nconc"), style = 'cont')+
      tm_shape(ctry_bnd)+
      tm_borders(col='green', alpha=0.3)
   return(t)
}

plot_m_conc_maps <- function(df1, out_fig_names, breaks){
   
   yr_c <- df1$Year %>% unique() %>% sort()
   tmap_train_l <- lapply(seq_along(yr_c), plot_conc_map, yr = yr_c, df1 = df1, brks = breaks)
   tmap_all <- do.call(tmap_arrange, tmap_train_l)
   tmap_save(tmap_all, filename = paste0('../graph/poll/', out_fig_names, '.tiff'), 
             dpi=300, height=18, width=15, units='in')
}
plot_m_conc_maps(train_data, 'obs_train', seq(0, 130, length.out = 8))
plot_m_conc_maps(test_data, 'obs_train', seq(0, 130, length.out = 8))
plot_m_conc_maps(poll_train %>% mutate(NO2=obs-pred), 'res_train', seq(-40, 90, length.out = 8))
plot_m_conc_maps(poll_test %>% mutate(NO2=obs-pred), 'res_test', seq(-40, 90, length.out = 8))
