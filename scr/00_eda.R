source("scr/fun_call_lib.R")
source("scr/fun_create_fold.R")
source("scr/fun_read_data.R")
source("scr/fun_call_predictor.R")
years <- 2010
i=1
fold_i=1
no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
# data_all <- no2_e_09_11
print(paste0("year: ", unique(no2_e_09_11$year)))
data_all <- create_fold(no2_e_09_11, seed)

test_data <- data_all[data_all$nfold==fold_i,]
train_data <- data_all[-test_data$index, ] #data_all$index starts from 1 to the length.
# normalized the predictor values
test_data <- test_data %>% mutate_at(names(dplyr::select(test_data, matches(pred_c))), scale)
train_data <- train_data %>% mutate_at(names(dplyr::select(train_data, matches(pred_c))), scale)
dim(train_data)
dim(test_data)
#f# SLR: select predictors
# train_data <- train_data[, c('MAJRDS_EU_10p', 'obs',
#                              'Xcoord', 'Ycoord')]
# test_data <- test_data[, c('MAJRDS_EU_10p', 'obs',
#                              'Xcoord', 'Ycoord')]
# train_data$MAJRDS_EU_10p <- scale(train_data$MAJRDS_EU_10p)
# train_data$obs <- scale(train_data$obs)
# mean(train_data$MAJRDS_EU_10p)
# sd(train_data$MAJRDS_EU_10p)
# mean(train_data$obs)
# sd(train_data$obs)
# test_data$MAJRDS_EU_10p <- scale(test_data$MAJRDS_EU_10p)
# test_data$obs <- scale(test_data$obs)


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
linmod <- lm(obs~MAJRDS_EU_10p,data=sp_train) # Store the regression model to use in a plot later
summary(linmod)
plot(obs~MAJRDS_EU_10p, data=sp_train,xlab='Major roads (10p neighboring)',ylab='NO2 concentrations (ug/m3)')
abline(linmod)

linmod <- lm(obs~no2_10MACC,data=sp_train) # Store the regression model to use in a plot later
summary(linmod)
plot(obs~no2_10MACC, data=sp_train,xlab='no2_10MACC',ylab='NO2 concentrations (ug/m3)')
abline(linmod)
# Is this linear relationship the same everywhere in London area?
panel.lm <- function(x,y,...) {
   points(x, y, pch=16)
   abline(lm(y~x))
   # text(-1, 80, round(cor(x, y)[1], 2))
   text(-0.6, 80, round(as.numeric(lm(y~x)$coefficient[2]), 2))
   # text(-1, 65, length(x))
}
#the data area divided into subsets on the basis of their locations
coplot(obs~no2_10MACC|Xcoord*Ycoord,data=data.frame(sp_train),
       panel=panel.lm, overlap=0.5)

coplot(obs~MAJRDS_EU_10p|Xcoord*Ycoord,data=data.frame(sp_train),
       panel=panel.lm, overlap=0.5)

coplot(obs~ROADS_EU_10p|Xcoord*Ycoord,data=data.frame(sp_train),
       panel=panel.lm, overlap=0.1)


#-----------method 2---------------
# This script uses gwr local statistics to demonstrate the local variations (especially for no2_10MACC)
library(gridExtra)
library(RColorBrewer)
mypalette.1 <- viridis(5)
source("scr/fun_call_lib.R")
source("scr/fun_create_fold.R")
source("scr/fun_read_data.R")
source("scr/fun_call_predictor.R")
years <- 2010
i=1
fold_i=1
no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
# data_all <- no2_e_09_11
print(paste0("year: ", unique(no2_e_09_11$year)))
data_all <- create_fold(no2_e_09_11, seed)

test_data <- data_all[data_all$nfold==fold_i,]
train_data <- data_all[-test_data$index, ] #data_all$index starts from 1 to the length.

local_crs <- CRS("+init=EPSG:3035")
sp_train <- sp::SpatialPointsDataFrame(data = train_data,
                                       coords = cbind(train_data$Xcoord, train_data$Ycoord),
                                       proj4string = local_crs)
sp_test <- sp::SpatialPointsDataFrame(data = test_data,
                                      coords = cbind(test_data$Xcoord, test_data$Ycoord),
                                      proj4string = local_crs)
source("scr/fun_calibr_gwr.R")
gw.ss.bx2 <- gwss(sp_train, vars = c("ROADS_EU_10p", "MAJRDS_EU_10p", "no2_10MACC", 'obs'), kernel = 'gaussian', 
                  adaptive = T, bw=80)
# gw.ss.bx3 <- gwss(sp_train, vars = c(slr$variables[-1], 'obs'), kernel = 'gaussian', 
#                   adaptive = T, bw=700) #40%
# gw.ss.bx4 <- gwss(sp_train, vars = c(slr$variables[-1], 'obs'), kernel = 'gaussian', 
#                   adaptive = T, bw=260) #15%

sf_gwr <- st_as_sf(gw.ss.bx2$SDF)

ggplot(data=eu_bnd) +
   geom_sf() +
   geom_sf(data = sf_gwr, aes(fill=Spearman_rho_ROADS_EU_10p.MAJRDS_EU_10p),
           size = 2,
           shape = 21, )+
   scale_fill_viridis_c()
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

spplot(gw.ss.bx2$SDF, "no2_10MACC_LSD", col.regions=mypalette.1, key.space='right')
spplot(gw.ss.bx2$SDF, "Spearman_rho_ROADS_EU_10p.obs", main="Spearman_rho_ROADS_EU_10p.obs",
       col.regions=mypalette.1, key.space='right')
spplot(gw.ss.bx2$SDF, "Spearman_rho_MAJRDS_EU_10p.obs", main='Spearman_rho_MAJRDS_EU_10p.obs',
       col.regions=mypalette.1, key.space='right')
spplot(eu_bnd)
spplot(gw.ss.bx2$SDF, "no2_10MACC_LM")
spplot(gw.ss.bx2$SDF, "ROADS_EU_10p_LSD")
spplot(gw.ss.bx2$SDF, "ROADS_EU_10p_LM")
spplot(gw.ss.bx2$SDF, "obs_LSD")
spplot(gw.ss.bx2$SDF, "obs_LM")

# The correlation between no2_10MACC and obs is locally varied
library(classInt)

# breaks.qt <- classIntervals(data.frame(gw.ss.bx2$SDF['Corr_ROADS_EU_20p.obs'])$Corr_ROADS_EU_20p.obs, n = 6, style = "quantile", intervalClosure = "right")

# spplot(gw.ss.bx2$SDF, "Corr_ROADS_EU_20p.obs", col.regions=mypalette.1, at = seq(from=0, to=0.8, by=0.1), 
#        key.space='right', at=breaks.qt$brks)
plot_cor <- function(layer_name, raster_stack, palette_region=mypalette.1){
   spplot(raster_stack$SDF, layer_name, main=layer_name, col.regions=palette_region,
          key.space='right')
}
plots <- lapply(paste0("Corr_", c("ROADS_EU_10p", "MAJRDS_EU_10p", "no2_10MACC"), ".obs"), plot_cor, 
                raster_stack=gw.ss.bx2, palette_region=mypalette.1)
do.call(grid.arrange, plots)
plots_spearman <- lapply(paste0("Spearman_rho_", c("ROADS_EU_10p", "MAJRDS_EU_10p", "no2_10MACC"), ".obs"), plot_cor, 
                         raster_stack=gw.ss.bx2, palette_region=mypalette.1)
do.call(grid.arrange, plots_spearman)

plots <- lapply(paste0("Corr_", c("ROADS_EU_10p", "MAJRDS_EU_10p", "no2_10MACC"), ".obs"), plot_cor, 
                raster_stack=gw.ss.bx3, palette_region=mypalette.1)
do.call(grid.arrange, plots)


plots <- lapply(paste0("Corr_", c("ROADS_EU_10p", "MAJRDS_EU_10p", "no2_10MACC"), ".obs"), plot_cor, 
                raster_stack=gw.ss.bx4, palette_region=mypalette.1)
do.call(grid.arrange, plots)



# This shows that no2_10MACC could be locally varying (the accuracy of the CTM could vary in space as well)

gw.ss.bs <- gwss(sp_train, vars = slr$variables[-1], kernel = kernel_type[i], 
                 adaptive = T, bw=nngb)


gw.ss.bs2 <- gwss(sp_train, vars = slr$variables[-1], kernel = kernel_type[i], 
                  adaptive = F, bw=bandwidth_calibr)



# Demo code
# data("DubVoter")
# gw.ss.bx <- gwss(Dub.voter, vars = c("GenEl2004", "LARent", "Unempl"),
#                  kernel = "boxcar", adaptive = TRUE, bw = 48, quantile = TRUE)
# gw.ss.bs <- gwss(Dub.voter,vars = c("GenEl2004", "LARent", "Unempl"),
#                  kernel = "bisquare", adaptive = TRUE, bw = 48)
