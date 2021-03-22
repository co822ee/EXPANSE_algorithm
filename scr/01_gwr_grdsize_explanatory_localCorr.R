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
gw.ss.bx2 <- gwss(sp_train, vars = c(slr$variables[-1], 'obs'), kernel = 'gaussian', 
                 adaptive = T, bw=nngb)
gw.ss.bx3 <- gwss(sp_train, vars = c(slr$variables[-1], 'obs'), kernel = 'gaussian', 
                 adaptive = T, bw=700) #40%
gw.ss.bx4 <- gwss(sp_train, vars = c(slr$variables[-1], 'obs'), kernel = 'gaussian', 
                 adaptive = T, bw=260) #15%
spplot(gw.ss.bx2$SDF, "no2_10MACC_LSD", col.regions=mypalette.1, key.space='right')
spplot(gw.ss.bx2$SDF, "no2_10MACC_LM")
spplot(gw.ss.bx2$SDF, "ROADS_EU_20p_LSD")
spplot(gw.ss.bx2$SDF, "ROADS_EU_20p_LM")
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
plots <- lapply(paste0("Corr_", slr$variables[-1], ".obs"), plot_cor, 
                raster_stack=gw.ss.bx2, palette_region=mypalette.1)
do.call(grid.arrange, plots)
plots_spearman <- lapply(paste0("Spearman_rho_", slr$variables[-1], ".obs"), plot_cor, 
                raster_stack=gw.ss.bx2, palette_region=mypalette.1)
do.call(grid.arrange, plots_spearman)

plots <- lapply(paste0("Corr_", slr$variables[-1], ".obs"), plot_cor, 
                raster_stack=gw.ss.bx3, palette_region=mypalette.1)
do.call(grid.arrange, plots)


plots <- lapply(paste0("Corr_", slr$variables[-1], ".obs"), plot_cor, 
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