source("scr/fun_call_lib.R")
source("scr/fun_read_data.R")
regression_grd_cellsize <- 200   #km
year_target <- 2009
kernel_type <- "exponential"
reg_grdsize <- regression_grd_cellsize*1000
csv_name <- paste0('run2_', year_target)

print(csv_name)
no2_e_09_11 <- subset_df_yrs(no2_e_all, year_target)
data_all <- no2_e_09_11

#f# stratified by station types, climate zones and/or years
set.seed(seed)
data_all$index <- 1:nrow(data_all)
train_sub <- stratified(data_all, c('type_of_st', 'zoneID'), 0.8)
test_sub <- data_all[-train_sub$index, ]
# --> yes indeed the stations are not the same in training and test data
#TODO we need to look at the groups separately or in combined?
#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_sub <- proc_in_data(train_sub, neg_pred)
test_sub <- proc_in_data(test_sub, neg_pred)
data_all <- rbind(train_sub, test_sub)

#----------hypothesis test (p-value)-------
# output <- read.csv(paste0("data/workingData/SLR_summary_model_", 
#                           csv_name, ".csv"), header = T)
# eq <- as.formula(paste0('obs~', paste(output$variables[-1], collapse = "+")))
# source("scr/fun_setupt_gwr.R")
# setup <- setup_gwr(train_sub, eu_bnd, 
#                    cellsize = reg_grdsize, local_crs = local_crs)
# sp_train <- setup[[1]]
# grd <- setup[[2]]
# DM <- setup[[3]]
# DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
#                 rp.locat=coordinates(sp_train))
# nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
#                adaptive = T, dMat = DM_1)
# gwr_model <- gwr.basic(eq, sp_train, regression.points=grd,
#                        adaptive = T, bw=nngb, dMat=DM, kernel=kernel_type)
# t_test <- gwr.t.adjust(gwr_model)

#----------VIF evaluation----------
output <- read.csv(paste0("data/workingData/SLR_summary_model_", 
                            csv_name, ".csv"), header = T)
eq <- as.formula(paste0('obs~', paste(output$variables[-1], collapse = "+")))
lm_model <- lm(eq, train_sub)
library(car)
vif(lm_model)<3
# All are below 3 (No collinearity in global regression model)
# Check local collinearity for basic GWR
source("scr/fun_setupt_gwr.R")
setup <- setup_gwr(train_sub, eu_bnd, 
                   cellsize = reg_grdsize, local_crs = local_crs)
sp_train <- setup[[1]]
grd <- setup[[2]]
DM <- setup[[3]]
DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                rp.locat=coordinates(sp_train))
nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
               adaptive = T, dMat = DM_1)
gwr_collin <- gwr.collin.diagno(eq, sp_train, kernel = kernel_type, bw=nngb,
                                adaptive = T, dMat = DM_1)
gwr_collin %>% str
# VIF spatially
spplot(gwr_collin$SDF[1:5])
(gwr_collin$SDF[1:5])
which(gwr_collin$VIF>=3, arr.ind = T)
which(gwr_collin$VIF>=5, arr.ind = T)
which(gwr_collin$local_CN>=3)
which(gwr_collin$local_CN>=10)
which(gwr_collin$VIF>=10, arr.ind = T)
hist(gwr_collin$local_CN)
length(which(gwr_collin$local_CN>=20))/length(gwr_collin$local_CN)
# For some observation points, the VIFs of some predictors exceed 3
# It is not ideal to remove the predictors, when only a local collinearity effect is present.
# We can do nothing, but such collinearity effects can cause a loss of precision and power in the coefficient estimates.
# Also, studies have shown that GWR may find patterns in the coefficients where no spatial patterns are actually present (ref).
# Therefore, here we try to use locally-compensated GW regression (with ridge term)
# Biased local estimations are only used at locations where collinearity is likely to be an issue
nngb_lcr <- bw.gwr.lcr(eq, sp_train, kernel = kernel_type,
                       lambda.adjust = T, cn.thresh = 20,  #commonly set between 20 and 30 # lambda=1, 
                       adaptive = T, dMat = DM_1)
gwr_lcr <- gwr.lcr(eq, sp_train, regression.points=grd,
                   lambda.adjust = T, cn.thresh = 20,   # lambda=1, 
                   adaptive = T, bw=nngb_lcr, dMat=DM, kernel=kernel_type)
summary(gwr_lcr$SDF$Local_CN)
gwr_model <- gwr.basic(eq, sp_train, regression.points=grd,
                       adaptive = T, bw=nngb, dMat=DM, kernel=kernel_type)
print(gwr_model)
# source('scr/fun_plot_gwr_coef.R')
# plot_gwr_coef(i, gwr_lcr, paste0(csv_name, "_lcr20"), n_row = 3, n_col = 3, eu_bnd = eu_bnd)
# plot_gwr_coef(i, gwr_model, csv_name, n_row = 2, n_col = 3, eu_bnd = eu_bnd)
# compare the two result
# plot(gwr_model$SDF$Intercept, gwr_lcr$SDF$Intercept)
# plot(gwr_model$SDF$ROADS_EU_5p, gwr_lcr$SDF$ROADS_EU_5p)
windows()
par(mfrow=c(2,3))
for(plot_i in seq_along(names(gwr_model$SDF))){
   plot(gwr_model$SDF[[plot_i]], gwr_lcr$SDF[[plot_i]], main=names(raster(gwr_model$SDF[plot_i])))
   abline(a=0, b=1)
}
# gwr_plot <- vector(mode = "list", length = ncol(gwr_model$SDF))
# for(plot_i in seq_along(names(gwr_model$SDF))){ #seq_along(names(gwr_model$SDF))
#    gwr_plot[[plot_i]] <- tm_shape((raster(gwr_lcr$SDF[plot_i])-raster(gwr_model$SDF[plot_i]))/raster(gwr_model$SDF[plot_i])*100)+
#       tm_raster(palette = viridis(5), style = "cont", title = '')+
#       tm_shape(eu_bnd) +
#       tm_borders(col='black')+
#       tm_layout(legend.title.size = 1, legend.text.size = 0.8, 
#                 legend.text.color = 'yellow', 
#                 title = names(raster(gwr_model$SDF[plot_i])),
#                 title.color = 'black')
# }
# gwr_plot$nrow <- 2
# gwr_plot$ncol <- 3
# mergeMap <- do.call(tmap_arrange, gwr_plot)
# tmap_save(mergeMap, filename = paste0('graph/gwr_coef/', csv_name, "_gwr-lcr.tiff"), 
#           dpi=100, height=10, width=10, units='in')