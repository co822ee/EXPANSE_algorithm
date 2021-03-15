# This script is to compare how the coef surface changes over time
source("scr/fun_call_lib.R")
source("scr/fun_read_data.R")

#o# multiple years
csv_names <- paste0('run1_train_', c(2002, 2008:2010))
years <- as.list(c(2002, seq(2008, 2010)))
# What are the coefficients selected
slr_l <- lapply(paste0("data/workingData/SLR_summary_model_", csv_names, '.csv'), 
              read.csv)
var_select <- lapply(slr_l, function(slr) slr$variables[-1])
length(var_select)
var_same <- table(unlist(var_select))[table(unlist(var_select))==length(var_select)] %>% names

csv_name <- csv_names[i]
print(csv_name)
no2_e_subsets <- lapply(seq_along(years), function(i) subset_df_yrs(no2_e_all, years[[i]]))
data_all <- no2_e_subsets

train_subs <- lapply(seq_along(no2_e_subsets), function(i){
   set.seed(seed)
   data_sub <- no2_e_subsets[[i]]
   data_sub$index <- 1:nrow(data_sub)
   train_sub <- stratified(data_sub, c('type_of_st', 'climate_zone'), 0.8)
   train_sub
})
test_subs <- lapply(seq_along(no2_e_subsets), function(i){
   set.seed(seed)
   data_sub <- no2_e_subsets[[i]]
   data_sub$index <- 1:nrow(data_sub)
   train_sub <- stratified(data_sub, c('type_of_st', 'climate_zone'), 0.8)
   test_sub <- data_sub[-train_sub$index, ]
   test_sub
})
lapply(seq_along(no2_e_subsets), function(i) any(test_subs[[i]]$index%in%train_subs[[i]]$index))
#f# SLR: select predictors
source("scr/fun_call_predictor.R")
#f# SLR: define/preprocess predictors (direction of effect)
source("scr/fun_slr_proc_in_data.R")
train_subs <- lapply(train_subs, proc_in_data, neg_pred = neg_pred)
test_subs <- lapply(test_subs, proc_in_data, neg_pred = neg_pred)

#f# SLR: perform cross-validation

#-----------#f# GWR: train GWR----------
print("GWR")
source("scr/fun_setupt_gwr.R")
# grd is the same for different years
setup <- setup_gwr(train_subs[[1]], eu_bnd, 
                   cellsize = 200000, local_crs = local_crs)

grd <- setup[[2]]
sp_trains <- lapply(train_subs, function(train_data){
   sp::SpatialPointsDataFrame(data = train_data,
                              coords = cbind(train_data$Xcoord, train_data$Ycoord),
                              proj4string = local_crs)
})
DMs <- lapply(sp_trains, function(sp_train){
   gw.dist(dp.locat=coordinates(sp_train),
           rp.locat=coordinates(grd))
})




nngbs <- lapply(seq_along(csv_names), function(i){
   read.table(paste0("data/workingData/GWR_nngb_", csv_names[i], ".txt"))[,1]
})

nngb %>% print()
source("scr/fun_gwr.R")
gwr_models <- lapply(seq_along(csv_names), function(i){
   gwr(sp_trains[[i]], grd, DMs[[i]], nngbs[[i]], csv_names[i])
})
coef_stacks <- lapply(seq_along(csv_names), function(i){
   coef_stack <- stack(gwr_models[[i]]$SDF[var_same])
   names(coef_stack) <- paste0(names(coef_stack), "_", years[[i]])
   coef_stack
})
coef_stacks <- do.call(stack, coef_stacks)
coef_names <- names(coef_stacks)

index_l <- list(seq(1, 9, 3), seq(1, 9, 3)+1, seq(1, 9, 3)+2)
mins <- unlist(lapply(index_l, function(index) min(minValue(coef_stacks[[index]]))))
maxs <- unlist(lapply(index_l, function(index) max(maxValue(coef_stacks[[index]]))))

mins <- rep(mins, 3)
maxs <- rep(maxs, 3)

# source('scr/fun_plot_gwr_coef.R')
# plot_gwr_coef(i, n_row = 3, n_col = 4)
fun <- function() {
   plot(eu_bnd[1], add = TRUE,col='transparent',border='grey', alpha=0.05)
}
library(tmap)
gwr_plot <- vector(mode = "list", length = length(coef_names))
for(plot_i in seq_along(coef_names)){ #seq_along(names(gwr_model$SDF))
   gwr_plot[[plot_i]] <- tm_shape(coef_stacks[[plot_i]])+
      tm_raster(palette = viridis(5), style = "cont", title = '',
                breaks=seq(mins[plot_i], 
                           maxs[plot_i],length.out = 6))+
      tm_shape(eu_bnd) +
      tm_borders(col='black')+
      tm_layout(legend.title.size = 1, legend.text.size = 0.8, 
                legend.text.color = 'yellow', 
                title = names(coef_stacks)[plot_i],
                title.color = 'yellow')
}
gwr_plot$nrow <- 3
gwr_plot$ncol <- 3

mergeMap <- do.call(tmap_arrange, gwr_plot)
tmap_save(mergeMap, filename = paste0('graph/gwr_coef/compare_08-10.tiff'), 
          dpi=600, height=10, width=10, units='in')
          
