plot_gwr_coef <- function(csv_i=1, gwr_model, csv_name, n_row, n_col, eu_bnd){
   # nngbs <- (lapply(paste0("data/workingData/GWR_nngb_", names, ".txt"), read.table) %>% Reduce(rbind,.))[,1]
   # source("scr/fun_setupt_gwr.R")
   # setup <- setup_gwr(train_sub, eu_bnd, 
   #                    cellsize = 200000, local_crs = CRS("+init=EPSG:3035"))
   # sp_train <- setup[[1]]
   # grd <- setup[[2]]
   # DM <- setup[[3]]
   # gwr_model <- gwr(sp_train, grd, DM, nngb, names[csv_i])
   fun <- function() {
      plot(eu_bnd[1], add = TRUE,col='transparent',border='grey', alpha=0.05)
   }
   # print(csv_names[csv_i])
   # print(read.csv(paste0("data/workingData/SLR_summary_model_", names[csv_i], '.csv'))[,c(1:2,7)])
   # print(paste0("nearest neighbours: ", nngb))
   # plot(stack(gwr_model$SDF), addfun = fun)
   # breaks <- list(seq(10,60,length.out = 6), seq(0,20,length.out = 6))
   coef_maps <- gwr_model$SDF[names(gwr_model$SDF)[!grepl('time_stamp', names(gwr_model$SDF))]]
   gwr_plot <- vector(mode = "list", length = ncol(coef_maps))
   
   for(plot_i in seq_along(names(coef_maps))){ #seq_along(names(gwr_model$SDF))
      # r_c <- gwr_model$SDF[plot_i] 
      r_c <- coef_maps[plot_i]
      gridded(r_c) <- T
      r_c <- raster(r_c)
      gwr_plot[[plot_i]] <- tm_shape(r_c)+
         tm_raster(palette = viridis(5), style = "cont", title = '')+
         tm_shape(eu_bnd) +
         tm_borders(col='black')+
         tm_layout(legend.title.size = 1, legend.text.size = 0.8, 
                   legend.text.color = 'yellow', 
                   title = names(r_c),
                   title.color = 'black')
   }
   gwr_plot$nrow <- n_row
   gwr_plot$ncol <- n_col

   mergeMap <- do.call(tmap_arrange, gwr_plot)
   if(!dir.exists("graph/gwr_coef/")) dir.create("graph/gwr_coef/")
   tmap_save(mergeMap, filename = paste0('graph/gwr_coef/', csv_name, ".tiff"), 
             dpi=100, height=10, width=10, units='in')
   print(paste0('output graph/gwr_coef/', csv_name, ".tiff"))
}

# tm_shape(stack(gwr_model$SDF[1:2]))+
#    tm_raster(palette = viridis(5), style = "cont", title = '')+
#    tm_shape(eu_bnd) +
#    tm_borders(col='black')+
#    tm_layout(legend.title.size = 1.2, legend.text.size = 1, legend.text.color = 'black', 
#              title = names(stack(gwr_model$SDF[1:2])),
#              title.color = 'white')

# Vis Method 2
# levelplot(raster(gwr_model$SDF[1]), margin=FALSE)
#----------------------------
# image(gwr.res.t$SDF,'no2_10MACC', main="no2_10MACC")
# # image(gwr.res.t$SDF,'Intercept')
# plot(eu_bnd[1],add=TRUE,pch=16,col='transparent',border='grey', alpha=0.05)
# contour(gwr.res.t$SDF,'no2_10MACC',lwd=2, labcex=1.1,add=TRUE)
# par(mfrow=c(3,4))
# plot_gwr_coef <- function(i){
#    image(gwr.res.t$SDF,names(gwr.res.t$SDF)[csv_i], main=names(gwr.res.t$SDF)[csv_i])
#    # image(gwr.res.t$SDF,'Intercept')
#    plot(eu_bnd[1],add=TRUE,pch=16,col='transparent',border='grey', alpha=0.05)
#    contour(gwr.res.t$SDF,names(gwr.res.t$SDF)[csv_i],lwd=1, labcex=1.1,add=TRUE)
# }
# lapply(seq_along(names(gwr.res.t$SDF)), plot_gwr_coef)
# # method 2
# coef_stack <- stack(gwr.res.t$SDF)
# plot(coef_stack)
# 
# tm_shape(gwr.res.t$SDF['Intercept'])+
#    tm_raster(n=10,palette = "plasma", auto.palette.mapping = FALSE,
#              title="OMI")+
#    tm_shape(eu_bnd)+tm_borders(col = "white")
