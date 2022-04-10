plot_gwr_coef <- function(csv_i=1, gwr_model, csv_name, n_row=3, n_col=3, eu_bnd, eu_bnd2=eu_bnd, 
                          dpi=100, negPred=negPred, mask_b=T){
   
   
   fun <- function() {
      plot(eu_bnd[1], add = TRUE,col='transparent',border='grey', alpha=0.05)
   }

   coef_maps <- gwr_model$SDF[names(gwr_model$SDF)[!grepl('time_stamp', names(gwr_model$SDF))]]
   # convert the coefficient maps of the variables that have a negative direction of effects
   gwr_plot <- vector(mode = "list", length = ncol(coef_maps))
   
   cor_all_sf <- st_as_sf(data.frame(txt='', 
                                     x=c(st_bbox(eu_bnd)[1], st_bbox(eu_bnd)[3]+1000000), 
                                     y=c(st_bbox(eu_bnd)[2], st_bbox(eu_bnd)[4])), coords=c('x','y'),
                          crs=crs(coef_maps))
   
   for(plot_i in seq_along(names(coef_maps))){
      r_c <- coef_maps[plot_i]
      gridded(r_c) <- T
      r_c <- raster(r_c)
      if(names(r_c)%in%names(negPred)) r_c=r_c*(-1)
      if(mask_b){
         r_c2 <- crop(r_c, extent(eu_bnd2))
         r_c3 <- mask(r_c2, eu_bnd2)
         
         gwr_plot[[plot_i]] <- tm_shape(cor_all_sf)+
            tm_text('txt', just='top', size=1.2)+
            tm_shape(r_c3)+
            tm_raster(palette = viridis(5), style = "cont", title = names(r_c3))+
            tm_shape(eu_bnd) +
            tm_borders(col='black')+  # html dimGray
            tm_layout(title.size = 1, legend.text.size = 0.9, 
                      legend.text.color = 'black', 
                      # title.position = c('right','top'),
                      legend.position = c('right','top'),
                      title.color = 'black')
      }else{
         
         gwr_plot[[plot_i]] <- tm_shape(cor_all_sf)+
            tm_text('txt', just='top', size=1.2)+
            tm_shape(r_c)+
            tm_raster(palette = viridis(5), style = "cont", title = names(r_c))+
            tm_shape(eu_bnd) +
            tm_borders(col='black')+  # html dimGray
            tm_layout(title.size = 1, legend.text.size = 0.9, 
                      legend.text.color = 'black', 
                      # title.position = c('right','top'),
                      legend.position = c('right','top'),
                      title.color = 'black')
      }
      
   }
   gwr_plot$ncol <- n_col
   gwr_plot$nrow <- ceiling(length(names(coef_maps))/n_col) ##n_row

   mergeMap <- do.call(tmap_arrange, gwr_plot)
   if(!dir.exists("graph/gwr_coef/")) dir.create("graph/gwr_coef/")
   if(ceiling(length(names(coef_maps))/n_col)==5){
      tmap_save(mergeMap, filename = paste0('graph/gwr_coef/', csv_name, ".tiff"), 
                dpi=dpi, height=13.5, width=11.5, units='in')
   }else{
      tmap_save(mergeMap, filename = paste0('graph/gwr_coef/', csv_name, ".tiff"), 
                dpi=dpi, height=10, width=10.5, units='in')
   }
   
   print(paste0('output graph/gwr_coef/', csv_name, ".tiff"))
}

