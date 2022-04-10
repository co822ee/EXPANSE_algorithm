source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# Whether to tune RF
target_poll = 'O3'
tuneRF_b = T
local_crs <- CRS("+init=EPSG:3035")
seed <- 123
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
# Multiple single years
csv_names <- paste0('all_', target_poll, "_", c(2000, 2005, 2010, 2015, 2019))   #2008:2012
# o_ and o2_ differences are whether to include data after 2013
years <- as.list(c(2000, 2005, 2010, 2015, 2019))
source("../expanse_multiyear/src/00_fun_read_data_gee.R")
for(yr_i in seq_along(csv_names)){
   df_all <- read_data(target_poll,  years[[yr_i]])
   source('../EXPANSE_algorithm/scr/fun_select_predictor.R')
   pred_c <- select_predictor(df_all)
   
   negPred <-    df_all %>% dplyr::select(pred_c, -zoneID) %>% sapply(., function(x) all(x<=0))
   negPred <- negPred[negPred]
   names(negPred)
   
   if(nrow(df_all)>200){
      
      csv_name <- csv_names[yr_i]
      print("********************************************")
      print(csv_name)
      print(target_poll)
      print("GWR")
      source("../EXPANSE_algorithm/scr/fun_setupt_gwr.R")
      
      
      setup <- setup_gwr(df_all, eu_bnd,
                         cellsize = 200000, local_crs = local_crs, xcoord="xcoord", ycoord="ycoord")
      sp_train <- setup[[1]]
      grd <- setup[[2]]
      DM <- setup[[3]]
      nngb <- read.table(paste0("data/workingData/GWR_nngb_", csv_name, ".txt"))[1,1]
      print(paste0("nngb: ", nngb))
      source("../EXPANSE_algorithm/scr/fun_gwr.R")
      gwr_model <- gwr(sp_train, grd, DM, nngb, csv_name)
      
      
      source('scr/fun_plot_gwr_coef.R')
      plot_gwr_coef(yr_i, gwr_model, csv_name, n_row = 3, n_col = 3, eu_bnd = eu_bnd, eu_bnd2 = eu_bnd,
                    negPred=negPred, mask_b=F)
   }
}
