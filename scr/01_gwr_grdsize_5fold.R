# This script is intended to test GWR models with different settings of
# regression grid cellsize.
source("scr/fun_call_lib.R")
source("scr/fun_read_data.R")

#---------Test the bandwidth----------
# global_vars <- "no2_10MACC"

regression_grd_cellsize <- c(10, 20, 50, 80, 100, 200, 500, 600, 1000, 1500, 2000)   #km
kernels <- c('exponential')
year_target <- 2010
nfolds=5
comb <- expand.grid(regression_grd_cellsize=regression_grd_cellsize, kernel_type=kernels, nfold=1:nfolds) %>% 
   mutate(csv_name = paste0('testGWR_', regression_grd_cellsize, '_', kernel_type, '_', year_target, "_fold_", nfold))
kernel_type <- comb$kernel_type %>% as.character()
reg_grdsize <- comb$regression_grd_cellsize*1000
csv_names <- comb$csv_name
folds=comb$nfold
# csv_names <- paste0('testGWR_', regression_grd_cellsize, "_", year_target)
years <- as.list(rep(year_target, length(csv_names)))

# perf_matrix <- data.frame(RMSE=0, RRMSE=0, IQR=0, rIQR=0, MAE=0, rMAE=0, rsq=0, explained_var=0,
#                           csv_name="0", datatype="0")
library(doParallel)
library(foreach)
cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)
foreach(i=seq_len(nrow(comb))) %dopar% {
   
   source("scr/fun_call_lib.R")
   csv_name <- csv_names[i]
   fold_i <- folds[i]
   print(csv_name)
   no2_e_09_11 <- subset_df_yrs(no2_e_all, years[[i]])
   # data_all <- no2_e_09_11
   print(paste0("year: ", unique(no2_e_09_11$year)))
   source("scr/fun_create_fold.R")
   data_all1 <- create_fold(no2_e_09_11, seed)
   test_sub <- data_all1[data_all1$nfold==fold_i,]
   train_sub <- data_all1[-test_sub$index, ] #data_all1$index starts from 1 to the length.
   
   
   #f# SLR: select predictors
   source("scr/fun_call_predictor.R")
   #f# SLR: define/preprocess predictors (direction of effect)
   source("scr/fun_slr_proc_in_data.R")
   train_sub <- proc_in_data(train_sub, neg_pred)
   test_sub <- proc_in_data(test_sub, neg_pred)
   # data_all <- rbind(train_sub, test_sub)
   
   #------------------Above code is needed for all algorithms----------------------
   #---------#f# SLR: train SLR -----------
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_run2_", years[[i]],"_fold_", fold_i,".csv"))
   slr_poll <- read.csv(paste0('data/workingData/SLR_result_all_run2_', years[[i]], "_fold_", fold_i,".csv"))
   eq <- as.formula(paste0('obs~',  paste(slr$variables[-1], collapse = "+")))
   
   #f# SLR: perform cross-validation
   
   #-----------#f# GWR: train GWR----------
   # the fixed/adaptive calibrated bandwidth does NOT change with regression grid cellsize.
   print("GWR")
   source("scr/fun_setupt_gwr.R")
   setup <- setup_gwr(train_sub, eu_bnd, 
                      cellsize = reg_grdsize[i], local_crs = local_crs)
   sp_train <- setup[[1]]
   grd <- setup[[2]]
   DM <- setup[[3]]
   plot(coordinates(grd), col='red', pch=18)
   plot(grd, add=T, col='dark green')
   plot(eu_bnd[1], pch=16, col='transparent',add=TRUE)
   plot(sp_train, add=T, pch=20)
   # tiff(paste0("graph/gwr_coef/coef_grid_", regression_grd_cellsize[i], '.tif'), 
   #      width = 5, height=4, units='in', res=100)
   # plot(grd)
   # Calibrate bandwidth using CV
   # The calibration is not influenced by the regression grid cell size
   # if(!file.exists(paste0("data/workingData/GWR_dist_", 
   #                        kernel_type[i], "_", years[[i]], ".txt"))){
   #    DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
   #                    rp.locat=coordinates(sp_train))
   #    # 
   #    bandwidth_calibr <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type[i],
   #                               adaptive = F, dMat = DM_1)
   #    write.table(bandwidth_calibr, paste0("data/workingData/GWR_dist_", 
   #                                         kernel_type[i], "_", years[[i]], ".txt"))
   # }
   
   if(!file.exists(paste0("data/workingData/GWR_nngb_", 
                          csv_name, ".txt"))&(!file.exists(paste0("data/workingData/GWR_nngb_run2_", 
                                                                                        years[[i]], "_fold_", fold_i,".txt")))){
      DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                      rp.locat=coordinates(sp_train))
      # 
      nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type[i],
                     adaptive = T, dMat = DM_1)
      write.table(nngb, paste0("data/workingData/GWR_nngb_", 
                               csv_name, ".txt"))
   }else{
      if(file.exists(paste0("data/workingData/GWR_nngb_run2_", 
                              years[[i]], "_fold_", fold_i, ".txt"))){
         nngb <- read.table(paste0("data/workingData/GWR_nngb_run2_", 
                                    years[[i]], "_fold_", fold_i, ".txt"))[,1]
         
      }else{
         nngb <- read.table(paste0("data/workingData/GWR_nngb_", 
                                   csv_name, ".txt"))[,1]
      }
   }
   
   # nngb %>% print()
   # source("scr/fun_gwr.R")
   # bandwidth_calibr <- read.table(paste0("data/workingData/GWR_dist_", 
   #                                       kernel_type[i], "_", years[[i]], ".txt"))[,1]
   


   gwr_model_ad <- tryCatch(gwr.basic(eq,
                                      data=sp_train,
                                      regression.points=grd,
                                      adaptive = T,
                                      bw=nngb,
                                      dMat=DM,
                                      kernel=kernel_type[i]),
                            error=function(e) T)
   
   # grd_i=13
   # gw.weight(DM[,grd_i], nngb, kernel_type[i], T) %>% hist  #grd_i is the regression point index.
   # 
   # 
   # if(!file.exists(paste0("data/workingData/GWR_max_w_", 
   #                        kernel_type[i], "_", years[[i]], ".txt"))){
   #    max_w <- lapply(1:ncol(DM), function(grd_i) gw.weight(DM[,grd_i], nngb, kernel_type[i], T) %>% max)
   #    write.table(unlist(max_w), paste0("data/workingData/GWR_max_w_", 
   #                              reg_grdsize[i], "_", years[[i]], ".txt"), row.names = F)
   # }
   # hist(unlist(max_w))
   
   # error: inv(): matrix seems singular
   if(!(typeof(gwr_model_ad)=='logical')){
      #f# GWR: perform cross-validation
      source("scr/fun_output_gwr_result.R")
      
      gwr_df_ad <- output_gwr_result(gwr_model_ad, train_sub, test_sub, local_crs,
                                     output_filename = paste0(csv_name, "_ad"))
      # Output test prediction only
      output_test <- gwr_df_ad %>% 
         filter(df_type=="test") %>% 
         dplyr::select(station_european_code, year, gwr, obs, nfold)
      write.csv(output_test, paste0('data/workingData/', csv_name, '.csv'), row.names = F)
      # output all models' performance matrix
      output_em <- function(pred_df, csv_name, model, year){
         error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', 'gwr'])
         
         em <- rbind(error_matrix(pred_df[pred_df$df_type=='test', 'obs'], pred_df[pred_df$df_type=='test', model]) , 
                     error_matrix(pred_df[pred_df$df_type=='train', 'obs'], pred_df[pred_df$df_type=='train', model])) %>% 
            as.data.frame()
         
         perf_matrix <- em[, c(1, 5, 7)] %>% mutate(df_type=c('test','train'), model=model, year=year, csv_name=csv_name)
         perf_matrix
      }
      # output_em(slr_poll, paste0('slr_', years[[i]]), 'slr', years[[i]])
      # output_em(gwr_df, csv_name, 'gwr', years[[i]])
      # output_em(gwr_df_ad, paste0(csv_name, '_ad'), 'gwr', years[[i]])
      em <- output_em(gwr_df_ad, csv_name, 'gwr', years[[i]])
      em <- em  %>% arrange(df_type)
      print(em)
      write.csv(em, paste0('data/workingData/test_grdsize_', csv_name, '.csv'), row.names = F)
      
      # # plot gwr surface
      # ncol(gwr_model_ad$SDF) %>% print()  # the number of predictors selected
      # source('scr/fun_plot_gwr_coef.R')
      # plot_gwr_coef(i, gwr_model_ad, csv_name = csv_name,
      #               n_row = 3, n_col = 3, eu_bnd=eu_bnd)
      # 
      # # Output surface
      # if(!dir.exists("data/workingData/gwr_coef")) dir.create("data/workingData/gwr_coef")
      # writeRaster(stack(gwr_model_ad$SDF), paste0("data/workingData/gwr_coef/", csv_name, ".tif"))
      
   }else{
      if(!file.exists('data/workingData/gwr_failed.txt')){
         write.table(csv_name, "data/workingData/gwr_failed.txt",
                     sep = ",", row.names = F)
      }else{
         write.table(csv_name, "data/workingData/gwr_failed.txt",
                     sep = ",", row.names = F, col.names = F, append = T)
      }
   }
   gc()
}
parallel::stopCluster(cl)


perfm <- lapply(paste0('data/workingData/', list.files('data/workingData/', 'test_grdsize_')), 
                function(file_name) read.csv(file_name, header=T) )
perfm <- do.call(rbind, perfm)
perfm$reg_grdsize <- c(lapply(perfm$csv_name[1:nrow(perfm)], function(f) strsplit(f, '_')[[1]][2]) %>% unlist() %>% as.numeric())
perfm$kernel <- c(lapply(perfm$csv_name[1:nrow(perfm)], function(f) strsplit(f, '_')[[1]][3]) %>% unlist() %>% as.character())
perfm$nfold <- lapply(perfm$csv_name[1:nrow(perfm)], function(f) strsplit(f, '_')[[1]][6]) %>% unlist() %>% as.character()
# ggplot(perfm)+
#    geom_line(aes(x=reg_grdsize, y=rsq, col=df_type))+
#    geom_point(aes(x=reg_grdsize, y=rsq, col=df_type))+
#    labs(title=paste0(unique(perfm$year), " (", unique(perfm$kernel), ")"),
#         x='grid cell size of regression coefficients (km)',
#         y='R squared',
#         col='data type')
ggplot(perfm)+
   geom_boxplot(aes(x=reg_grdsize, y=rsq, fill=df_type, group=reg_grdsize))+
   facet_grid(df_type~.)+
   labs(title=paste0(unique(perfm$year), " (", unique(perfm$kernel), ")"),
        x='grid cell size of regression coefficients (km)',
        y='R squared',
        col='data type')

read.csv(paste0("data/workingData/SLR_summary_model_run1_train_break_noxy", 2009,".csv"))
