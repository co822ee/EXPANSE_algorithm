gwr_stepwise <- function(x_var, kernel_type, sp_train, df_train, grd, DM, calibr_nngb, csv_name_fold){
   
   output <- data.frame(variables=0, increR2=0)
   library(doParallel)
   library(foreach)
   cl <- parallel::makeCluster(5)
   doParallel::registerDoParallel(cl)
   R2_l <- foreach(i=seq_along(x_var)) %dopar% {
      source("scr/fun_call_lib.R")
      R2 <- vector("numeric", length=length(x_var))
      DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                      rp.locat=coordinates(sp_train))
      
      # stepwise
      x_var <- names(data.frame(sp_train) %>% dplyr::select(matches(x_var)))
      eq_gwr <- as.formula(paste0("obs~", x_var[i]))
      
      # Optimize nngb (adaptive bandwidth size)
      if(calibr_nngb){
         nngb <- bw.gwr(eq_gwr, data=sp_train, approach = "CV", kernel = kernel_type,
                        adaptive = T, dMat = DM_1)
         print(paste0("nngb: ", nngb))
      }else{
         nngb <- read.table(paste0("data/workingData/GWR_nngb_",
                                   csv_name_fold,".txt"))[,1]
      }
      gwr_m <- tryCatch(gwr.basic(eq_gwr,
                                  data=sp_train,
                                  regression.points=grd,
                                  adaptive = T,
                                  bw=nngb,
                                  dMat=DM,
                                  kernel=kernel_type), 
                        error=function(e) T)
      if(typeof(gwr_m)!='logical'){
         coef_stack <- stack(gwr_m$SDF)
         if(!any(minValue(coef_stack)[-1]<0)){  #coef surface values should be larger than zero
            source("scr/fun_gen_df_gwr.R")
            gwr_pred <- gen_df_gwr(coef_stack, sp_train, as.data.frame(sp_train), T)
            R2[i] <- error_matrix(as.data.frame(sp_train)$obs, gwr_pred)[8]  #explained_var
            # error_matrix(as.data.frame(sp_train)$obs, as.numeric(predict(lm(eq_gwr, as.data.frame(sp_train)))))
            # summary(lm(eq_gwr, as.data.frame(sp_train)))$adj.r.squared
            # summary(lm(eq_gwr, as.data.frame(sp_train)))$r.squared # explained_var  in error_matrix
         }
      }
      R2[i]
   }
   parallel::stopCluster(cl)
   R2 <- unlist(R2_l)
   x_highest <- x_var[which.max(R2)]
   R2_highest <- max(R2)
   step_i <- 1
   output[step_i,] <- cbind(variables=x_highest, increR2=R2_highest)
   output
   #---step2-----
   # step_i=2
   #for
   step_i=2
   while(step_i<=10){  #(as.numeric(output[step_i,2])-as.numeric(output[step_i-1,2]))>=0.01
      print(step_i)
      x_var_new <- x_var[!(x_var%in%output$variables[1:(step_i-1)])]
      print(length(x_var_new))
      # write.table(x_var_new, "data/workingData/stepGWR_", step_i)
      R2 <- vector("numeric", length=length(x_var))
      
      cl <- parallel::makeCluster(5)
      doParallel::registerDoParallel(cl)
      R2_l <- foreach(i=seq_along(x_var_new)) %dopar% {
         # This script is for doing stepwise selection for gwr
         source("scr/fun_call_lib.R")
         DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                         rp.locat=coordinates(sp_train))
         # stepwise
         
         eq_gwr <- as.formula(paste0("obs~", paste(c(output$variables, x_var_new[i]), collapse = "+")))
         # Optimize nngb (adaptive bandwidth)
         if(calibr_nngb){
            nngb <- bw.gwr(eq_gwr, data=sp_train, approach = "CV", kernel = kernel_type,
                           adaptive = T, dMat = DM_1)
            print(paste0("nngb: ", nngb))
         }else{
            nngb <- read.table(paste0("data/workingData/GWR_nngb_run1_train_break_noxy",
                                      year_target,".txt"))[,1]
         }
         gwr_m <- tryCatch(gwr.basic(eq_gwr,
                                     data=sp_train,
                                     regression.points=grd,
                                     adaptive = T,
                                     bw=nngb,
                                     dMat=DM,
                                     kernel=kernel_type), 
                           error=function(e) T)
         if(typeof(gwr_m)!='logical'){
            coef_stack <- stack(gwr_m$SDF)
            if(!any(minValue(coef_stack)[-1]<0)){ #coef surface values should be larger than zero
               source("scr/fun_gen_df_gwr.R")
               gwr_pred <- gen_df_gwr(coef_stack, sp_train, as.data.frame(sp_train), T)
               R2[i] <- error_matrix(as.data.frame(sp_train)$obs, gwr_pred)[8]  #explained_var
               # error_matrix(as.data.frame(sp_train)$obs, as.numeric(predict(lm(eq_gwr, as.data.frame(sp_train)))))
               # summary(lm(eq_gwr, as.data.frame(sp_train)))$adj.r.squared
               # summary(lm(eq_gwr, as.data.frame(sp_train)))$r.squared # explained_var  in error_matrix
            }
         }
         R2[i]
      }
      parallel::stopCluster(cl)
      R2 <- unlist(R2_l)
      x_highest <- x_var_new[which.max(R2)]
      R2_highest <- max(R2)
      if(R2_highest!=0){
         output[step_i,] <- cbind(variables=x_highest, increR2=R2_highest)
         print(output)
         step_i <- step_i+1
      }else{
         break
      }
   }
   output$increR2 <- as.numeric(output$increR2)
   print(paste0("output data/workingData/stepGWR_", 
                csv_name_fold, ".txt"))
   write.table(output, paste0("data/workingData/stepGWR_", 
                              csv_name_fold, ".txt"), row.names = F)
   
   #---- Exclusion: p-value larger than 0.1 (global)-----
   eq <- as.formula(paste0("obs~", paste(output$variables, collapse = "+")))
   # DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
   #                 rp.locat=coordinates(sp_train))
   # nngb <- bw.gwr(eq, data=sp_train, approach = "CV", kernel = kernel_type,
   #                adaptive = T, dMat = DM_1)
   # 
   # print(paste0("nngb: ", nngb))
   # gwr_model <- tryCatch(gwr.basic(eq, sp_train,
   #                                 adaptive = T, bw=nngb, kernel=kernel_type, 
   #                                 F123.test = TRUE),
   #                       error=function(e) T)
   # if(typeof(gwr_m)!='logical'){
   # }else{
   #    
   # }
   p_value <- summary(lm(eq, df_train))$coefficients[-1,4]  #exclude intercept
   if(any(p_value>0.1)){
      i=1
      while(i <= length(p_value)){
         if(any(p_value>0.1)){
            output_new <- output[-(which.max(p_value)),]
            eq <- as.formula(paste0("obs~", paste(output_new$variables, collapse = "+")))
            p_value <- summary(lm(eq, df_train)$coefficients[-1, 4])
         }else{
            break
         }
      }
   }else{
      output_new <- output
   }
   #---- Exclusion: vif larger than 3 (global)-----
   eq <- as.formula(paste0('obs~', paste(output_new$variables, collapse = "+")))
   lm_model <- lm(eq, df_train)
   
   vif_v <- vif(lm_model)
   if(any(vif_v>3)){
      i=1
      while(i <= length(vif_v)){
         if(any(vif_v>3)){
            output_new <- output[-(which.max(vif_v)),]
            eq <- as.formula(paste0("obs~", paste(output_new$variables, collapse = "+")))
            vif_v <- vif(lm(eq, df_train))
         }else{
            break
         }
      }
   }else{
      output_new <- output
   }
   print(paste0("output_new data/workingData/stepGWR_", 
                csv_name_fold, ".txt"))
   write.table(output_new, paste0("data/workingData/stepGWR_", 
                              csv_name_fold, ".txt"), row.names = F)
   
   output_new
}
