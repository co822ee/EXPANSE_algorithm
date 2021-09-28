calibr_gwr <- function(sp_train, slr_csvname, write_output=T, obs_varname='obs'){
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_", slr_csvname, '.csv'))
   eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
   
   # Calibrate bandwidth using CV
   DM_1 <- gw.dist(dp.locat=coordinates(sp_train),
                   rp.locat=coordinates(sp_train))
   nngb <- bw.gwr(eq, data=sp_train, approach = "CV",
                  kernel = "exponential", adaptive = T, dMat = DM_1)
   if(write_output){
      print(paste0("output data/workingData/GWR_nngb_", slr_csvname, ".txt"))
      write.table(nngb, paste0("data/workingData/GWR_nngb_", slr_csvname, ".txt"))   
   }
   nngb
}