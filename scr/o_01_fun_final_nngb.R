final_nngb <- function(sp_train, output_new, write_output=T){
   eq <- as.formula(paste0('obs~',  paste(output_new$variables, collapse = "+")))
   
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