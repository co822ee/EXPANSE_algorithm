finalGWR <- function(sp_train, grd2, dm=DM, nngb, output_new){
   # Build GWR
   eq <- as.formula(paste0('obs~',  paste(output_new$variables, collapse = "+")))
   
   
   gwr.res.t <- gwr.basic(eq,
                          data=sp_train, 
                          regression.points=grd2, 
                          adaptive = T,
                          bw=nngb, 
                          dMat=dm,
                          kernel='exponential')
   
   gwr.res.t
}

