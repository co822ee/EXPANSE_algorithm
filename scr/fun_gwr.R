gwr <- function(sp_train, grd2, dm=DM, nngb, slr_csvname, obs_varname='obs'){
   # Build GWR
   slr <- read.csv(paste0("data/workingData/SLR_summary_model_", slr_csvname, '.csv'))
   eq <- as.formula(paste0(obs_varname, '~',  paste(slr$variables[-1], collapse = "+")))
   
   
   gwr.res.t <- gwr.basic(eq,
                          data=sp_train, 
                          regression.points=grd2, 
                          adaptive = T,
                          bw=nngb, 
                          dMat=dm,
                          kernel='exponential')
   
   gwr.res.t
}

