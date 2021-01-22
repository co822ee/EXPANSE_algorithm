image(gwr.res.t$SDF,'no2_10MACC', main="no2_10MACC")
# image(gwr.res.t$SDF,'Intercept')
plot(eu_bnd[1],add=TRUE,pch=16,col='transparent',border='grey', alpha=0.05)
contour(gwr.res.t$SDF,'no2_10MACC',lwd=2, labcex=1.1,add=TRUE)
par(mfrow=c(3,4))
plot_gwr_coef <- function(i){
   image(gwr.res.t$SDF,names(gwr.res.t$SDF)[i], main=names(gwr.res.t$SDF)[i])
   # image(gwr.res.t$SDF,'Intercept')
   plot(eu_bnd[1],add=TRUE,pch=16,col='transparent',border='grey', alpha=0.05)
   contour(gwr.res.t$SDF,names(gwr.res.t$SDF)[i],lwd=1, labcex=1.1,add=TRUE)
}
lapply(seq_along(names(gwr.res.t$SDF)), plot_gwr_coef)
# method 2
coef_stack <- stack(gwr.res.t$SDF)
plot(coef_stack)

tm_shape(gwr.res.t$SDF['Intercept'])+
   tm_raster(n=10,palette = "plasma", auto.palette.mapping = FALSE,
             title="OMI")+
   tm_shape(eu_bnd)+tm_borders(col = "white")
