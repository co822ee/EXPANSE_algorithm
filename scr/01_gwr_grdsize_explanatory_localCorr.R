# This script uses gwr local statistics to demonstrate the local variations (especially for no2_10MACC)
data("DubVoter")
library(gridExtra)
library(RColorBrewer)
mypalette.1 <- viridis(5)
gw.ss.bx <- gwss(Dub.voter, vars = c("GenEl2004", "LARent", "Unempl"),
                 kernel = "boxcar", adaptive = TRUE, bw = 48, quantile = TRUE)
gw.ss.bs <- gwss(Dub.voter,vars = c("GenEl2004", "LARent", "Unempl"),
                 kernel = "bisquare", adaptive = TRUE, bw = 48)

gw.ss.bx2 <- gwss(sp_train, vars = c(slr$variables[-1], 'obs'), kernel = 'gaussian', 
                 adaptive = T, bw=nngb)
gw.ss.bx3 <- gwss(sp_train, vars = c(slr$variables[-1], 'obs'), kernel = 'gaussian', 
                 adaptive = T, bw=700) #40%
gw.ss.bx4 <- gwss(sp_train, vars = c(slr$variables[-1], 'obs'), kernel = 'gaussian', 
                 adaptive = T, bw=260) #15%
spplot(gw.ss.bx2$SDF, "no2_10MACC_LSD", col.regions=mypalette.1, key.space='right')
spplot(gw.ss.bx2$SDF, "no2_10MACC_LM")
spplot(gw.ss.bx2$SDF, "ROADS_EU_20p_LSD")
spplot(gw.ss.bx2$SDF, "ROADS_EU_20p_LM")
spplot(gw.ss.bx2$SDF, "obs_LSD")
spplot(gw.ss.bx2$SDF, "obs_LM")

# The correlation between no2_10MACC and obs is locally varied
library(classInt)

# breaks.qt <- classIntervals(data.frame(gw.ss.bx2$SDF['Corr_ROADS_EU_20p.obs'])$Corr_ROADS_EU_20p.obs, n = 6, style = "quantile", intervalClosure = "right")

# spplot(gw.ss.bx2$SDF, "Corr_ROADS_EU_20p.obs", col.regions=mypalette.1, at = seq(from=0, to=0.8, by=0.1), 
#        key.space='right', at=breaks.qt$brks)
plot_cor <- function(layer_name, raster_stack, palette_region=mypalette.1){
   spplot(raster_stack$SDF, layer_name, main=layer_name, col.regions=palette_region,
          key.space='right')
}
plots <- lapply(paste0("Corr_", slr$variables[-1], ".obs"), plot_cor, 
                raster_stack=gw.ss.bx2, palette_region=mypalette.1)
do.call(grid.arrange, plots)
plots_spearman <- lapply(paste0("Spearman_rho_", slr$variables[-1], ".obs"), plot_cor, 
                raster_stack=gw.ss.bx2, palette_region=mypalette.1)
do.call(grid.arrange, plots_spearman)

plots <- lapply(paste0("Corr_", slr$variables[-1], ".obs"), plot_cor, 
                raster_stack=gw.ss.bx3, palette_region=mypalette.1)
do.call(grid.arrange, plots)


plots <- lapply(paste0("Corr_", slr$variables[-1], ".obs"), plot_cor, 
                raster_stack=gw.ss.bx4, palette_region=mypalette.1)
do.call(grid.arrange, plots)



# This shows that no2_10MACC could be locally varying (the accuracy of the CTM could vary in space as well)

gw.ss.bs <- gwss(sp_train, vars = slr$variables[-1], kernel = kernel_type[i], 
                 adaptive = T, bw=nngb)


gw.ss.bs2 <- gwss(sp_train, vars = slr$variables[-1], kernel = kernel_type[i], 
                  adaptive = F, bw=bandwidth_calibr)
