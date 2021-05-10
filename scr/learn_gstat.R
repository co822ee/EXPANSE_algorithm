# https://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf
# The meuse data set: a tutorial for the gstat R package
library(gstat)
library(sp)
data("meuse")
class(meuse)
names(meuse)
coordinates(meuse) = ~x+y
# proj4string(meuse) = CRS("+init=epsg:28992")
class(meuse)
plot(meuse)
bubble(meuse, "zinc")

data("meuse.grid")
summary(meuse.grid)
coordinates(meuse.grid) = ~x+y
class(meuse.grid)
gridded(meuse.grid)=T
class(meuse.grid)
image(meuse.grid["dist"])
title("distance to river (red = 0)")

zinc.idw = idw(zinc~1, meuse, meuse.grid)
summary(zinc.idw)
image(zinc.idw["var1.pred"])
spplot(zinc.idw["var1.pred"], main = "zinc inverse distance weighted interpolations")
spplot(meuse.grid["dist"], main = "distance to river")

hist(meuse$zinc)
hist(log(meuse$zinc))
plot(log(zinc)~sqrt(dist), meuse)
abline(lm(log(zinc)~sqrt(dist), meuse))

# Calculate variogram
# 1) assume a constant trend for the variable log(zinc)
# --> the variogram of the sample/data is calculated 
# (ordinary kriging)
lzn.vgm = variogram(log(zinc)~1, meuse)
lzn.vgm
class(lzn.vgm)
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(1, "Sph", 1500, 1))
plot(lzn.vgm, lzn.fit)
fit.variogram(lzn.vgm, model=vgm(1, "Exp", 900, 1))

# 2) specify a mean function, eg using sqrt(dist) as a predictor variable
# --> the variogram of the residuals is instead calculated when a linear model is given.
# (regression kriging; universal kriging)
lznr.vgm = variogram(log(zinc)~sqrt(dist), meuse)
lznr.fit = fit.variogram(lznr.vgm, model=vgm(1, "Exp", 300, 1))
plot(lznr.vgm, lznr.fit)
plot(lznr.vgm, fit.variogram(lznr.vgm, model=vgm(1, "Sph", 300, 1)))

# Kriging
# (ordinary kriging)
lzn.kriged = krige(log(zinc)~1, meuse, meuse.grid, model = lzn.fit)
lzn.condsim = krige(log(zinc)~1, meuse, meuse.grid, model = lzn.fit, nsim=4, nmax=50)
summary(lzn.kriged)
summary(lzn.condsim)
spplot(lzn.kriged["var1.pred"])
spplot(lzn.kriged["var1.var"])
spplot(lzn.condsim)
spplot(cbind(lzn.condsim, lzn.kriged["var1.pred"]))

# (universal kriging)
lznr.kriged = krige(log(zinc)~sqrt(dist), meuse, meuse.grid, model = lznr.fit)
spplot(lznr.kriged["var1.pred"])
spplot(lznr.kriged["var1.var"])

# Directional variograms
lzn.dir = variogram(log(zinc)~1, meuse, alpha = c(0, 45, 90, 135))
lzndir.fit = vgm(.59, "Sph", 1200, .05, anis = c(45, .4))
plot(lzn.dir, lzndir.fit, as.table = TRUE)
