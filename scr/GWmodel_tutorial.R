# https://rpubs.com/chrisbrunsdon/101305
library(GWmodel)
# With GWR, we can test whether the relationship between predictors and the outcome
# variable vary with space (spatial heterogeneity; changing geographically).


data(LondonHP)
head(data.frame(londonhp))
# Price per square meter
londonhp$PPSQM <- londonhp$PURCHASE / londonhp$FLOORSZ
# Prcie per square meter vs. professional proportion
linmod <- lm(PPSQM~PROF,data=londonhp) # Store the regression model to use in a plot later
summary(linmod)
plot(PPSQM~PROF,data=londonhp,xlab='Proportion Professional/Managerial',ylab='Cost per Square Metre')
abline(linmod)
# Is this linear relationship the same everywhere in London area?
panel.lm <- function(x,y,...) {
   points(x, y, pch=16)
   abline(lm(y~x))
}
#the data area divided into subsets on the basis of their locations
coplot(PPSQM~PROF|coords.x1*coords.x2,data=data.frame(londonhp),
       panel=panel.lm, overlap=0.8)


# GWR
data(LondonBorough)
plot(londonborough)
plot(londonhp, pch=16, col='firebrick',add=TRUE)
# londonhp data are clustered
# specify the grid topology with the following parameters:
# - the smallest coordinates for each dimension, here: 0,0
# - cell size in each dimension, here: 1,1 
# - number of cells in each dimension, here: 5,5
xmin <- londonborough@bbox[1, 1]
ymin <- londonborough@bbox[2, 1]
xmax <- londonborough@bbox[1, 2]
ymax <- londonborough@bbox[2, 2]
cellsize <- 2000
grd <- SpatialGrid(GridTopology(c(503400,155400),c(1000,1000),c(60,48)))
grd2 <- SpatialGrid(GridTopology(c(xmin,ymin),
                                 c(cellsize,cellsize),
                                 c(floor((xmax-xmin)/cellsize)+2,floor((ymax-ymin)/cellsize)+2)))
plot(grd)
plot(londonborough,add=TRUE,col=adjustcolor('navyblue',alpha.f=0.5))
plot(grd2, add=T, col='green')
plot(londonhp, pch=16, col='firebrick',add=TRUE)
# distance between regression points and data points
DM <- gw.dist(dp.locat=coordinates(londonhp),rp.locat=coordinates(grd2))
# basic GWR analysis
londonhp$PPSQM <- scale(londonhp$PPSQM)
londonhp$PROF <- scale(londonhp$PROF)
gwr.res <- gwr.basic(PPSQM~PROF, data=londonhp, regression.points=grd2, bw=10000, 
                     dMat=DM,kernel='gaussian')
# bw can be chosen ‘automatically’ by cross-validation. 
# kernel: the functional form of the kernel (the weighting applied in the window) - here it is Gaussian

#(the Spatial* object with the GWR results is in gwr.res$SDF - here the object is a SpatialPixelsDataFrame)
image(gwr.res$SDF,'PROF')
image(gwr.res$SDF,'Intercept')
contour(gwr.res$SDF,'Intercept',lwd=3,add=TRUE)
plot(londonborough,add=TRUE)
plot(londonhp,add=TRUE,pch=16,col='blueviolet')
# plot(grd, add=T, col='grey', alpha=0.5)
plot(grd2, add=T, col='green')
image(gwr.res$SDF)
