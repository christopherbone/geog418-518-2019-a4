#################################################
##Spatial Interpolation with IDW
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(shp2, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

proj4string(grd) <- proj4string(shp2)
proj4string(shp2) <- proj4string(grd)

#IDW Interpolation
P.idw <- gstat::idw(MaxTemp ~ 1, shp2, newdata=grd, idp=1)
r       <- raster(P.idw)
r.m     <- mask(r, bc)

tm_shape(RASTER) + 
  tm_raster(n=10,palette = "RdBu",
            title="TITLE") + 
  tm_shape(shp2) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#################################################
# Leave-one-out validation routine
IDW.out <- vector(length = length(shp2))
for (i in 1:length(shp2)) {
  IDW.out[i] <- idw(MaxTemp ~ 1, shp2[-i,], shp2[i,], idp=0.5)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ shp2$MaxTemp, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ shp2$MaxTemp), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
text(35, 20, (paste0("RMSE: ", round(sqrt( sum((IDW.out - shp2$MaxTemp)^2) / length(shp2)), 3))))



#################################################
# Implementation of a jackknife technique to estimate a confidence interval at each unsampled point.
# Create the interpolated surface
img <- gstat::idw(MaxTemp~1, shp2, newdata=grd, idp=2.0)
n   <- length(shp2)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(MaxTemp~1, shp2[-i,], newdata=grd, idp=2.0)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Southern California
r <- raster(img.sig, layer="v")
r.m <- mask(r, bc)

# Plot the map
tm_shape(RASTER) + tm_raster(n=7,title="TITLE") +
  tm_shape(POINTS) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)