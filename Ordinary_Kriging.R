#################################################
##Spatial Interpolation with Kriging

# Define the trend model
f.0 <- as.formula(MaxTemp ~ 1) 

#Create variogram
var.smpl <- variogram(f.0, shp2, cloud = FALSE) 
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(model="Sph", nugget = 0, psill = 10, 
                              range = 400))
plot(var.smpl, dat.fit)


# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige(f.0, shp2, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, bc)

# Plot the map
tm_shape(RASTER) + 
  tm_raster(n=10, palette="RdBu",  
            title="MaxTemp") +
  tm_shape(POINTS) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#Plot Variance Map
r   <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, bc)

tm_shape(RASTER) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map") +tm_shape(POINTS) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#Plot 95% CI Map
r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, bc)

tm_shape(RASTER) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map") +tm_shape(POINTS) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)