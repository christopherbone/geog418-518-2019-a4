#################################################
##Spatial Interpolation with Thiessen Polygons
# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(shp2)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(shp2)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, shp2, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)

# Finally, we'll clip the tessellated  surface to the South Coast Air Basin boundaries
th.clp   <- raster::intersect(bc,th.spdf)

# Map the data
tm_shape(POLYGONS) + 
  tm_polygons(col="MaxTemp", palette="RdBu",
              title="TITLE OF LEGEND") +
  tm_shape(shp2) +
  tm_dots(col = "black") +
  tm_legend(legend.outside=TRUE)

