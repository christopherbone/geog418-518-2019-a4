#################################################
##Prepare Pollution Data
library(rgdal)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(tmap)
library(gstat)
library(raster)    # Used to clip out thiessen polygons
library(sp)


dir <- ""
setwd()

#DATASET 1
#Read the pollution csv dataset.
ozone = read.csv("", header = T, sep = ",")

#DATASET 2
#Read the monitoring station spatial dataset as an OGR data object.
monitor = readOGR(dsn = , layer = )

#Extract the monitoring stations for the South Coast (SC)
SC.monitor = monitor[monitor$AIRBASIN %in% c(),]

#Reproject the data to a suitable projection. Here we use a UTM projection because of the scale of the analysis. 
SC.monitor.t = spTransform(SC.monitor, CRS("+init=epsg:26911"))


#DATASET 3
#Read the California Air Basin spatial dataset.
Ca.AirBasin = readOGR(dsn = , layer = )

#Extract the South Coast air basin from the spatial dataset. 
SC.AirBasin = Ca.AirBasin[Ca.AirBasin$NAME %in% c(),] 

#Reproject the South Coast air basin spatial dataset to match the projeciton of the monitoring station dataset.  
SC.AirBasin.t = spTransform(SC.AirBasin, CRS("+init=epsg:26911"))


##################################################################
##Process Pollution Data
#You need to represent each location with a single value in order to perform statistical analyses.

#Examine the first several rows of the ozone dataset. 
head(ozone)

#Looking at the date and hour columns, you can see that we need to process the data
#to get summary statistics.

#Calculate the mean and max ozone level for each site for all readings.
mean.ozone = aggregate(value ~ site, ozone, mean)
max.ozone = aggregate(value ~ site, ozone, max)

#Join the mean and max ozone values to their respective monitoring stations. In doing so, you will need to rename the 
#first column of the monitoring data to site in order to have a unique name to match the two datasets.
names(SC.monitor.t)[1] ="site"  

#Merge the the monitoring station shapefile with the ozone data using the site column.  
mrg.tab.mean <- sp::merge(SC.monitor.t, mean.ozone, by = , all.x = FALSE) 
mrg.tab.max <- sp::merge(SC.monitor.t, max.ozone, by = , all.x = FALSE)

#Create a max and a mean spatialPointDataFrame. 
ozone.mean.spdf = na.omit(mrg.tab.mean)
ozone.max.spdf = na.omit(mrg.tab.max)

# Load and observe ozone data
tm_shape(POLYGON) + 
  tm_polygons() +
  tm_shape(POINTS) +
  tm_dots(col="value", palette = "RdBu", 
          title="Sampled Ozone \n(in ppm)", size=0.7) + 
  tm_legend(legend.outside=TRUE)
