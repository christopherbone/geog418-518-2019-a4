#################################################
##Prepare Temperature Data
library(lubridate)
library(dplyr)
library(rgdal)
library(spatialEco)
library(bcmaps)
library(tmap)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
library(gstat)

dir <- ""
setwd()

#DATASET 1
#Extact EC Files
ecList <- list.files(path = "./Data/EC", pattern = "*.ascii$")

#Loop through and combine data
for(i in 1:length(ecList)){
  StationName <- substr(ecList[[i]], 1, nchar(ecList[[i]])-6) 
  data <- read.csv(paste(dir,"/Data/EC/", ecList[[i]], sep = ""), skip = 1, header = TRUE)
  
  if(nrow(data) > 0){
    data$Station <- StationName
    dataSubset <- data[,c("Station","time", "MAX_TEMP")]
    
    colnames(dataSubset) <- c("Station", "Date", "MaxTemp")
    dataSubset$MaxTemp <- as.numeric(dataSubset$MaxTemp)  
  } else{
    dataSubset <- data.frame(Station = StationName, 
                             Date = NA, 
                             MaxTemp = NA)
  }
  
  if(i == 1){
    ECResult <- dataSubset
  }else {
    ECResult <- rbind(ECResult, dataSubset)
  }
}

#DATASET 2
#Read in shp file
shp <- readOGR(dsn = "./Data", layer = "")


#CombineDatasets
ECResult$Dte <- as_date(ECResult$Date)
ECResult$Month <- month(ECResult$A COLUMN FOR DATE)

#Subset Month
ECMonth <- subset(ECResult, ECResult$Month == YOUR ASSIGNED MONTH)

# Summarize by max temp
csvDataSum <- ECMonth %>%
  group_by(Station) %>% 
  summarize(MaxTemp = max(MaxTemp, na.rm = TRUE))
  
#Filter for cold -90 and hot 57
csvDataSum <- subset(csvDataSum, csvDataSum$MaxTemp > REALISTIC LOW TEMP & csvDataSum$MaxTemp < REALISTIC HIGH TEMP)

#Join
shp2 <- merge(shp, csvDataSum,
             by.x = , 
             by.y = )

#Reproject
shp2 <- spTransform(shp2, CRS("+init=epsg:FIND EPSG FOR NAD83 BC ALBERS"))

#Trim data
shp2@data <- shp2@data[,c(1,21)]

#Remove NA
shp2 <- sp.na.omit(shp2)

#Get BC shape
bc <- as_Spatial(bc_neighbours()) #Get shp of BC bounds
bc <- bc[which(bc$name == "British Columbia" ),]

# Load and observe temp data
tm_shape(POLYGONS) + 
  tm_polygons(col = "gray50") +
  tm_shape(POINTS) +
  tm_dots(col="COLUMN NAME", palette = "RdBu", 
          title="LEGEND TITLE", size=0.7) + 
  tm_legend(legend.outside=TRUE)
