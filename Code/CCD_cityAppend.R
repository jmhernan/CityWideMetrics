########################
# CCD AND CENSUS SHAPEFILES NEEDED
# JOSE M HERNANDEZ
# 10/17/16
# Updated 1/26/2017
# New Jersey 
########################
library(sp)
library(rgdal)
library(maps)
library(maptools)
library(foreign)
library(dplyr)

rm(list=ls())

## STEP1: Get your data ready
data <- readRDS("~/Google Drive/CRPE/ccd/ccd_2010_15.RData")

##We only want NJ 
NJ_data <- data.frame(filter(data, fipst==34))

## STEP 2: Load geographic data
#read in Census Places polygons

NJ <- readOGR("/Users/crpeadmin/Google Drive/CRPE/shapefiles/shp_NJ", "tl_2011_34_place")

#combine all states' shapefiles 
summary(NJ$NAME)
plot(NJ)
##first, need to change the polygon IDs so that they are not duplicated across shapefile sets
NJ1 <- spChFIDs(NJ, as.character(NJ$GEOID))

## STEP 3: Attach city onto state data
#set missing lon/lat to 0, & set lon/lat to coordinates
NJ_data$latcod[is.na(NJ_data$latcod)] <- 0
NJ_data$loncod[is.na(NJ_data$loncod)] <- 0
coordinates(NJ_data) <- c("loncod", "latcod")

#tell R that school coordinates are in the same lat/long reference system as the places data
proj4string(NJ_data) <- proj4string(NJ1)

#combine is.na() with over() to do the containment test (note that we need to "demote" places to a SpatialPolygons object first)
inside.place <- !is.na(over(NJ_data, as(NJ1, "SpatialPolygons")))

#use "over" again, this time with places as a SpatialPolygonsDataFrame object, to determine which places (if any) contains each school, and store the place name as attribute of the schools data
NJ_data$city <- rep(NA, nrow(NJ_data))
NJ_data$city <- over(NJ_data, NJ1)$NAMELSAD

#write the augmented state dataset to new .dta file
schools <- as.data.frame(NJ_data)
####
saveRDS(schools, "~/Google Drive/CRPE/ccd/ccd_2010_2015_city.RData")
####
