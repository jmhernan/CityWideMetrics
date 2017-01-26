#################
#Performance last step 
#Jose Hernandez
#1/25/17
#################
library(tidyr)
library(dplyr)
##Perfromance data should be a long file of schools by year.  We use the performance.  

rm(list=ls())
file <- "~/Google Drive/CRPE/CityWideMetrics/CleanedData/New Jersey/CleanedNJParc.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data)


wdata <- reshape(data, 
                 timevar = "SUBJECT",
                 idvar = c("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","COUNTY_NAME","DISTRICT_NAME","SCHOOL_NAME"),
                 direction = "wide")

names(wdata)

#get relevant features
njData15 <- wdata[,c(1:6,14,22)]
#addyear
njData15$year <- 2015 
names(njData15)

#rename and reorder variables 
names(njData15) <- c("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","COUNTY_NAME","DISTRICT_NAME","SCHOOL_NAME",
                     "prfadv.read","prfadv.math","year")

#reorder so that columns match the combined file 
njData15 <- njData15[,c("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","year","COUNTY_NAME","DISTRICT_NAME","SCHOOL_NAME",
                        "prfadv.math","prfadv.read")]

#Append to previous years data
file2 <- "~/Google Drive/CRPE/CityWideMetrics/CleanedData/New Jersey/NJ Performance.csv" 
data2 <- read.csv(file2, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data2)
#We want to combine Proficient and Advanced 

data2$prfadv.math <- data2$ALL.PROFICIENT.math + data2$ALL.ADVANCED.math
data2$prfadv.read <- data2$ALL.PROFICIENT.ELA + data2$ALL.ADVANCED.ELA

#get what we need 
njDataAll <- data2[,c(2:8,69,70)]
#names have to match 
names(njDataAll) <- c("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","year","COUNTY_NAME",
                      "DISTRICT_NAME","SCHOOL_NAME","prfadv.math","prfadv.read") 
names(njDataAll)

#append using "rbind" this will work ONLY if the column names match! 

njPerf <- rbind(njDataAll,njData15)
#THIS IS WHAT A performance CLEAN DATASET SHOULD LOOK LIKE...
#write.csv(njPerf, file = "~/Google Drive/CRPE/CityWideMetrics/CleanedData/New Jersey/CleanedNJParc12_15.csv") #csv
saveRDS(njPerf, file="~/Google Drive/CRPE/CityWideMetrics/CleanedData/New Jersey/CleanedNJParc12_15.RData") #Rdata 
