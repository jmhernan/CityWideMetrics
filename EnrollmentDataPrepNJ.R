#########################
#Enrollment Data Prep NJ
#Jose Hernandez
#1/25/17
#########################
#########################
##ENROLLMENT DATA 
##THERE IS A FILE PER YEAR
##WE NEED A LONG FILE OF SCHOOL BY YEAR WITH SUBGROUP PROPORTIONS
#########################
library(tidyr)
library(dplyr)

rm(list=ls())

file <- "~/Google Drive/CRPE/CityWideMetrics/RawData/NJ/Enrollment/2010_11.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data)
# we only want totals CHECK CODEBOOK FOR RIGHT CODE and subset 

data_11 <- subset(data, PRGCODE == "55"  & School.Name != "District Total" & School.Name != "County Total" )
#create totals 
data_11$white <- data_11$WH_F + data_11$WH_M 
data_11$black <- data_11$BL_F + data_11$BL_M
data_11$hisp <- data_11$HI_F + data_11$HI_M
data_11$asian <- data_11$AS_F + data_11$AS_M
data_11$native <- data_11$AM_F + data_11$AM_M
data_11$hw_native <- data_11$PI_F + data_11$PI_M
data_11$two_more <- data_11$MU_F + data_11$MU_M

#Select your variables 
data_11 <- data_11[, c( "Co.code",     "County.Name",   "DIST_ID",      
                        "District.Name", "SCHOOL_ID",     "School.Name",  
                        "ROW_TOTAL",     "FREE_LUNCH",    "REDUCED_LUNCH",
                        "LEP",           "MIGRANT",          
                        "white",         "black",         "hisp",         
                        "asian",         "native",        "hw_native",
                        "two_more")]
#rename using lowercases and  names
names(data_11) <- c("county_id",     "county_name",   "district_id",      
                    "district_name", "school_id",     "school_name",  
                    "row_total",     "free_lunch",    "reduced_lunch",
                    "lep",           "migrant",             
                    "white",         "black",         "hisp",         
                    "asian",         "native",        "hw_native",
                    "two_more")
#use most recent to create year variable 
data_11$year <- 2011
##########
#new year 
##########

file <- "~/Google Drive/CRPE/CityWideMetrics/RawData/NJ/Enrollment/2011_12.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data)
# we only want totals CHECK CODEBOOK FOR RIGHT CODE and subset 

data_12 <- subset(data, PRGCODE == "55"  & School.Name != "District Total" & School.Name != "County Total" )
#create totals 
data_12$white <- data_12$WH_F + data_12$WH_M 
data_12$black <- data_12$BL_F + data_12$BL_M
data_12$hisp <- data_12$HI_F + data_12$HI_M
data_12$asian <- data_12$AS_F + data_12$AS_M
data_12$native <- data_12$AM_F + data_12$AM_M
data_12$hw_native <- data_12$PI_F + data_12$PI_M
data_12$two_more <- data_12$MU_F + data_12$MU_M

#Select your variables 
data_12 <- data_12[, c( "COUNTY_ID",     "County.Name",   "DIST_ID",      
                      "District.Name", "SCHOOL_ID",     "School.Name",  
                      "ROW_TOTAL",     "FREE_LUNCH",    "REDUCED_LUNCH",
                      "LEP",           "MIGRANT",          
                      "white",         "black",         "hisp",         
                      "asian",         "native",        "hw_native",
                      "two_more")]
#rename using lowercases and  names
names(data_12) <- c("county_id",     "county_name",   "district_id",      
                   "district_name", "school_id",     "school_name",  
                   "row_total",     "free_lunch",    "reduced_lunch",
                   "lep",           "migrant",             
                   "white",         "black",         "hisp",         
                   "asian",         "native",        "hw_native",
                   "two_more")
#use most recent to create year variable 
data_12$year <- 2012
##########
#new year 
##########
file <- "~/Google Drive/CRPE/CityWideMetrics/RawData/NJ/Enrollment/2012_13.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data)
# we only want totals CHECK CODEBOOK FOR RIGHT CODE and subset 

data_13 <- subset(data, PRGCODE == "55"  & SCHOOL.NAME != "District Total" & SCHOOL.NAME != "County Total" )
#create totals 
data_13$white <- data_13$WH_F + data_13$WH_M 
data_13$black <- data_13$BL_F + data_13$BL_M
data_13$hisp <- data_13$HI_F + data_13$HI_M
data_13$asian <- data_13$AS_F + data_13$AS_M
data_13$native <- data_13$AM_F + data_13$AM_M
data_13$hw_native <- data_13$PI_F + data_13$PI_M
data_13$two_more <- data_13$MU_F + data_13$MU_M

#Select your variables 
data_13 <- data_13[, c( "COUNTY.CODE",     "COUNTY.NAME",   "DISTRICT.CODE",      
                        "DISTRICT.NAME", "SCHOOL.CODE",     "SCHOOL.NAME",  
                        "ROW_TOTAL",     "FREE_LUNCH",    "REDUCED_PRICE_LUNCH",
                        "LEP",           "MIGRANT",       
                        "white",         "black",         "hisp",         
                        "asian",         "native",        "hw_native",
                        "two_more")]
#rename using lowercases and  names
names(data_13) <- c("county_id",     "county_name",   "district_id",      
                    "district_name", "school_id",     "school_name",  
                    "row_total",     "free_lunch",    "reduced_lunch",
                    "lep",           "migrant",              
                    "white",         "black",         "hisp",         
                    "asian",         "native",        "hw_native",
                    "two_more")
#use most recent to create year variable 
data_13$year <- 2013
##########
#new year 
##########
file <- "~/Google Drive/CRPE/CityWideMetrics/RawData/NJ/Enrollment/2013_14.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data)
# we only want totals CHECK CODEBOOK FOR RIGHT CODE and subset 

data_14 <- subset(data, PRGCODE == "55"  & SCHOOL_NAME != "District Total" & SCHOOL_NAME != "County Total" )
#create totals 
data_14$white <- data_14$WH_F + data_14$WH_M 
data_14$black <- data_14$BL_F + data_14$BL_M
data_14$hisp <- data_14$HI_F + data_14$HI_M
data_14$asian <- data_14$AS_F + data_14$AS_M
data_14$native <- data_14$AM_F + data_14$AM_M
data_14$hw_native <- data_14$PI_F + data_14$PI_M
data_14$two_more <- data_14$MU_F + data_14$MU_M

#Select your variables 
data_14 <- data_14[, c( "COUNTY_ID",     "COUNTY_NAME",   "DIST_ID",      
                        "LEA_NAME", "SCHOOL_ID",     "SCHOOL_NAME",  
                        "ROW_TOTAL",     "FREE_LUNCH",    "REDUCED_PRICE_LUNCH",
                        "LEP",           "MIGRANT",       
                        "white",         "black",         "hisp",         
                        "asian",         "native",        "hw_native",
                        "two_more")]
#rename using lowercases and  names
names(data_14) <- c("county_id",     "county_name",   "district_id",      
                    "district_name", "school_id",     "school_name",  
                    "row_total",     "free_lunch",    "reduced_lunch",
                    "lep",           "migrant",              
                    "white",         "black",         "hisp",         
                    "asian",         "native",        "hw_native",
                    "two_more")
#use most recent to create year variable 
data_14$year <- 2014
##########
#new year 
##########
file <- "~/Google Drive/CRPE/CityWideMetrics/RawData/NJ/Enrollment/2014_15.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data)
# we only want totals CHECK CODEBOOK FOR RIGHT CODE and subset 

data_15 <- subset(data, PRGCODE == "55"  & SCHOOL_NAME != "District Total" & SCHOOL_NAME != "County Total" )
#create totals 
data_15$white <- data_15$WH_F + data_15$WH_M 
data_15$black <- data_15$BL_F + data_15$BL_M
data_15$hisp <- data_15$HI_F + data_15$HI_M
data_15$asian <- data_15$AS_F + data_15$AS_M
data_15$native <- data_15$AM_F + data_15$AM_M
data_15$hw_native <- data_15$PI_F + data_15$PI_M
data_15$two_more <- data_15$MU_F + data_15$MU_M

#Select your variables 
data_15 <- data_15[, c( "COUNTY_ID",     "COUNTY_NAME",   "DIST_ID",      
                        "LEA_NAME", "SCHOOL_ID",     "SCHOOL_NAME",  
                        "ROW_TOTAL",     "FREE_LUNCH",    "REDUCED_PRICE_LUNCH",
                        "LEP",           "MIGRANT",       
                        "white",         "black",         "hisp",         
                        "asian",         "native",        "hw_native",
                        "two_more")]
#rename using lowercases and  names
names(data_15) <- c("county_id",     "county_name",   "district_id",      
                    "district_name", "school_id",     "school_name",  
                    "row_total",     "free_lunch",    "reduced_lunch",
                    "lep",           "migrant",              
                    "white",         "black",         "hisp",         
                    "asian",         "native",        "hw_native",
                    "two_more")
#use most recent to create year variable 
data_15$year <- 2015
##########
#new year 
##########
file <- "~/Google Drive/CRPE/CityWideMetrics/RawData/NJ/Enrollment/2015_16.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")

names(data)
# we only want totals CHECK CODEBOOK FOR RIGHT CODE and subset 

data_16 <- subset(data, PRGCODE == "55"  & SCHOOL_NAME != "District Total" & SCHOOL_NAME != "County Total" )
#create totals 
data_16$white <- data_16$WH_F + data_16$WH_M 
data_16$black <- data_16$BL_F + data_16$BL_M
data_16$hisp <- data_16$HI_F + data_16$HI_M
data_16$asian <- data_16$AS_F + data_16$AS_M
data_16$native <- data_16$AM_F + data_16$AM_M
data_16$hw_native <- data_16$PI_F + data_16$PI_M
data_16$two_more <- data_16$MU_F + data_16$MU_M

#Select your variables 
data_16 <- data_16[, c( "COUNTY_ID",     "COUNTY_NAME",   "DIST_ID",      
                        "LEA_NAME", "SCHOOL_ID",     "SCHOOL_NAME",  
                        "ROW_TOTAL",     "FREE_LUNCH",    "REDUCED_PRICE_LUNCH",
                        "LEP",           "MIGRANT",       
                        "white",         "black",         "hisp",         
                        "asian",         "native",        "hw_native",
                        "two_more")]
#rename using lowercases and  names
names(data_16) <- c("county_id",     "county_name",   "district_id",      
                    "district_name", "school_id",     "school_name",  
                    "row_total",     "free_lunch",    "reduced_lunch",
                    "lep",           "migrant",              
                    "white",         "black",         "hisp",         
                    "asian",         "native",        "hw_native",
                    "two_more")
#use most recent to create year variable 
data_16$year <- 2016
##########
##APPEND CHECK NAMES FIRST
########
NJ_enroll <- rbind(data_12, data_13, data_14, data_15, data_16)

#write.csv(njPerf, file = "~/Google Drive/CRPE/CityWideMetrics/CleanedData/CleanedNJParc12_15.csv") #csv
saveRDS(NJ_enroll, file="~/Google Drive/CRPE/CityWideMetrics/CleanedData/New Jersey/NJ_enrollment2010_16.RData") #Rdata 
######
