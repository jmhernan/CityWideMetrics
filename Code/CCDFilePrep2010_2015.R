######################################
# CCD File preparation
# 2010-2014
# Variable extraction for all years
# Jose M Hernandez
# 10/17/2016
######################################
# Updated 1/26/2016 JH
# NOW INCLUDES 2014-15 School information 
######################################
rm(list=ls())
#install.packages('readr')
library(readr)
library(dplyr)


file09 <- "~/Google Drive/CRPE/ccd/sc092a.txt" 
data09 <- read_delim(file09, delim = '\t')
#only take what is needed 
data09 <- data09 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID09,SEASCH09,LEANM09,SCHNAM09,
         TYPE09,STATUS09,ULOCAL09,LATCOD09,LONCOD09,LEVEL09,MAGNET09,CHARTR09) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID09,seasch=SEASCH09,leanm=LEANM09,schnam=SCHNAM09,
         type=TYPE09,status=STATUS09,ulocal=ULOCAL09,latcod=LATCOD09,loncod=LONCOD09,level=LEVEL09,magnet=MAGNET09,chartr=CHARTR09)

data09$year <- 2010
##2011
file10 <- "~/Google Drive/CRPE/ccd/sc102a.txt" 
data10 <- read_delim(file10, delim = '\t')

#only take what is needed 
data10 <- data10 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID,SEASCH,LEANM,SCHNAM,
         TYPE,STATUS,ULOCAL,LATCOD,LONCOD,LEVEL,MAGNET,CHARTR) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID,seasch=SEASCH,leanm=LEANM,schnam=SCHNAM,
         type=TYPE,status=STATUS,ulocal=ULOCAL,latcod=LATCOD,loncod=LONCOD,level=LEVEL,magnet=MAGNET,chartr=CHARTR)
data10$year <- 2011
##2012
file11 <- "~/Google Drive/CRPE/ccd/sc111a_supp.txt" 
data11 <- read_delim(file11, delim = '\t')

#only take what is needed 
data11 <- data11 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID,SEASCH,LEANM,SCHNAM,
         TYPE,STATUS,ULOCAL,LATCOD,LONCOD,LEVEL,MAGNET,CHARTR) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID,seasch=SEASCH,leanm=LEANM,schnam=SCHNAM,
         type=TYPE,status=STATUS,ulocal=ULOCAL,latcod=LATCOD,loncod=LONCOD,level=LEVEL,magnet=MAGNET,chartr=CHARTR)
data11$year <- 2012
#2013
#only take what is needed 
file12 <- "~/Google Drive/CRPE/ccd/sc122a.txt" 
data12 <- read_delim(file12, delim = '\t')

data12 <- data12 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID,SEASCH,LEANM,SCHNAM,
         TYPE,STATUS,ULOCAL,LATCOD,LONCOD,LEVEL,MAGNET,CHARTR) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID,seasch=SEASCH,leanm=LEANM,schnam=SCHNAM,
         type=TYPE,status=STATUS,ulocal=ULOCAL,latcod=LATCOD,loncod=LONCOD,level=LEVEL,magnet=MAGNET,chartr=CHARTR)
data12$year <- 2013

#2014

file13 <- "~/Google Drive/CRPE/ccd/sc122a.txt" 
data13 <- read_delim(file13, delim = '\t')

names(data13)
data13 <- data13 %>%
  select(NCESSCH, FIPST,LEAID,SCHNO,STID,SEASCH,LEANM,SCHNAM,
         TYPE,STATUS,ULOCAL,LATCOD,LONCOD,LEVEL,MAGNET,CHARTR) %>%
  rename(ncessch=NCESSCH, fipst=FIPST,leaid=LEAID,schno=SCHNO,stid=STID,seasch=SEASCH,leanm=LEANM,schnam=SCHNAM,
         type=TYPE,status=STATUS,ulocal=ULOCAL,latcod=LATCOD,loncod=LONCOD,level=LEVEL,magnet=MAGNET,chartr=CHARTR)
data13$year <- 2014

#2015#
#The 2014-15 file is not yet out as of (1/26/2017) Check to see if this is still the case.  As a result we have to combine multiple CCD files,
#the geographic locations and schools charecteristic files contain all the information we need.  We no longer use the enrollment numbers from the 
#CCD data and that's the file that is lagged the most...

data14 <- readRDS("~/Google Drive/CRPE/ccd/ccd_201415.RData")

data14$magnet <- NA
names(data14)
data14 <- data14 %>%
  select(NCESSCH, FIPST.y,LEAID,SCHID,ST_LEAID,ST_SCHID,LEA_NAME,SCH_NAME,
         SCH_TYPE,SY_STATUS,LOCALE,LATCODE,LONGCODE,LEVEL,magnet,CHARTER_TEXT) %>%
  rename(ncessch=NCESSCH, fipst=FIPST.y,leaid=LEAID,schno=SCHID,stid=ST_LEAID,seasch=ST_SCHID,leanm=LEA_NAME,schnam=SCH_NAME,
         type=SCH_TYPE,status=SY_STATUS,ulocal=LOCALE,latcod=LATCODE,loncod=LONGCODE,level=LEVEL,magnet=magnet,chartr=CHARTER_TEXT)
data14$year <- 2015

###append and calculate proportions and other clean_up
ccd_2010_15 <- rbind(data09,data10, data11, data12, data13, data14)
ccd_2010_15 <- data.frame(ccd_2010_15)
#Clean up missing values 
##fix missing values
ccd_2010_15[ ,]  <- lapply(ccd_2010_15[ ,] , 
                           FUN = function(x) {x[x == -1] <- NA; x})
ccd_2010_15[ ,]  <- lapply(ccd_2010_15[ ,] , 
                           FUN = function(x) {x[x == -2] <- NA; x})
ccd_2010_15[ ,]  <- lapply(ccd_2010_15[ ,] , 
                           FUN = function(x) {x[x == -9] <- NA; x})
# Prep other variables
summary(ccd_2010_15$ulocal)
#Use these codes
#ULOCAL        24     AN     NCES urban-centric locale code.  
# 11 = City, Large
# 12 = City, Mid-size
# 13 = City, Small
# 21 = Suburb, Large
# 22 = Suburb, Mid-size
# 23 = Suburb, Small
# 31 = Town, Fringe
# 32 = Town, Distant
# 33 = Town, Remote
# 41 = Rural, Fringe
# 42 = Rural, Distant
# 43 = Rural, Remote





ccd_2010_15$urban <- ifelse(ccd_2010_15$ulocal == 11 | ccd_2010_15$ulocal == 12 | ccd_2010_15$ulocal == 13, "urban",
                            ifelse(is.na(ccd_2010_15$ulocal), "missing","not urban"))

ccd_2010_15$urban[is.na(ccd_2010_15$urban)] <- "missing"

table(ccd_2010_15$urban)
###################################################################################################################
## Adapt Stata file to locate names to flag as "not a school" created by Thiago and Patrick for City Indicators Work 
## file lives in AWS: "Project\CRPE\projects\arnold\data\_ccd\attach_ccd_demographics_2015-03-31.do"
####################################################################################################################
ccd_2010_15$notschool <- tolower(ccd_2010_15$schnam)
# online home schools 
ccd_2010_15[grep("online | on-line",ccd_2010_15$notschool),]$notschool = "1"  
ccd_2010_15[grep("\\<distance\\>",ccd_2010_15$notschool),]$notschool = "1"  
ccd_2010_15[grep("home sch",ccd_2010_15$notschool),]$notschool = "1"  
ccd_2010_15[grep("\\<homelink\\>",ccd_2010_15$notschool),]$notschool = "1"  
ccd_2010_15[grep("\\<homebound\\>",ccd_2010_15$notschool),]$notschool = "1"  
ccd_2010_15[grep("\\<homeworks\\>",ccd_2010_15$notschool),]$notschool = "1"  
ccd_2010_15[grep("\\<home connections\\>",ccd_2010_15$notschool),]$notschool = "1"  
ccd_2010_15[grep("\\<virtual\\>",ccd_2010_15$notschool),]$notschool = "1"  
#juvinile detention/ hospital/detention centers 
ccd_2010_15[grep("det ctr",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<juvenile\\>",ccd_2010_15$notschool),]$notschool = "1" 
#ccd_2010_15[grep("\\<correct\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<detention\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<j j a e p\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<jjaep\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<jail\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<treatment\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<hospital\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<hospitality\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<deaf\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<blind\\>",ccd_2010_15$notschool),]$notschool = "1" 
ccd_2010_15[grep("\\<therapy\\>",ccd_2010_15$notschool),]$notschool = "1" 
#ccd_2010_15[grep("facility",ccd_2010_15$notschool),]$notschool = "1" 
#$ccd_2010_15[grep("facilities",ccd_2010_15$notschool),]$notschool = "1" 
#ccd_2010_15[grep("transition",ccd_2010_15$notschool),]$notschool = "1" 
#special ed
ccd_2010_15[grep("\\<life skills\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<skills center\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<special\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<sp ed\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<spcl needs\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("independent stud",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<independent lrn\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<independent learning\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<independent alternative\\>",ccd_2010_15$notschool),]$notschool = "1" ##
ccd_2010_15[grep("\\<independent technical real access\\>",ccd_2010_15$notschool),]$notschool = "1"
#ccd_2010_15[grep("\\<continu\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<alternative\\>",ccd_2010_15$notschool),]$notschool = "1" ##
ccd_2010_15[grep("\\<alt\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<adult\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<learning support\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<lrn ctr\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<lrn center\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<learning ctr\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<learning center\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<program\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<preschool\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("\\<nursery\\>",ccd_2010_15$notschool),]$notschool = "1"
#ccd_2010_15[grep("\\<renew acceler\\>",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("tech. ctr.",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("ctr.",ccd_2010_15$notschool),]$notschool = "1"
ccd_2010_15[grep("coop.",ccd_2010_15$notschool),]$notschool = "1"

ccd_2010_15$notschool <- ifelse(ccd_2010_15$notschool == "1" , "1","0")
table(ccd_2010_15$notschool)

#########Save file 
saveRDS(ccd_2010_15, file="~/Google Drive/CRPE/ccd/ccd_2010_15.RData")
#########