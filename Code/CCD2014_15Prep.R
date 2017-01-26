#################################################################################
#  Jose Hernandez                                                               #
#  Combine CCD files to use in the creation of master                           #
#  does not contain enrollment information this is a preliminary release of the # 
#  school information                                                           #
#################################################################################
#############################
# Combine 2014-15 CCD files #
#############################
library(readr)
library(dplyr)
#geographic information
file <- "~/Google Drive/CRPE/ccd/geolacation1415.csv" 
data <- read.csv(file, header=TRUE,stringsAsFactors = F,fileEncoding="latin1")
names(data)

#school characteritics 
file14 <- "~/Google Drive/CRPE/ccd/ccd_sch_029_1415_w_0216601a.txt" 
data14 <- read_delim(file14, delim = '\t', col_types = "")
str(data14)
data14$NCESSCH <- as.numeric(data14$NCESSCH)
#join the two 

ccd2014_15 <- full_join(data14,data, by=c("NCESSCH"))
ccd2014_15 <- data.frame(ccd2014_15)

saveRDS(ccd2014_15, file="~/Google Drive/CRPE/ccd/ccd_201415.RData")
###############