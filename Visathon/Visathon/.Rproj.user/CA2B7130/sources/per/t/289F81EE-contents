# 
# Course: IST719
# Team: 
# Visathon
# Due Date: 3/17/2020
#
# ----------------  Package Section --------------------------------------------------
#specify the packages of interest
packages=c("reshape2", "ggplot2", "dplyr", "RColorBrewer", "lubridate", "stringr", "tm",
           "wordcloud", "alluvial", "treemap", "ggmap", "tidyverse", "jsonlite", "viridis")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()
require(dplyr)

install.packages("plyr",dependencies=TRUE)
library(plyr)

#----------------- Functions -----------------------------------------------------------
# create a function to replace each column NA with mean for that column

replaceNAwMeans<-function(vec)
{
  numcols<-length(colnames(vec))
  index<-1
  while(index<=numcols)
  {
    theColV <- vec[,index]
    if (is.numeric(theColV))
    {
      theColV[is.na(theColV)]<-mean(theColV,na.rm=TRUE)
      vec[,index]<-theColV
    }
    index<-index+1
  }
  return(vec)
}

#----------------- Load Buoy Data ------------------------------------------------------

buoyData <- read.csv("/Users/joycewoznica/IST719/Visathon/BuoyData_2_2.csv", 
                     header = TRUE,
                     stringsAsFactors = FALSE)

# need to get rid of NA's - maybe replace with means?
# review missing data
tMissing <-sum(is.na(buoyData))
cat("The number of missing values in Buoy Data is ", tMissing)

# replace missing data with means - which will not work on rows that are not numeric
buoyDataM <- replaceNAwMeans(buoyData)
tMissing <-sum(is.na(buoyDataM))
cat("The number of missing values in Buoy Data is ", tMissing)
buoyData <- buoyDataM

# structure of buoyData
str(buoyData)
# 24325 rows of 12 variables - add to poster
summary(buoyData)
# date code
#fixdate <- as.POSIXct(input.date, format="%m/%d/%y %H:%M")
#dates <- as.POSIXct(xmlSApply(dates,xmlValue),format="%Y-%m-%dT%H:%M:%S")

# Questions:
# - Does depth affect saltiness (DEPTH_m and SC_us_cm)
# - How does Chlorophyll affect dissovled oxygen (Chl_ug_L to Dox_mg_L)
# - Does percepitation change turbidity (cloudiness) (PRECIP_in to Tn_Ntu)
# - maybe a time series of something?

# Maybe scatter plot to see relationship between depth and saltiness
# - Does depth affect saltiness (DEPTH_m and SC_us_cm)
g <- ggplot(buoyData, aes(x = DEPTH_m, y = SC_us_cm))
g <- g + geom_point(aes(color = T_DEGC)) + 
  scale_color_viridis(option = "inferno")
g <- g + labs(title = "Saltiness by Depth at Buoy Location",
              subtitle = "showing temperature in degrees centigrade")
g <- g + theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
               plot.subtitle = element_text(hjust = 0.5))
g <- g + xlab("Depth of Location in Meters") + ylab("Specific Conductance (microSiemens per cm) or Saltiness") 
g

# Maybe something around algae content
# does pH have any relationship to Chl_ug_L (Chlorophyll)
g <- ggplot(buoyData, aes(x = pH, y = Chl_ug_L))
g <- g 

# Do something by time of day?
min(buoyData$DATE_TIME)
max(buoyData$DATE_TIME)
min(buoyData$Tn_Ntu)
max(buoyData$Tn_Ntu)

# tubidity over time
# need to extract and maybe group by date?
# this is a mess, so fix by month or day, not time
# extract the date and then keep in order and make plot by date or by month
g <- ggplot(buoyData) + aes(x = DATE_TIME, y = Tn_Ntu, group = pH)
g <- g + geom_line(aes(color = pH))
g <- g + geom_point(aes(color = pH)) 
g <- g + labs(title = "Turbidity (cloudiness) over Time",
              subtitle="grouped by Ph level")
g <- g + xlab("Date and Time") 
g

