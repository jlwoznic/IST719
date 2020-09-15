####

## Author: Daniel Scholnick, Hongseok Kim, Joyce Woznica
## Purpose: Vizathon
## Class: IST719

###

#load programs
library(readxl)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(scales)
library(vioplot)

#load dataset- page 2 only
file_buoy <- read_excel("/Users/dscholnick/Documents/MiDS/IST719/BuoyData_2_2.xlsx", sheet = 2)
buoy <- as.data.frame(file_buoy)

#explore data
head(buoy) 
str(buoy)
colnames(buoy)
dim(buoy)


#Change colnames
colnames(buoy) <- c("Date.Time", "Depth(m)", "Temperature(C)", "Ionic.Content", "pH", "Oxygen(mgL)", "Turbidity", "Algae(mgL)"
                    ,"Precipitation(in)", "Avg.Daily.Wind(mph)", "Wind.Direction", "Max.Wind(mph)" )

#Add in a column for month and year
buoy$Year<- strftime(buoy$Date.Time,"%Y")
buoy$Month<- strftime(buoy$Date.Time,"%m")

#check
head(buoy$Month)
head(buoy$Year)
colnames(buoy)

####Create a normalized data set
#define Min-Max normalization function
min_max_norm <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }

#######create a norm data frame for comparison of variables
buoy_nm <- as.data.frame( lapply(buoy[,2:12], min_max_norm))
#add dates back in 
buoy_norm <- cbind(buoy$Date.Time, buoy_nm, buoy$Year, buoy$Month)
colnames(buoy_norm) <- c("Date.Time", "Depth(m)", "Temperature(C)", "Ionic.Content", "pH", "Oxygen(mgL)", "Turbidity", "Algae(mgL)"
                      ,"Precipitation(in)", "Avg.Daily.Wind(mph)", "Wind.Direction", "Max.Wind(mph)", "Year", "Month" )
#check results
str(buoy_norm)
unique(buoy$Month)

##########################
#Visualizations
##########################    

#Create boxplot of variation of measurements from norm data
par(mar = c(10,3,3,1))
boxplot(buoy_norm[,c(2:12)], las = 2
        , col = c( "orange","red","purple", "yellow","blue" ,"brown", "green", "goldenrod", "magenta", "lightblue", "darkgreen")
        , pch= 16, main = "Variation of Onondaga Lake Measurements")


#Histogram of Algae
hist(buoy$`Algae(mgL)`,  col = "yellowgreen", main = "Histogram of Onondaga Lake Algae Levels", las = 1
     ,ylab = "", xlab = "Algae(mgL)", xlim = c(0,35), breaks = 40)


#Violin plot of oxygen
par(mar= c(1,5,3,1))
vioplot(buoy$Ionic.Content,  col = "purple", main = "Frequency of Ionic Content Levels in Onondaga Lake", las = 1
        ,ylab = "Ionic Content (microSiemens per centimeter)")

# #Violin plot of Depth
# vioplot(buoy$`Depth(m)`,  col = "lightblue2", main = "Frequency of Water Depth Measurements", las = 1
#         ,ylab = "Depth(m)")

#barplot of Algae level by depth
par(mar = c(5,5,2,1))
res <- 18
greens <- colorRampPalette(c( "darkgreen", "darkgreen","yellowgreen", "lightblue1","blue"))(res)
g.cut <- as.factor(cut(ag[, "Depth(m)"], breaks = res))
cols <- greens[g.cut]
barplot(ag$`Algae(mgL)`~ ag$`Depth(m)`, las = 1, col = cols
        , xlab = "Depth (meters)", ylab = "Algae (mgL)", main = "Onondaga Lake Algae Level by Depth")

######Aggregate data

#aggregate buoy columns with date as a list
ag2 <- aggregate(buoy[,2:8], list(buoy$Date.Time), mean, na.rm = T)
str(ag2)

# plot(ag2[,3:8], panel = panel.smooth, pch = 16, col = "blue"
#      ,main = "Daily Averages of Onondaga Lake Water Metrics")
# colnames(ag2)

##aggregate buoy_norm columns with date as a list
ag2_norm <- aggregate(buoy_norm[,2:12], list(buoy_norm$Year), mean, na.rm = T)
str(ag2_norm)

# ##Matrix of plots of normalized data over time for water metrics
# plot(ag2_norm[,3:8], panel = panel.smooth, pch = 16, col = "blue"
#      ,main = "Daily averages")

# ##Matrix of Avg. Annual change of water metrics
# plot(ag2_norm[,2:8], panel = panel.smooth, pch = 16, col = "blue")
  

##Create multiline plot for Avg.Annual change of water metrics 
par(mar= c(5,3, 3, 10))
plot(ag2_norm$`Temperature(C)`,type = "b",col = "red", xlab = "Year", ylab = "",
     main = "Average Yearly Onondaga Lake Metrics 2016-2018", ylim=c(0,.8), pch = 16, las =1, lwd =1.5)
lines(ag2_norm$Ionic.Content, type = "b", col = "purple", pch = 16, lwd =1.5)
lines(ag2_norm$pH, type = "b", col = "yellow", pch = 16, lwd =1.5)
lines(ag2_norm$`Oxygen(mgL)`, type = "b", col = "blue", pch = 16, lwd =1.5)
lines(ag2_norm$Turbidity, type = "b", col = "brown", pch = 16, lwd =1.5)
lines(ag2_norm$`Algae(mgL)`, type = "b", col = "green", pch = 16, lwd =1.5)
lines(ag2_norm$`Precipitation(in)`, type = "b", col = "goldenrod", pch = 16, lwd =1.5)
lines(ag2_norm$`Avg.Daily.Wind(mph)`, type = "b", col = "magenta", pch = 16, lwd =1.5)
lines(ag2_norm$Wind.Direction, type = "b", col = "orange", pch = 16, lwd =1.5)
lines(ag2_norm$`Max.Wind(mph)`, type = "b", col = "darkgreen", pch = 16, lwd =1.5)
par(xpd=TRUE)
legend("topright", inset=c(-0.3,.2), legend=c("Temperature(C)", "Ionic.Content", "pH", "Oxygen(mgL)", "Turbidity", "Algae(mgL)"
                       ,"Precipitation(in)", "Avg.Daily.Wind(mph)", "Wind.Direction", "Max.Wind(mph)")
       ,col = c("red","purple", "yellow","blue" ,"brown", "green", "goldenrod", "magenta","orange", "darkgreen"), pch=16,cex=0.8
       ,title= "Metrics", text.font=4)

