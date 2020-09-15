# 
# Course: IST719
# Name: Joyce Woznica
# Project Code: HW3
# Due Date: 2/3/2020
#
# Package Section
# ------------------------------------------------------------------
#specify the packages of interest
packages=c("reshape2", "ggplot2", "dplyr", "RColorBrewer")

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

# I find this does not always work, so added to install when required here
require(dplyr)

install.packages("plyr",dependencies=TRUE)
library(plyr)

# Data loading
# ------------------------------------------------------------------
rData <- read.csv("/Users/joycewoznica/IST719/Project/equine-death-and-breakdown.csv", 
                  header = TRUE, na.strings = "NA", 
                  stringsAsFactors = FALSE)

# review missing data
tMissing <-sum(is.na(rData))
cat("The number of missing values in Equine Death and Breakdown Data is ", tMissing)
rData$Weather.Conditions[is.na(rData$Weather.Conditions)] <- "Clear" 

# although not showing up as NA or missing - there is quite a bit of missin:
# - weather information
# - Inv. Location can be blank - should we make any assumptions?
# what should we do with that?
# ? should we do something to standardize the weather ?
# maybe come up with a scheme - "if contains cloudy" -> Cloudy, "if contains Clear" -> Clear, etc.

# ------------------------------------------------------------------
# force no scientific notation
options(scipen=999)
# General Statistics and Analysis
summary(rData)
str(rData)

# A little more summary information
test<-as.matrix(rData)
summary(test)

# ------------------------------------------------------------------
# Initial Visualizations
# A) Single Dimensional
# Frequency of Incidents by Type
table(rData$Incident.Type)
inc.type <- table(rData$Incident.Type)
inc.type <- as.data.frame(inc.type)
colnames(inc.type) <- c("Incident.Type", "Frequency")
num.colors <- length(inc.type$Incident.Type)
# gives error, in future, pick greens, tans, beige, and browns
my.cols <- brewer.pal(num.colors, "BrBG")

my.bar <- barplot(table(rData$Incident.Type), col=my.cols,
                   ylab = "Incident Frequency",
                   main = "Single Dimensional: Number of Incidents by Type",
                   las = 2, cex.names = 0.65,
                   legend = TRUE, xaxt = "n", 
                   args.legend = list(x = "topleft", bty = "n", cex=0.75))

# B) Single Dimension
# Frequency of Incidents by Track
for.pie<-as.data.frame(table(rData$Track))
colnames(for.pie) <- c("Track", "Frequency")
# need more colors, but for now - okay
my.cols <- brewer.pal(num.colors, "Set3")

pie(for.pie$Frequency, labels = for.pie$Track, 
    main = "Single Dimensional: Frequency of Incidents by Track",
    angle = 45, cex = .75,
    col = my.cols)

# C) Single Dimension
# Incidents per year - box plot?
for.plot  <-as.data.frame(table(rData$Year))
colnames (for.plot) <- c("Year", "Frequency")

plot(for.plot$Frequency, type = "b", 
     main = "Single Dimensional: Racetrack Incidents by Year", 
     sub = "Decline of Equine Racetrack Incidents noted by Year",
     xaxt = "n", pch = 19, lwd = 2,
     col = "darkgreen", bg = "yellow",
     xlab = "Year", ylab = "Incident Frequency")
# add year to x axis
axis(1, at=1:11, labels = for.plot$Year)

# pick better colors
num.colors <- length(unique(rData$Incident.Type))
xtz <- colorRampPalette(c("blue", "purple", "red", "orange", "yellow", "green"))
#xtz <- colorRampPalette(c("navy", "blue", "darkgreen", "tan"))
my.cols <- xtz(num.colors)

# multi-dimensional 1
barplot(table(rData$Incident.Type, rData$Division), beside=TRUE, legend=TRUE,
        main="Mulit-Dimensional: Incident Type by Racetrack Division", 
        sub="Frequency of Each Incident Type by Racetrack Division - Harness or Thoroughbred",
        col = my.cols,
        axisnames=TRUE, las = 1, cex.names = 1,
        args.legend = list(x = "topleft", bty = "n", cex=0.85))

# MultiDimensional 2
num.colors <- length(unique(rData$Incident.Type))
xtz <- colorRampPalette(c("navy", "blue", "darkgreen", "green", "yellow", "brown"))
my.cols <- xtz(num.colors)

g <- ggplot(rData, aes(x=Track, fill=factor(Incident.Type)))
g <- g + ggtitle("Multi-Dimensional: Incident Type by Track")
g <- g + labs(subtitle="Number of each Incident Type that occured at each Track in New York State")
g <- g + geom_bar(position="stack")
g <- g + scale_fill_manual(name="Incident Types", values= my.cols)
g <- g + xlab("Track") + ylab("Number of Incidents")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.title = element_text(hjust=0.5))
g
