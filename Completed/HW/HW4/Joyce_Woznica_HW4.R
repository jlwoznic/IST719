# 
# Course: IST 719
# Name: Joyce Wozninca
# Homework #4
# Code: HW4 R Code
# Due Date: 2/10/2020
#

# --------------- Package Load -------------------------------------------------------------------
# Load the required packages.
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

# This loads applicable libraries
install.packages("plyr", dependencies=TRUE)
library(plyr)

# -----------  Part 1 --------------------------------------------------------------------------------------
# 
# Modify Plots with Illustrator
#
# ------------ Figure 4-5: Bar Chart  ------------
hotdog.df <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv", header=TRUE, stringsAsFactors = FALSE)

f.title <- "Nathan's Hot Dog Eating Contest Results, 1980-2010"
f.xaxis.lbl <- "Year"
f.yaxis.lbl <- "Hot dogs and buns (HDB) eaten"

# make sure those that are new records (value of 1) have the #821122 color
fill_colors <- c()
for (i in 1:length(hotdog.df$New.record))
{
  if (hotdog.df$New.record[i] == 1)
  {
    fill_colors <- c(fill_colors, "#821122")
  }
  else
  {
    fill_colors <- c(fill_colors, "#CCCCCC")
  }
}

# Plot the bar graph
barplot(hotdog.df$Dogs.eaten, names.arg=hotdog.df$Year, col=fill_colors, border=NA, 
        xlab=f.xaxis.lbl, ylab=f.yaxis.lbl, main=f.title,
        space=0.3) 

# ------------  Figure 4-21: Stacked Bar Chart ------------
hot_dog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", 
                           sep = ",", header = TRUE)
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004", "2005",
                           "2006", "2007", "2008", "2009", "2010")
hot_dog_matrix <- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0,200), 
        xlab = "Year", ylab = "Hot dogs and buns (HDBs) eaten",
        main = "Hot Dog Eating Contest Results, 1980-2010")

# ------------  Figure 4-25: Scatterplot ------------
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv",
                        sep=",", header=TRUE)
plot(subscribers$Subscribers, type="p", ylim=c(0, 30000))

# ------------ Figure 4-40: Timeseries ------------
population <-
  read.csv("http://datasets.flowingdata.com/world-population.csv",
           sep=",", header=TRUE)
plot(population$Year, population$Population, type="l",
     ylim=c(0, 7000000000), xlab="Year", ylab="Population")

# ------------ Figure 4-42: Step chart ------------
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep=",", header=TRUE)
plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rate (Dollars)")

# ------------ Figure 4-47: LOESS curve ------------
# Load data
unemployment <- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv",
                          sep=",")
# Plain scatter plot
plot(1:length(unemployment$Value), unemployment$Value)
scatter.smooth(x=1:length(unemployment$Value), y=unemployment$Value,
               ylim=c(0,11), degree=2, col="#CCCCCC", span=0.5)

# -----------  Part 2 --------------------------------------------------------------------------------------
# 
# VT, Chapter 6, Plots
#
# ------------ Figure 6-9: Scatterplot matrix------------
# Load the data
crime <-  read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv", sep=",", header=TRUE)
crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]

# plot the scatterplot matrix
# add a LOESS curve
plot(crime2[,2:9], panel = panel.smooth)

# ------------ Figure 6-15: Bubble chart------------
crime <-  read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", sep="\t", header=TRUE)
symbols(crime$murder, crime$burglary, circles = crime$population)
radius <- sqrt (crime$population/pi)
symbols(crime$murder, crime$burglary, circles = radius, inches = 0.35,
        fg = "white", bg = "red", xlab = "Murder Rate", ylab = "Burglary Rate")
text(crime$murder, crime$burglary, crime$state, cex = 0.5)

# ------------ Figure 6-24: Histogram ------------
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
hist(birth$X2008, breaks=10, xlim=c(0, 60))

# ------------ Figure 6-32: Density Plot ------------
birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)

library(lattice)
# just plotted density
plot(d2008, type="n")
polygon(d2008, col="#821122", border="#cccccc")

# ------------ One or Two Small Multiple Dimension Plots ---------------
#
# See Figures 6-38 and 6-40
# you need the lattice or ggplot2 packages to make small multiple plots
# you can use the art of sales datasets
#
# Note: No need to use Adobe Illustrator
sales <- read.csv("/Users/joycewoznica/IST719/Labs/Lab3/sales.csv", header=TRUE, stringsAsFactors = FALSE)

breaks = seq(0,800,by=50)
par(mfrow=c(4,3))
# B L T R
par(mar = c(2, 2, 2.5, 0.25), cex.lab = 0.25, cex.main = 1.5)
par(family="serif")
# need more space between tops and bottoms and more overall on top
# maybe do rep by 
reps.list <- unique(sales$sales.rep)
library(RColorBrewer)
num.colors <- 12
xtz <- colorRampPalette(c("navy", "cyan", "darkgreen", "green", "yellow", "orange", "red"))
my.cols <- xtz(num.colors)

# build plot
i <- 1
while (i <= 12)
{
  hist(sales[sales$sales.rep == reps.list[i],]$income, xlim=c(0,max(sales$income)), 
       breaks= breaks, xlab = NA, col=my.cols[i],
       main = paste("Income Distribution -", reps.list[i], sep=" "))
  i <- i + 1
}


