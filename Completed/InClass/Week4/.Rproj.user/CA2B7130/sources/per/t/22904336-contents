# 
# Course: IST 719
# Name: Joyce Wozninca
# Homework #2 (Week 2)
# Code: HW2 R Code
# Due Date: 1/20/2020
#

# ----------------------------------------------------------------------------------------------------------------------
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

# ----------------------------------------------------------------------------------------------------------------------
# Part 1
# Recreate Graphics in Chapter 4 - Visualize This
# ------------------------
# Reproduce Figure 4-11 - Bar Chart
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
# to get axis the same - resize the plot
# exported as Figure4-11

# ------------------------
# Reproduce Figure 4-22 - Stacked Bar Chart
hotdogplaces.df <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", sep=",", header=TRUE)
names(hotdogplaces.df) <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")
hotdog.matrix <- as.matrix(hotdogplaces.df)

f.title <- "Hot Dog Eating Contest Results, 1980-2010"
f.xaxis.lbl <- "Year"
f.yaxis.lbl <- "Hot dogs and buns (HDB) eaten"

# plot this
barplot(hotdog.matrix, border=NA, space=0.25, ylim=c(0,200),
        xlab=f.xaxis.lbl, ylab=f.yaxis.lbl, main = f.title)
# to get axis the same - resize the plot
# exported as Figure4-22

# ------------------------
# Reproduce Figure 4-28 - Scatterplot
# read in the subscriber data
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv", sep=",", header=TRUE)
plot(subscribers$Subscribers, type="h", ylim=c(0,30000),
     xlab="Day", ylab="Subscribers")
points(subscribers$Subscribers, pch=19, col="black")
# exported as Fig4-28

# ------------------------
# Reproduce Figure 4-34 - Time Series
# read in the population data
population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep=",", header=TRUE)
plot(population$Year, population$Population,type="l", ylim=c(0,7000000000), xlab="Year", ylab="Population")
# exported as Fig4-34

# ------------------------
# Reproduce Figure 4-43 - Step Chart
# read in the postage data
postage <- read.csv ("http://datasets.flowingdata.com/us-postage.csv", sep=",", header=TRUE)
plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rate (Dollars)")
# exported as Fig4-43

# ----------------------------------------------------------------------------------------------------------------------
# Part 2: Simple Distributions
# Download art.csv (from the Data and Images section) to your (or a lab) machine. 
artdata <- read.csv("/Users/joycewoznica/IST719/HW/Homework2-Week2/art.csv", 
                    sep=",", header=TRUE, stringsAsFactors = FALSE)

str(artdata)
summary(artdata)
View(artdata)

# Using R, make four plots to answer the following questions. Here, we are interested in looking at different ways to 
# look at the same data. Use the par() function to put all four plots in the same plot space (like we did in the lab). 
# Also, give the plots titles and x- and y-axis labels, use colors that you like, and, if you are using a plot with points,  
# use a symbol you like (pch). In other words, customize these plots to show me that you know how to modify different elements of the plots.

# set up to put plots on one page
par(mfrow = c(2,2))

# 1. What is the distribution of total.sale for the whole dataset? 
#    Provide two different plots that show two different ways of showing distribution. 
#    Title your plot(s): Distribution of total.sale

# plot one is a histogram showing total sales distribution
num.colors <- 6
# fade colors from blue to red
FUN <- colorRampPalette(c("green", "blue"))
my.cols <- FUN(num.colors)

x <- artdata$total.sale
h <- hist(artdata$total.sale, breaks=12, col=my.cols,
          xlab = "Total Sales", ylab = "Frequency of Sales", 
          main = "Distribution of Total Sales")
xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit,yfit, col="red", lwd=2)

# now do a density plot of total sale
d <- density(artdata$total.sale)
plot(d, main = "Distribution of Total Sales")
polygon(d, col="turquoise1", border="darkgreen")

# 2. Next we want to compare the distributions of subsets of total.sales. 
#    Use a third type of distribution plot 
#    (different from what you used for the question above) for both of these plots.
# 2a. What is the distribution of the totals sales for drawing paper? 
#     Title your plot “distribution of the total sales for drawing paper”
unique(artdata$paper)
drawing.df <- artdata[artdata$paper == "drawing",]
boxplot(drawing.df$total.sale, col=c("yellow"), 
        xlab = "Drawing Paper",  ylab = "Total Sales in Dollars ($)", 
        main="Distribution of Total Sales for Drawing Paper",
        ylim = c(0,100), cex.main = 0.85)

# 2b. What is the distribution of the totals sales for watercolor paper? 
#    Title your plot: “distribution of the totals sales for watercolor paper
watercolor.df <- artdata[artdata$paper == "watercolor",]
boxplot(watercolor.df$total.sale, col=c("blue"), 
        xlab = "Watercolor Paper",  ylab = "Total Sales in Dollars ($)", 
        main="Distribution of Total Sales for Watercolor Paper",
        ylim = c(0,100), cex.main = 0.85)

# ----------------------------------------------------------------------------------------------------------------------
# Part 3: Grouping and Multidimension Plots
# Using the art.csv dataset again, answer the questions below. 
# This work will require that you make plots that show relationships or allow comparisons. 
# You will need to use some of the grouping functions and data sub-setting we used in the lab. 
# Don't forget to set stringsAsFactors = FALSE, or it will affect the results of your plots. 
# Also, put all three plots in the same plot space again, and customize colors and other parameters in some interesting way.
par(mfrow = c(2,2))

# 1. Is there a relationship between the unit price of art goods and their units sold? 
#    If so, what kind of relationship is it? Indicate which plot answers this question.

# relationships between data with a scatter plot
plot(artdata$unit.price, artdata$units.sold, 
     main = "1. Relationship of Unit Price to Units Sold",
     sub = "Units sold decrease as unit price increases, not linear",
     xlab = "Unit Price", ylab = "Units Sold",
     col = "darkgreen", pch = 19, cex=0.5)
# how do we see what kind of relationship it is? TRENDLINE
abline(lm(artdata$units.sold ~ artdata$unit.price), col="red", lwd = 3) # regression line (y~x)
# answer: "Units sold decrease as unit price increases, but not linearly"
# shown in plot in upper left corner (#1)

# 2. Does the art company sell more units of drawing paper or watercolor paper? 
#    Indicate which plot answers this question.
p.units <- tapply(artdata$units.sold, list(artdata$paper), sum)
barplot(p.units, main = "2. Units Sold by Paper", ylab = "Units Sold", 
        col = c("tan", "lightblue"),
        sub = "Art Company sells more units of watercolor paper")
# answer: Art Company sells more units of watercolor paper
# shown in plot in upper right corner (#2)

# 3. Does the art company bring in more money (income) selling drawing paper or watercolor paper? 
#    Indicate which plot answers this question.
# income = total.sales
totbystore <- tapply(artdata$total.sale,list(artdata$paper, artdata$store), sum)
totbystore.df <- as.data.frame(totbystore)
# add in grand totals across all stores
t.sales <- melt(tapply(artdata$total.sale, list(artdata$paper), sum))
colnames(t.sales) <- c("paper", "total")
totbystore.df$Total <- t.sales$total
totbystore.max <- as.matrix(totbystore.df)
barplot(totbystore.max, beside=TRUE,  
        main = "3. Total Sale by Different\nPaper Materials by Store", col=c("yellow","navy"),
        ylab = "Total Units Sold", xlab="Store and Grand Total", cex.names=0.75,
        sub = "Art Company makes more money by selling watercolor paper",
        ylim = c(0,135000))
legend('topleft', legend = rownames(totbystore.df), lwd=5, lty=5, bty="n", col = c("yellow","navy"),cex=0.5)
# answer: Art Company makes more by selling watercolor paperr
# shown in plot in lower left corner (#3))
