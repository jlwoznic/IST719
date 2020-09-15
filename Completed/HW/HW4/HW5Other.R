# Author: Pan
# Purpose: IST719 HW5

# HW 5
# Submitexercises in VT Chapter 6
# Scatterplot Matrix, Figure 6-9
# Bubble chart, Figure 6-15
# Histogram, Figure 6-24
# Density Plot. Figure 6-32
# Small multiple using data of your choice, see Fig. 6-38 & 6-40
# Submit all charts as PDF names Ex6_LASTNAME.pdf

#correlation can help you predict one metric by knowing another, with scatterplot and multiple scatterplots. scatterplot can be used to look for relationships between two variables

# Load data
crime <-
  read.csv(
    "http://datasets.flowingdata.com/crimeRatesByState2005.csv",
    sep=",")
crime[1:10,]
crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]

# Scatterplot Matrix
plot(crime2[,2:9])
pairs(crime2[,2:9], panel=panel.smooth) #The panel argument in pairs() takes a function of x and y. In this example, you use panel.smooth(), which is a native function in R and results in a LOESS curve. 

#bubble chart



radius <- sqrt( crime2$population/ pi )

symbols(crime2$murder, crime2$burglary, circles=radius, inches=0.35,fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate")
text(crime2$murder, crime2$burglary, crime2$state, cex=0.7)
crime3<-crime2

# Histogram, Figure 6-24
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
hist(birth$X2008)

# Density Plot. Figure 6-32
birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)
d2008frame <- data.frame(d2008$x, d2008$y)
write.table(d2008frame, "birthdensity.txt", sep="\t")
plot(d2008, type="n")
polygon(d2008, col="#821122", border="#cccccc")

# Small multiple using data of your choice, see Fig. 6-38 & 6-40
data.fname <-file.choose()
sales <- read.csv(data.fname, header= TRUE, stringsAsFactors = FALSE)
#Receipt sale of each type of wine
breaks = seq(0,800,by=50)
max(sales$recipt)
par(mfrow=c(7,1))
?hist
hist(sales[sales$wine=="Riesling",]$recipt, xlim=c(0,max(sales$recipt)), breaks=breaks, xlab="Receipt sale of Riesling",main="Distribution of Receipt Sale of Riesling")
hist(sales[sales$wine=="Pinot Gris",]$recipt, xlim=c(0,max(sales$recipt)),breaks=breaks,xlab="Receipt sale of Pinot Gris",main="Distribution of Receipt Sale of Pinot Gris")
hist(sales[sales$wine=="Chardonnay",]$recipt, xlim=c(0,max(sales$recipt)),breaks=breaks,xlab="Receipt sale of Chardonnay",main="Distribution of Receipt Sale of Chardonnay")
hist(sales[sales$wine=="Cabernet Sauvignon",]$recipt, xlim=c(0,max(sales$recipt)),breaks=breaks,xlab="Receipt sale of Cabernet Sauvignon",main="Distribution of Receipt Sale of Cabernet Sauvignon")
hist(sales[sales$wine=="Merlot",]$recipt,xlim=c(0,max(sales$recipt)),breaks=breaks,xlab="Receipt sale of Merlot",main="Distribution of Receipt Sale of Merlot")
hist(sales[sales$wine=="Sauvignon Blanc",]$recipt, xlim=c(0,max(sales$recipt)),breaks=breaks, xlab="Receipt sale of Sauvignon Blanc",main="Distribution of Receipt Sale of Sauvignon Blanc")
hist(sales[sales$wine=="Shiraz",]$recipt, xlim=c(0,max(sales$recipt)),breaks=breaks,xlab="Receipt sale of Shiraz",main="Distribution of Receipt Sale of Shiraz")
sales<-sales[sales$recipt<800,]




