# Class 2 in class work
# Author: Joyce Woznica
# wine sales

my.dir <- "/Users/joycewoznica/IST719/Labs/Lab3/"
sales <- read.csv(file=paste0(my.dir, "sales.csv"), header=TRUE, stringsAsFactors = FALSE)

View(sales)
str(sales)
colnames(sales)

plot(sales$expenses, sales$income)

plot(sales$expenses, sales$income, pch = 16, cex = 1, col = "orange")

# abline
abline(lm(sales$income ~ sales$expenses), col = "red", lwd = 3)
abline(h - 400, col = "blue")
abline (v = 9, col = "blue")
# rug function for tick marks

# box plot
# 2-dimensional
boxplot(sales$units.sold~sales$type, xlab="Wine Type", ylab="Units Sold", col = c("palegreen", "paleturquoise1"), main = "2 dimensional box plot")

units.by.wine <- aggregate(sales$units.sold, list(wine=sales$wine), FUN=sum)
units.by.wine

# red - red, white - white, barplot of regions by units.sold
# use tapply or aggregate
# col = "red" if sales$wine = "red"
# col = "white" if sales$wine = "white"
ab <- tapply(sales$units.sold, list(sales$rep.region, sales$type), sum)
library(reshape2)
ab <- melt(ab)
colnames(ab) <- c("region", "type", "units.sold")
barplot(ab$units.sold, names.arg = ab$region, col = c("red", "white"),
          beside=TRUE)

cd <- tapply(sales$units.sold, list(sales$type, sales$rep.region), sum)
cd <- as.data.frame(cd)
#cd <- melt(cd)
#colnames(cd) <- c("type", "region", "units.sold")
barplot(cd$units.sold, names.arg = cd$type, col = c("palegreen", "blue", "orange", "red", "paleturquoise1"),
        legend.text = c("Central", "East", "North", "South", "West"),
        beside=TRUE)

