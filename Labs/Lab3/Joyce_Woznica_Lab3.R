#
# Author: Joyce Woznica
# Class: IST 719
# Date: 1/22/2020
# Subject: Lab 3, Week 3 
#
# Lab 3

library(RColorBrewer)
my.dir <- "/Users/joycewoznica/IST719/Labs/Lab3/"
sales <- read.csv(file=paste0(my.dir, "sales.csv"), header=TRUE, stringsAsFactors = FALSE)

colnames(sales)
display.brewer.all()

# just create a random bit of information
rand.data <- replicate(8, rnorm(35,35,sd=1.5))
# boxplot using a set from RColorBrewer
boxplot(rand.data, col = brewer.pal(8, "Set1"))

num.colors <- 8
# fade colors from blue to red
FUN <- colorRampPalette(c("blue", "red"))
my.cols <- FUN(num.colors)
boxplot(rand.data, col=my.cols)

num.colors <- 8
# fade colors from blue to red to green
xtz <- colorRampPalette(c("blue", "red", "green"))
my.cols <- xtz(num.colors)
boxplot(rand.data, col=my.cols)

plot(sales$expenses, sales$income)

plot(sales$expenses, sales$income, pch = 16, cex = 1, col = "orange")

col.vec <- rep("orange", nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex = 1, col = col.vec)

col.vec <- rep(rgb(30,144,255, maxColorValue = 255), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex = 1, col = col.vec)

# subset of these based on a value, so set unit price over 14 to red, rest blue
col.vec[sales$unit.price > 14] <- rgb(255, 64, 64, maxColorValue = 255)
plot(sales$expenses, sales$income, pch=16, cex = 1, col = col.vec)

# continue to subset
col.vec <- rep(rgb(30,144,255, maxColorValue = 255), nrow(sales))
col.vec[sales$type == "red"] <- rgb(255, 64, 64, maxColorValue = 255)
plot(sales$expenses, sales$income, pch=16, cex = 1, col = col.vec)

x <- rnorm(117)
y <- rnorm(117)
plot(x,y)

# overplotting - fixes
col.vec <- rep(rgb(0.8, 0.15, 0.15), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex = 1, col = col.vec)

# alpha to add transparency
col.vec <- rep(rgb(0.8, 0.15, 0.15, alpha=0.3), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex = 1, col = col.vec)

# more transparency
col.vec <- rep(rgb(0.8, 0.15, 0.15, alpha=0.2), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex = 1, col = col.vec)

# change expansion size of the points
col.vec <- rep(rgb(0.8, 0.15, 0.15, alpha=0.3), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex = .5, col = col.vec)

# change again to see if there are two clouds
col.vec <- rep(rgb(0.8, 0.15, 0.15, alpha=0.1), nrow(sales))
plot(sales$expenses, sales$income, pch=16, cex = .3, col = col.vec)

# now try smoothscatter
smoothScatter(sales$expenses, sales$income, 
              colramp=colorRampPalette(c("black", "cyan", "pink", "red")))
smoothScatter(sales$expenses, sales$income, 
              colramp=colorRampPalette(c("white", "cyan", "pink", "red")))

install.packages("aplpack")
capabilities("tcltk")
# not working, try something different
library(aplpack)
bagplot(sales$expenses, sales$income,
        show.whiskers = F,
        col.loophull = "#aaccff",
        col.looppoints = "#3355ff",
        col.baghull = "#7799ff",
        col.bagpoints = "#000088",
        transparency = T)

my.alpha <- 100
col.vec <- rep(rgb(30,144,255,maxColorValue = 255, alpha = my.alpha), nrow(sales))
# change color for unit price > 10
col.vec[sales$unit.price > 10] <- rgb(64,255,64,maxColorValue = 255, alpha = my.alpha)
# change color for unit price > 14
col.vec[sales$unit.price > 14] <- rgb(255,64,64,maxColorValue = 255, alpha = my.alpha)
plot(sales$expenses, sales$income, col=col.vec)

# plots for Adobe Illustrator
n <- 1000
x <- rnorm(n)
y <- x^2 + rnorm(n, mean =1, sd = 0.25)
plot(c(x, -1.5, 1.5, 0), c(y, 14, 14, 0))

A <- sample(c("here", "there", "nowwhere", "everywhere"), size = n, replace = T)
B <- sample(c("now", "later"), size = n, replace = T)
barplot(table(B,A), beside=T)

pie(table(A))

