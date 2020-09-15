#
# Author: Joyce Woznica
# Class: IST 719
# Date: 2/3/2020
# Subject: Lab 4, Week 4 
#
# Lab 4

# load sales data we used before
my.dir <- "/Users/joycewoznica/IST719/Labs/Lab3/"
sales <- read.csv(file=paste0(my.dir, "sales.csv"), header=TRUE, stringsAsFactors = FALSE)

dat.1 <- tapply(sales$units.sold, list(sales$wine), sum)
dat.2 <- tapply(sales$income, list(sales$wine), sum)

# separate the plot field
par(mfrow = c(2,1))
# set the margins B T L R
par(mar = c(0.5,5,4,0.1), cex.lab = 0.8)

# do a plot
barplot(dat.2, xaxt = "n", las = 2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "income on units sold", side = 3, line = 1, cex = 1.3, adj = 0)

# new plot region
par(mar = c(6, 5, 0, 1), cex.lab = 0.8)
# collect output from the barplot
bar.out <- barplot(dat.1, xaxt = "n", las = 2)
# replace spaces with carriage return for names of wine
# this line adds the names of the axis in vertical origin
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)

# break plot space up into shape
M <- matrix(c(1,1,1,
              1,1,1,
              2,2,2),
            nrow = 3, byrow = T)
# to set layout on the plot space
layout(M)
layout.show(2)

par(mar = c(0.5, 5, 4, 0.1), cex.lab = 0.8)
# fills top space
barplot(dat.2, xaxt = "n", las = 2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "income on units sold", side = 3, line = 1, cex = 1.3, adj = 0)
par(mar = c(6, 5, 0, 1), cex.lab = 0.8)
# collect output from the barplot
bar.out <- barplot(dat.1, xaxt = "n", las = 2)
# replace spaces with carriage return for names of wine
# this line adds the names of the axis in vertical origin
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)


# break plot space up into shape 
M <- matrix(c(1,1,1,3,
              1,1,1,4,
              2,2,2,5),
            nrow = 3, byrow = T)
# to set layout on the plot space
layout(M)
# shows the number for each plot I run in the order it will appear in the plot
layout.show(5)
par(mar = c(0.5, 5, 4, 0.1), cex.lab = 0.8)
# fills top space
barplot(dat.2, xaxt = "n", las = 2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "income on units sold", side = 3, line = 1, cex = 1.3, adj = 0)
par(mar = c(6, 5, 0, 1), cex.lab = 0.8)
# collect output from the barplot
bar.out <- barplot(dat.1, xaxt = "n", las = 2)
# replace spaces with carriage return for names of wine
# this line adds the names of the axis in vertical origin
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)

# some more margins and then add some pie charts
par (mar = c(1,1,1,1))
pie(dat.1)
pie(dat.1)
pie(dat.1)

# pick up where left off
dat.3 <- tapply(sales$units.sold, list(sales$type), sum)
dat.4 <- tapply(sales$units.sold, list(sales$rep.region), sum)
dat.5 <- tapply(sales$units.sold, list(sales$year), sum)

# repeat from before
# break plot space up into shape
M <- matrix(c(1,1,1,3,
              1,1,1,4,
              2,2,2,5),
            nrow = 3, byrow = T)
# to set layout on the plot space
layout(M)
# shows the number for each plot I run in the order it will appear in the plot
layout.show(5)
par(mar = c(0.5, 5, 4, 0.1), cex.lab = 0.8)
# fills top space
barplot(dat.2, xaxt = "n", las = 2)
mtext(text = "income", side = 2, line = 4, adj = 0)
mtext(text = "income on units sold", side = 3, line = 1, cex = 1.3, adj = 0)
par(mar = c(6, 5, 0, 1), cex.lab = 0.8)
# collect output from the barplot
bar.out <- barplot(dat.1, xaxt = "n", las = 2)
# replace spaces with carriage return for names of wine
# this line adds the names of the axis in vertical origin
axis(side = 1, at = bar.out, labels = gsub(" ", "\n", names(dat.2)), las = 2)

# some more margins and then add some pie charts
par (mar = c(1,1,1,1))
pie(dat.3)
pie(dat.4)
pie(dat.5)

# split the screen
split.screen(figs = c(2,1))
screen(1)
pie(dat.1)
screen(2)
pie(dat.2)

screen(1, new = F)
mtext("Joyce", side = 3, line = 1)
screen(2, new = F)
mtext("Here", side = 3, line = 1)

# to stop using screens
close.screen(1:2)

# split the screen a little more
split.screen(figs = c(2,1))
screen(1)
pie(dat.1)
screen(2)
pie(dat.2)

screen(1, new = F)
mtext("Joyce", side = 3, line = 1)
screen(2, new = F)
mtext("Here", side = 3, line = 1)
# split the bottom of the screen into more screens
split.screen(c(2,2))
screen()
pie(dat.3)

# now fonts
n <- 500
x <- abs(rnorm(n, 6, 2))
y <- x^2 + rnorm(n, 0, 2*x)

my.par <- par()
my.par$adj
# can set family to change how things look
my.par$family

plot(x,y)
my.par$font
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text")
# change the font
# bold on axes
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text", font = 2)
# italic on axes
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text", font = 3)
# bold italic
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text", font = 4)
# serif font
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text", font = 5)

my.par$font.axis
my.par$font.lab
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text", 
     font.axis = 2, font.lab = 3, font.main = 1)

# set font.family
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text", 
     family = "HersheyGothicEnglish")

par(family = "mono")
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text")
par(family = "serif")
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text")
par(family = "sans")
plot(x,y, main = "Fiddling with Fonts", xlab = "some x label", ylab = "ylab text")

# Bookman Old Style
# Courier

plot(1:10, 1:10, type = "n")
# this does not work because I am running on a Mac!
windowsFonts( A = windowsFont("Arial Black"),
              B = windowsFont("Bookman Old Style"),
              C = windowsFont("Comic Sans MS"),
              D = windowsFont("Symbol"))

# alternative
install.packages ("extrafont")
install.packages ("Rttf2pt1")
library(extrafont)
font_import()
# vector of font family names
fonts()
# Show entire table
fonttable()
# register for PDF output
loadfonts()

text(2, 2, "Hello World Default")
text(3, 3, "Hello World Arial Black", family = "Arial Black")
text(4, 4, "Hello World Verdana", family = "Verdana")
text(5, 5, "Hello World Comic Sans", family = "Comic Sans MS")
text(6, 6, "Hellow World Wingdings", family = "Wingdings")

# examples of usage
#pdf("font_plot.pdf", family="Impact", width=4, height=4)
#plot(mtcars$mpg, mtcars$wt, 
#     main = "Fuel Efficiency of 32 Cars",
#     xlab = "Weight (x1000 lb)",
#     ylab = "Miles per Gallon")
#dev.off()
#
#library(ggplot2)
#p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
#        ggtitle("Fuel Efficiency of 32 Cars") +
#        xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
#        theme(text=element_text(size=16, family="Impact"))
#
#ggsave("font_ggplot.pdf", plot=p,  width=4, height=4)
