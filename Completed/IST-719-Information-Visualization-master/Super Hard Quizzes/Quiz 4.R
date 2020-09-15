# Author: Pan Chen
# Purpose: IST719 Quiz 4
#


# 1. Assume you have read in a csv file and that the data is now a dataframe called my.df. Your data has 739 rows and one of the columns contains dates and times. The line below contains the first 3 (our of 739) dates in the dataset:
#   
#   "2014, Aug, Fri the 16 at 18:40" "2014, Jun, Sat the 24 at 11:51" "2014, Jun, Sun the 25 at 7:22"
# 
# Using R code, convert these dates to the data type as.POSIXct and find the min and max dates.
#create a dataframe for demo purposes
df <- NULL
# df<-c(x,date)
#populate the date column as per question
df$date <- c("2014, Aug, Fri the 16 at 18:40", "2014, Jun, Sat the 24 at 11:51", "2014, Jun, Sun the 25 at 7:22")
#convert the format of the data in the date column to a new column
df$date2 <- as.POSIXct(strptime(df$date, "%Y, %b, %a the %d at %R"))
#find the min date
min(df$date2)
#find the max date
max(df$date2)

#QUESTION 2

# Assume that the data from question 1 also has columns called mode, x, and y. Mode has three categories (T, H, Q) and x and y are both continuous. Write the code to make a scatter plot of x and y where points related to the category T are a light green, points related to H are a dark red, and points related to Q are a shade of purple. Use the rgb function to make your colors and set the transparency to about 50%. (HINT find colors on the r color chart and then use those rgb values.)
#create a dataframe with column x,y,mode for demo purposes
df2 <- data.frame(replicate(2,sample(0:100,100,rep=TRUE)))
df2$mode<- replicate(1,sample(c("T","H","Q"),100,rep=TRUE))
colnames(df2) <- c("x", "y","mode")
#convert the mode column to factors
df2$mode<-as.factor(df2$mode)
#As we can see, the mode has three Levels:H Q T
#plot the data with legend
plot(df2$x, df2$y, col=c(rgb(139,0,0,127.5,maxColorValue=255),rgb(160,32,240,127.5,maxColorValue=255),rgb(144,238,144,127.5,maxColorValue=255))[df2$mode],pch=16)
legend("topright", c("H","Q","T"), cex=1.0, bty="n", fill=c(rgb(139,0,0,127.5,maxColorValue=255),rgb(160,32,240,127.5,maxColorValue=255),rgb(144,238,144,127.5,maxColorValue=255)))

#
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)

# 
# If we have two vectors, pressures and temperatures from the same data frame, which are the correct answer(s) for plotting them with points.

A.	
qplot(temperature, pressure, data=pressure, geom=c("line", "point"))

B.	
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()

C.	
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

D.	
A and C
# 
# QUESTION 8
# 
# With respect to working with interactive visualizations,
# which type of interaction is the most frequent, useful and central to the analytic process?
# 
# 
# A.	
# Aggragating
# 
# B.	
# Zooming and Panning
# 
# C.	
# Comparing
# 
# D.	
# Highlighting and filtering
# 
# 1 points   Saved
# QUESTION 9
# 
# Suppose you work at a software company that sells many different types of games.
# You have access to a massive and comprehensive company sales dataset. 
# You are asked to create a visualization showing the sale's of strategy games 
# during the summer broken down by gender, age for each state in the U.S. and 
# they want to particular attention placed on gender. 
# Which visualization features would need to be supported by am interactive 
# visualization software program? 
# 
# A.	
# filtering, highlighting and aggregating
# 
# B.	
# sorting, highlighting and hierarchical navigation 
# 
# C.	
# drilling into details, direct and exploratory navigation
# 
# D.	
# A and C

#Write a piece of R code to find the names of the sales representatives that sold the most units of red wine in each region in 2012. (sales.csv)
#subset the data
sales2012<-subset(sales, year==2012 &type=="red")
list2012=tapply(sales2012$units.sold, list(sales.rep=sales2012$sales.rep,sales2012$rep.region),FUN=sum)
df2012<-as.data.frame(list2012)
#Central region
rownames(df2012[which.max(df2012$Central),])
#East region
rownames(df2012[which.max(df2012$East),])
#North region
rownames(df2012[which.max(df2012$North),])
#South region
rownames(df2012[which.max(df2012$South),])
#West region
rownames(df2012[which.max(df2012$West),])


#Write the code necessary to find out which wine is responsible for the highest recipts in 2014 for each region? (sales.csv)
sales2014<-subset(sales, year==2014)
list2014<-tapply(sales2014$receipt, list(sales2014$wine,sales2014$rep.region),FUN=sum)
df2014<-as.data.frame(list2014)
#Central region
rownames(df2014[which.max(df2014$Central),])
#East region
rownames(df2014[which.max(df2014$East),])
#North region
rownames(df2014[which.max(df2014$North),])
#South region
rownames(df2014[which.max(df2014$South),])
#West region
rownames(df2014[which.max(df2014$West),])





#RColorBrewer loaded already
library(RColorBrewer)
display.brewer.all()

#generate a boxplot with a random dataset
rand.data<-replicate(8,rnorm(35,35,sd=1.5))
boxplot(rand.data, col = brewer.pal(8, "Set1"))

#generate a boxplot with color
num.colors <- 8
my.FUN <- colorRampPalette(c("chartreuse4","azure2","darkorchid1"))
my.cols <- my.FUN(num.colors)
boxplot(rand.data, col=my.cols)
boxplot(rand.data, col = c("#62F24B", "#FF9A4F"))

col.1 <- rgb(255, 154, 79, maxColorValue = 255)
col.2 <- rgb(98,242,75, maxColorValue = 255)
boxplot(rand.data, col=c(col.1, col.2, "#5789E6"))

#a more beautiful scatterplot
plot(sales$receipt,sales$expenses)

colnames(sales)
my.col <-rep(col.1, dim(sales)[1])
my.col[sales$rep.sex ==1] <- col.2
plot(sales$receipt, sales$expenses, col=my.col)

my.col<-rep(col.1, dim(sales)[1])
my.col[sales$type =="red"] <- col.2
plot(sales$receipt, sales$expenses, col=my.col)


col.1 <-rgb(255,154,79, alpha=50, maxColorValue = 255)
col.2 <- rgb(98,242,75, alpha=255, maxColorValue = 255)
my.col <- rep(col.1, dim(sales)[1])
my.col[sales$unit.price>14] <- col.2
plot(sales$receipt, sales$expenses, col=my.col, pch=16)

#generate a list that displays the sum of sales of each type of wine
?aggregate
colnames(sales)
agg.data<- aggregate(sales$units.sold, by=list(type = sales$type, wine=sales$wine), FUN = sum)
barplot(agg.data$x, names.arg = agg.data$wine)

#generate a beautiful barplot the displays how many of each type of wine sold
par(mar=c(5,10,4,2))
barplot(agg.data$x, names.arg = agg.data$wine, las=2, horiz=T)
wine.colors<- c(rgb(255,240,150, maxColorValue = 255), rgb(160,30,65, maxColorValue = 255))
pie(c(10,10), col=wine.colors)

bar.colors <- rep("blue", nrow(agg.data))
bar.colors[agg.data$type =="red"] <- wine.colors[1]
bar.colors[agg.data$type == "white"] <- wine.colors[1]
bar.colors[agg.data$type == "white"] <- wine.colors[2]

barplot(agg.data$x, names.arg = agg.data$wine,las=2, horiz=T, col=bar.colors)

#########
#working with images in plots
#######
#load png

agg.data.receipts <- aggregate(sales$receipt, by=list(type=sales$type,wine= sales$wine), FUN = sum)
                               
options(scipen = 9)
ima<- readPNG("/Users/cpkoywk/Library/Mobile Documents/com~apple~CloudDocs/IST719/Data and Images/bottles.png")
r1 <- readPNG("/Users/cpkoywk/Library/Mobile Documents/com~apple~CloudDocs/IST719/Data and Images/R1.png")
w1 <- readPNG("/Users/cpkoywk/Library/Mobile Documents/com~apple~CloudDocs/IST719/Data and Images/W1.png")

pch <- rep("W", 7)
pch [agg.data$type == "red"] <- "R"
plot(agg.data$x, agg.data.receipts$x)

colnames(agg.data)[3] <- "units"
agg.data$receipt <- agg.data.receipts$x
agg.data

par(mar=c(5.1,4.1,4.1,2.1))
plot(agg.data$units, agg.data$receipt, pch=pch, col=bar.colors, bty ="n", xlab="units sold", ylab = "receipts", xlim = c(0, 1.25 * max(agg.data$units)), ylim=c(0,1.25*max(agg.data$receipt)), main="units sold by receipts", adj=0)

lim <-par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
rect(lim$usr[1], lim$usr[3] , lim$usr[2], lim$usr[4], col = rgb(1,1,1,.85), border="white")

r1.x1 <- agg.data$units[agg.data$type == "red"]
r1.x2 <- r1.x1 +3000
r1.y1 <- agg.data$receipt[agg.data$type =="red"]
r1.y2 <- r1.y1 +65000

rasterImage(r1, r1.x1, r1.y1, r1.x2, r1.y2)

w1.x1 <- agg.data$units[agg.data$type == "white"]
w1.x2 <- w1.x1 +3000
w1.y1 <- agg.data$receipt[agg.data$type =="white"]
w1.y2 <- w1.y1 +65000

rasterImage(w1, w1.x1, w1.y1, w1.x2, w1.y2)

text(agg.data$units + 2000, agg.data$receipt, labels= agg.data$wine, adj = 0, cex=1.2)
