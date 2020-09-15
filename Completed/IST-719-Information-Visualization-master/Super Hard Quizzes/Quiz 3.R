# Author: Pan Chen
# Purpose: IST719 Lab5 - color
#

data.fname <-file.choose()
sales <- read.csv(data.fname, header= TRUE, stringsAsFactors = FALSE)
dim(sales)

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
