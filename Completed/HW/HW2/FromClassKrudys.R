# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Author: JH/GK
# Purpose: Using Visualizations to answer business questions
# #
# Data Set: Wine Sales 
#
# Question : What is the relationship between expenses and recipt?
# Question : In general, does one type of wine sell better than the other?
# Question : Which region has the most sales
# Question : How many units were sold by region
# Question : How did the units sold of red verses white wine differ by region
# Question : Is Income growing over time

#
# 1) working with aggragation and tapply
#
# 2) noting the differences between single and multi-dimension plots
#
#  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# now we are going to try to answer questions using visualization
# some of this will require the grouping that we just went through.

fname <- file.choose()
sales <- read.csv(file=fname, header=TRUE, stringsAsFactors = F)

View(sales)
str(sales)
colnames(sales)

#  [1] "X"  "sale.date"    "sales.rep"    "rep.sex"      "rep.region"  
# "rep.feedback" "wine"         "type"         "cost"        
# "unit.price"   "units.sold"   "income"       "expenses"     "year"

# QUESTION: What is the relationship between expenses and income?
# what kind of vars are these?

sales$expenses[1:10]
sales$income[1:10]

# relationships of continuous by continuous data
plot(sales$expenses, sales$income, main = "scatter")

plot(sales$expenses,sales$income,col="orange",
     xlab="Expenses",ylab="income",
     main="Relationship between expenses and income")

# how do we see what kind of relationship it is? TRENDLINE
abline(lm(sales$income ~ sales$expenses), col="red", lwd = 3) # regression line (y~x)

# abline niftyness
abline(h = 400, col = "blue")
abline(v = 9, col = "blue")
rug(x = sales$income, side = 2, col = "orange")
rug(x = sales$expenses, side = 1, col = "orange")

#     rug     http://rfunction.com/archives/390

# QUESTION: What is the relationship between expense and type?
# continuous and categorical

sales$type[1:10]
boxplot(sales$expenses ~ sales$type)
# what is this tilda thing doing?
# boxplot does the grouping for us
# ALSO THIS IS A MULTI DIMENSION PLOT
tapply(sales$expenses, list(sales$type), mean)

#############

#Question : in general, does one type of wine sell better than the other?

unique(sales$wine)  # category
head(sales$units.sold)
head(sales$type)    #  white, red

# type  red, white

# understanding and interpreting boxplots
# https://www.wellbeingatschool.org.nz/information-sheet/understanding-and-interpreting-box-plots

boxplot(sales$units.sold~sales$type,xlab="Wine Type",ylab="Units Sold",
        col=c("palegreen","paleturquoise1"),main="2 dim box plot")

abline(h=mean(sales$units.sold), lty=2,col="red")
units.by.type<- aggregate(sales$units.sold,list(type=sales$type), FUN = sum)
units.by.type

#  or by wine catgy ie, Cab, Chard, Merlot, Pinot, Reisling, Sauv Blanc, Shiraz

boxplot(sales$units.sold~sales$wine,xlab="Wine",ylab="Units Sold",col=c("palegreen","paleturquoise1"),
        main="2 dim box plot")

units.by.wine<- aggregate(sales$units.sold,list(wine=sales$wine), FUN = sum)
units.by.wine

##############

# a sale may include more than one bottle sold
# QUESTION: WHICH REGION SOLD THE MOST UNITS
# look at the data

sales$units.sold[1:10]
sales$rep.region[1:10]

###############   

# QUESTION: are recipts growing over time for each region?
colnames(sales)

# first lets look without region consideration

options(scipen=999)  # disable scientific notation
par(mar=c(5,8,4,2))  #bottom,left,top,right

x<-as.numeric(colnames(M))
x

trec<-tapply(sales$income,sales$year,sum) # total income by year
trec

plot(x,trec,type="l",ylim=c(0,max(trec)),las=2,xlab="Year",
     ylab="",col="deepskyblue2",lwd=3,main="Total Income over time by Year")
mtext(text="income",side=2,line=4)

# change min on y axis

plot(x,trec,type="l",ylim=c(min(trec),max(trec)),las=2,xlab="Year",
     ylab="",col="deepskyblue2",lwd=3,main="Total Income over time by Year")
mtext(text="Income",side=2,line=4)

###  lets look more granular by region

M <- tapply(sales$income,list(sales$rep.region,sales$year),sum)

plot(M[1,], type = "l")
x <- as.numeric(colnames(M))
options(scipen=999)
plot(x, M[1,], type = "l", col="red", lwd=2
     , ylab = "recipts in dollars"
     , xlab = "years"
     , ylim = c(0,max(M)), bty = "n")
lines(x, M[2,], col="blue", lwd=2)
lines(x, M[3,], col="orange", lwd=2, lty=2)
lines(x, M[4,], col="brown", lwd=2, lty=2)
lines(x, M[5,], col="green", lwd=2, lty=2)

# bottomleft  "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
legend('bottomleft', legend = rownames(M), lwd=2
       , lty=1, col=c('red', 'blue', 'orange', 'brown',' green'), bty='n', cex=.75)
# this is a multi dimention plot
# this is a multi-dimension plot
# this is a multi-dimension plot
# this is a multi-dimension plot
# say why

# A word about stringsAsFactors

# open the art dataset
fname.art <- file.choose()

art.1 <- read.csv(file = fname.art, header = TRUE, stringsAsFactors = TRUE)

table(art.1$paper, art.1$paper.type)

colnames(art.1)
water <- art.1[art.1$paper == "watercolor", ]

barplot(tapply(water$units.sold, list(water$paper.type), sum))

art.2 <- read.csv(file = fname.art
                  , header = TRUE
                  , stringsAsFactors = FALSE)

water.2 <- art.2[art.1$paper == "watercolor", ]

barplot(tapply(water.2$units.sold, list(water.2$paper.type), sum))

art.1$paper.type
class(art.1$paper.type)
# factors store numbers
art.2$paper.type


