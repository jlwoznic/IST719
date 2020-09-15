#
# Author: Joyce Woznica
# Class: IST 719
# Date: 2/17/2020
# Subject: Lab 6, Week 6 
#
# Lab 6
#---------------- Package Load -------------------
install.packages("ggplot2")
library(ggplot2)

#---------------- Load Data (Sales) --------------
sales <- read.csv("/Users/joycewoznica/IST719/Labs/Lab3/sales.csv", 
                  header=TRUE, stringsAsFactors = FALSE)

#---------------- Understanding gplot -----------
p <- ggplot(sales) #storing the data twice, in sales, and it p$data

# get information about ggplot
class(p)
attributes(p)
p$data
p$layers
p$scales
summary(p)
View(p$data) # opens the sales dataset

#--------------- Plotting with ggplot ------------
# aes
ggplot(sales) + aes(x = expenses) # map expenses to the x-axis - scale appropriately
# or
ggplot(sales, aes(x=expenses))

range(sales$expenses)
plot(sales$expenses)

# if you use sales$expenses or sales$income - you are storing twice, not a best practice
ggplot(sales) + aes(x=expenses, y=income) + geom_point() 

ggplot(sales) +
  aes(x = sales$expenses, y=sales$income) +
  geom_point()

# the way I do it...
p <- ggplot(sales)
p <- p + aes(x=sales$expenses, y=sales$income)
p <- p + geom_point()
p

# let's look at p now (since it is now a plot)
p$data
p$layers
p$theme
p$mapping

# the way I do it...
p <- ggplot(sales)
p <- p + aes(x=sales$expenses, y=sales$income)
p <- p + geom_point(color="red")
p

p <- ggplot(sales)
p <- p + aes(x=sales$expenses, y=sales$income)
p <- p + geom_point(color="blue")
p

# maps the color of the points to be the type of wine
ggplot(sales) + 
  aes(x=sales$expenses, y=sales$income, color=type) + 
  geom_point()

ggplot(sales) + 
  aes(x=expenses, y=income, color=unit.price > 14) + 
  geom_point()

# set aesthetic based on unit price
ggplot(sales) + 
  aes(x=expenses, y=income) + 
  geom_point(color=ifelse(sales$unit.price > 14, "red", "green"))

# ggplot defines its own set of colors
ggplot(sales) + 
  aes(x=expenses, y=income, color=unit.price) + 
  geom_point()

ggplot(sales) + 
  aes(x=expenses, y=income, 
      color=unit.price,
      shape = type,
      alpha = income) + 
  geom_point()

ggplot(sales) + 
  aes(x=expenses, y=income, 
      color=rep.region,
      shape = type,
      alpha = unit.price,
      size = units.sold) + 
  geom_point()

p1 <- ggplot(sales)
p2 <- ggplot(sales, aes(y=income, x=expenses, shape=rep.region))

summary(p1)
attributes(p1)
summary(p2)
attributes(p2)
p1$labels
p2$labels
p1$mapping
p2$mapping

#--------------- Geom with ggplot ------------
ggplot(sales) +
  aes(y=income, x=expenses) +
  geom_point() + geom_rug()

# build a linear model for a line
income.pred <- predict(lm(sales$income~sales$expenses))

ggplot(sales) +
  aes(y=income, x=expenses) +
  geom_point() + geom_line(aes(y=income.pred), color="red", lwd=3)

ggplot(sales) +
  aes(y=income, x=expenses) + aes(y=income.pred) +
  geom_point() + geom_line(color="red", lwd=3)
  
# we will now make a mess
ggplot(sales) +
  aes(y=income, x=expenses) +
  geom_point(color="pink") + 
  geom_rug() + 
  geom_line(aes(y=income.pred)) + 
  geom_line(aes(y=income.pred + 150)) +
  geom_vline(xintercept = 10, color="blue") +
  geom_hline(yintercept = 500, color="orange") +
  geom_abline(intercept = 50, slope=100, color="red", lty = 3, lwd=2)

# loess method
ggplot(sales) +
  aes(y=income, x=expenses) + geom_point() +
  geom_smooth(method="loess")

# default method
ggplot(sales) +
  aes(y=income, x=expenses) + geom_point() +
  geom_smooth()

# overplotting
ggplot(sales) + 
  aes(y=income, x=expenses) + geom_point() +
  geom_bin2d()

ggplot(sales) + 
  aes(y=income, x=expenses) + geom_point() +
  geom_bin2d(bins=50)

# creating price
price <- ifelse(sales$unit.price > 14, "expensive", "moderate")
price[sales$unit.price < 9] <- "cheap"

ggplot(sales) + aes(y=income, x=expenses, color=price) +
  geom_bin2d(bins=50)

#--------------- Different Plots ggplot ------------
df <- aggregate(sales$units.sold, list(year=sales$year), sum)
df2 <- aggregate(sales$units.sold,
                 list(year=sales$year, region=sales$rep.region), sum)

ggplot(sales) + aes(x=income) + geom_blank()
ggplot(sales) + aes(x=income) + geom_histogram()
# shape of income
ggplot(sales) + aes(x=income) + geom_histogram(binwidth=10)
ggplot(sales) + aes(x=income) + geom_histogram(binwidth=10, fill="orange")

ggplot(sales) + aes(x=income) + 
  geom_histogram(binwidth=10, fill="orange") +
  geom_vline(aes(xintercept= mean(income)), 
             color="blue", linetype = "dashed", size = 1)

ggplot(sales) + aes(x=income) + 
  geom_histogram(binwidth=10, fill="orange", alpha=0.9) +
  aes(y = ..density..) +
  geom_density(alpha=0.3, fill = "blue", color="blue")

ggplot(sales) + aes(x="jeff", y=income) + geom_boxplot()
ggplot(sales) + aes(x=rep.region, y=income) + geom_boxplot()

ggplot(df) + aes(x=year, y=x) + geom_line() + ylim(c(0,40000))
ggplot(df) + aes(x=year, y=x) + geom_step() + ylim(c(0,40000))

ggplot(df) + aes(x = year, y = x) + 
  geom_ribbon(ymin = df$x - 1000, ymax = df$x + 1000, fill="yellow") +
  geom_line() + ylim(c(0,40000))

# this is what I want to do for my plot!
ggplot(df2) + aes(x = year, y = x, color = region) +
  geom_line() + ylim(c(0,10000))

#--------------- geom_bar with ggplot ------------
df <- aggregate(sales$units.sold, list(region=sales$rep.region), sum)
colnames(df)[2] <- "sales"

ggplot(sales) + aes(x=rep.region) + geom_bar(fill="orange") +
  ggtitle("Number of sales by region")

ggplot(sales) + aes(x=rep.region) + geom_bar(fill="orange", width=0.5) +
  ggtitle("Number of sales by region")

ggplot(sales) + aes(x=rep.region, fill = type) + geom_bar(position = "dodge") 
ggplot(sales) + aes(x=rep.region, fill = type) + geom_bar(position = "fill") 

ggplot(df) + aes(x = region, y = sales, fill = region) +
  geom_bar(stat = "identity")

# this makes a pie chart - bar is wrapping (coord_polar)
ggplot(df) + aes(x = "", y = sales, fill = region) +
  geom_bar(width=1, stat = "identity") +
  coord_polar("y", start = 45)

#--------------- stats with ggplot ------------
hist(sales$income)

p <- ggplot(sales) + aes(x = income)
p <- p + geom_histogram() + stat_bin(binwidth = 20)
p

p <- ggplot(sales) + aes(x = income)
p <- p+  stat_density()
p

ggplot(sales) + aes(y=income) + stat_boxplot() + geom_boxplot()
# same as leaving off the geom_boxplot or stat_boxplot off

ggplot(sales) + aes(x= expenses, y = income) + stat_bin2d() +
  stat_density_2d(col="red")

ggplot(sales) + aes(x = rep.region) + stat_count()
ggplot(sales) + aes(x = rep.region) + geom_bar()

ggplot(df) + aes( x = region, y = sales) + geom_bar(stat="identity")
ggplot(df) + aes( x = region, y = sales) + geom_bar() + stat_identity()

ggplot(sales) + aes( x = income) +
  geom_histogram(aes(fill= ..count..)) +
  aes(y = ..density..) +
  geom_density(fill = "yellow", alpha = 0.1)





