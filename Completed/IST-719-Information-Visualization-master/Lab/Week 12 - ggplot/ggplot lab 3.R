#author: Pan Chen
#IST-719 Lab 7:ggplot

# Figure 2.2, page 9
#scatterplot
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()


# Figure 2.7, page 13 - we can use ggplot to count for each column
#barchart
g1<-ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
g2<-ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()
grid.arrange(g1,g2,nrow=1)



# Figure 2.11, page 16
#boxchart for multiple cols
g3<-ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
g4<-ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()
grid.arrange(g3,g4,nrow=1)


# Figure 3.4, page 23 (grouping bars together like tapply)
#geom bar is set stat="count" as default
?geom_bar
library(gcookbook) # For the data set
as.data.frame(cabbage_exp)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +geom_bar(stat="identity",position="dodge")

# Figure 3.11, page 29
#coloring negative and positive bars differently
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", position="identity")

# Figure 4.19, page 64
#stacked area graph
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()

# Figure 4.24, page 68
#Proportional Stacked Area Graph
#convert thousands to percent
?ddply
uspopage_prop <- ddply(uspopage, "Year", transform,Percent = Thousands / sum(Thousands) * 100)
ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))
