# Author: Pan
# Purpose: HW1, started on my home computer

#Figure 4-11
#open the data file
hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv", 
                    sep=",", header = TRUE)
#check the data frame
hotdogs

#check dogs eaten column
hotdogs$Dogs.eaten
#basic barplot
barplot (hotdogs$Dogs.eaten)
#add label to the x axis
barplot (hotdogs$Dogs.eaten, names.arg = hotdogs$Year)
#add color and names to two axis
barplot (hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col="red", border =NA, xlab="year",
         ylab = "Hotdogs and buns (HDB) eaten")
##only add color to the year the US wins the contest VT pg.92
fill_colors <- c()
for (i in 1: length(hotdogs$Country)){
  if(hotdogs$Country[i] == "United States"){
    fill_colors <-c(fill_colors, "#821122")
  }else{
      fill_colors<-c(fill_colors, "#cccccc")
    }
}

barplot (hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col=fill_colors, border =NA, xlab="year",
         ylab = "Hotdogs and buns (HDB) eaten")
#adding main title to the graph, final product (Figure 4-11)
barplot (hotdogs$Dogs.eaten, main="Nathan's Hot Dog Eating Contest Results, 1980-2010",
         names.arg = hotdogs$Year, col=fill_colors, border =NA, 
         space=0.3,xlab="year",ylab = "Hotdogs and buns (HDB) eaten")

#Stacked bar chart- Fig 4-22
#load data
hot_dog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", 
                    sep=",", header = TRUE)
#change the name of the columns
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", 
                           "2004", "2005", "2006", "2007", "2008", "2009", "2010")
#convert hot_dog_places to Matrix so it could be plotted on barplot
hot_dog_matrix <- as.matrix(hot_dog_places)
#then barplot it, limits of the y-axis go from 0 to 200
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0,200), xlab="Year", ylab ="Hot dogs and buns (HDBs) eaten",
        main="Hot Dog Eating Contest Results, 1980-2010")

# Scatterplot- Fig 4-28
#load data
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv", 
                           sep=",", header = TRUE)
#scatterplot the subscribers data
plot(subscribers$Subscribers)
#specigfy the point type, and set the range of y-axis
plot(subscribers$Subscribers, type="p", ylim=c(0,30000))
plot(subscribers$Subscribers, type="h", ylim=c(0,30000))
#combine the above two
plot(subscribers$Subscribers, type="h", ylim=c(0,30000), xlab="Day", ylab="Subscribers")
points(subscribers$Subscribers, pch=19, col="black")

# Time series- Fig 4-34, continuous data and connecting the dots
#load data
population <- read.csv("http://datasets.flowingdata.com/world-population.csv", 
                        sep=",", header = TRUE)
plot(population$Year, population$Population, xlab="Year", ylab="Population", type="l"
     ,ylim=c(0, 7000000000))


# Step chart- Fig 4-43
#load data
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", 
                       sep=",", header = TRUE)
plot(postage$Year, postage$Price, type="s", main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rat (Dollars)")

#Part 2

# Download art.csv (from the Data and images section) to your (or a lab) 
#machine. Using R, answer the following questions. This work will 
#be similar to what we did for lab 2.
art <- read.csv ("/Users/cpkoywk/Library/Mobile Documents/com~apple~CloudDocs/IST719/Week 2 Data and Images/art.csv",
                 header = TRUE, stringsAsFactors = FALSE)

# Use the par() function to put all 4 plots in the same plot space. 
par(mfrow = c(2,2))
#Also, give the plots titles, x and y labels, use colors that you like,
#and if you are using a plot with points, use a symbol you like (pch). 
#In other words, customize these plots to show me that you know how to 
#modify different elements of the plots.

# What is the distribution of total sales for the whole dataset? Provide two different plots that show two different ways of showing distributions.
plot (art$total.sale, col="red",pch=2, ylim=c(0, 120), 
     main="Plot Distribution of Total Sale",
     ylab="Total Sale (in Dollar amount)", xlab="Sale No.")
barplot (art$total.sale, col="darkgreen", ylim=c(0, 120), 
         main="Barplot of Total Sale",
         ylab="Total Sale (in Dollar amount)", xlab="Sale No.", border=NA)

# What is the distribution of the totals sales for drawing paper?
art.drawing <-  art[art$paper =="drawing",]
hist(art.drawing$total.sale,col="green", main="Histogram Distribution of Total Sale for Drawing Paper", xlab = "Total Sale for Drawing Paper", ylab = "Total Number of Sales")

# What is the distribution of the totals sales for watercolor paper?
art.watercolor <-  art[art$paper =="watercolor",]
hist(art.watercolor$total.sale,col="blue", main="Histogram Distribution of Total Sale for Watercolor Paper", xlab = "Total Sale for Watercolor Paper", ylab = "Total Number of Sales")

# Complete the 5 chart exercises in VT Chapter 4; R PORTIONS ONLY. Do not complete Illustrator exercises yet.
# Upload the VT Chapter 4 plots as one PDF file. This means you will have multiple plots in one PDF file. Name the file: HW1-VT_LastName.pdf
# Upload the plots from the art.csv dataset as a single pdf with the file name HW1-ART_LasteName.pdf






par(mfrow = c(2,2)) #??? With the par( ) function, 
#you can include the option mfrow=c(nrows, ncols) to create 
#a matrix of nrows x ncols plots that are filled in by row.
#mfcol=c(nrows, ncols) fills in the matrix by columns.
hist(tips$tip, col="red")
plot(tips$tip, pch=3)
plot(sort(tips$tip))
boxplot(tips$tip)


#parenthesis is always used for functions, square brackets are used for subset or reference
unique(tips$sex)
male.tips <- tips[tips$sex =="Male",]

my.index <- which(tips$sex=="Female")
female.tips <- tips[my.index,] #what does this comma do again? pass the index in the square brackets with a comma ater the index saying gimme all the columns

par (mfrow = c(1,2))
boxplot (male.tips$tip, ylim=c(0,10), col = "cornflowerblue")
boxplot (female.tips$tip, ylim=c(0,10), col = "pink")

#???
wine.file <- paste (my.desktop, "wine.txt", sep="")
wine <- read.table(wine.file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")

colnames(wine)
barplot(table(wine$rep.region)) #which region sells most wine



#Quiz 2
boxplot (hotdogs$Dogs.eaten)
plot  (hotdogs$Dogs.eaten)
hotdogs [3:4]
dim (hotdogs)
str (hotdogs)
class (hotdogs)
paste(sales.item,sales.count, sep= ":") 

sales.item <- c("tablets", "laptops")

sales.count <- c("45","30")

paste(sales.item, sales.count)

