# Author: Pan
# Purpose: IST719 HW2

#choose the file, as "tmp"
tmp <- file.choose()
artdata <- read.csv (file = tmp, header = TRUE, stringsAsFactors = FALSE)
str(artdata)
par(mfrow = c(2,3))
# 1.	What are the distributions of total sales for water color and drawing papers? Show this as a grouped boxplot (two boxplots in the same plot).
artdata$total.sale
artdata$paper
boxplot(artdata$total.sale ~ artdata$paper, col=c("yellow","lightblue"), xlab = "Paper Material",  ylab = "Total Sales in $", main="Sales and Paper Material")

# 2.	Are total sales growing for each store over the years covered in the dataset? Show as a multi-line plot.
M2 <- tapply(artdata$total.sale,list(artdata$store, artdata$year),sum)
x2 <- as.numeric(colnames(M2))
plot(x2,xaxt="n", M2[1,], type ="l", col="red", xlab = "Year", ylim = c(0, max(M1)), ylab="Total Sales in $")
axis(1, xaxp=c(2012, 2015, 3))
lines(x2, M2[2, ], col = "blue")
lines(x2,M2[3, ], col = "orange")
lines(x2,M2[4, ], col = "pink")
mtext (text="Sales by Store Location", side =3, line =1.5, cex=0.5, adj=0)
legend('bottomright', legend = rownames(M1), lwd=2, lty=1, bty="n", col = c("red","blue", "orange", "pink"),cex=0.75)

# 3.	How do the total units sold of water color and drawing paper differ by store? Do stores tend to sell the same ratios of each?
M3<-tapply(artdata$units.sold,list(artdata$paper,artdata$store),sum)
barplot(M3, beside=T,col=c("red","lightblue"), main="Units Sold by Store Location", ylab="Units Sold", xlab="Store Location")
legend('topleft', legend = rownames(M3), lwd=2, lty=1, bty="n", col = c("red","lightblue"),cex=0.58)

# 4.	Each paper (watercolor and drawing) has different subtypes. For watercolor only, how are the total sales of the different paper types (column is paper.type) similar or different for each store? (hint: make a watercolor subset of the whole dataset, then show a grouped barchart using a matrix from tapply)
artdata$paper.type
artdata2 <- artdata[artdata$paper=="watercolor", ]
M4 <- tapply(artdata2$total.sale,list(artdata2$paper.type,artdata2$store),sum)
barplot(M4, beside=T, col=c("red","floralwhite", "orange","green"), main="Total Sales of Watercolor's \nSubtypes", ylab="Total Sales in $", xlab="Store Location")
legend('topleft', legend = rownames(M4), lwd=2, lty=1, bty="n", col = c("red","floralwhite", "orange","green"),cex=0.58)

# 5.	In the Davenport store, do the sales representatives (column is 'rep') tend to sell the same amounts of water color and drawing paper?
artdataDavenport <- artdata[artdata$store=="Davenport", ]
M5<-tapply(artdataDavenport$units.sold,list(artdataDavenport$paper,artdataDavenport$rep),sum)
barplot(M5, beside=T, col=c("orange","green"), main="Sales by Different Reps", ylab="Total Units Sold", xlab="Sale Rep")
legend('topleft', legend = rownames(M5), lwd=2, lty=1, bty="n", col = c("orange","green"),cex=0.75)

# 6.	Over the years, does the ratio of units sold for water color and drawing paper stay the same? Is one growing while the other stays constant?
proportion <- function(vec) {
  percentage <- vec/sum(vec)
  return(percentage)
}
M6 <- tapply(artdata$units.sold,list(artdata$paper, artdata$year),sum)
x6 <- as.numeric(colnames(M6))

M8<-apply(M6,2,proportion)
plot(x6, M8[1, ], xaxt="n",type ="l", col="red", xlab = "Year", ylim = c(0, 1), ylab="Ratio of Units Sold")
lines(x6,M8[2, ], col = "blue")
mtext (text="Ratio of Units Sold Over the Years", side =3, line =2, cex=0.7, adj=0)
?legend('bottomright', legend = rownames(M8), lwd=3, lty=1, bty="n", col = c("red","blue"))
axis(1, xaxp=c(2012, 2015, 3))


# 
# barplot(M6, beside=F, col=c("yellow","lightblue"), main="Units Sold by Different \nPaper Materials Over Years", ylab="Total Units Sold", xlab="Year")
# legend('topleft', legend = rownames(M6), lwd=2, lty=1, bty="n", col = c("yellow","lightblue"),cex=0.5)
