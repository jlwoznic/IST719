# talking about color
# In Class 3

# wine sales

library(RColorBrewer)
my.dir <- "/Users/joycewoznica/IST719/Labs/Lab3/"
sales <- read.csv(file=paste0(my.dir, "sales.csv"), header=TRUE, stringsAsFactors = FALSE)

colnames(sales)
# list of palette names and ranges of colors in those palettes
display.brewer.all()
display.brewer.pal(11, "RdYlBu")

# just create a random bit of information
rand.data <- replicate(8, rnorm(35,35,sd=1.5))
# boxplot using a set from RColorBrewer
boxplot(rand.data, col = brewer.pal(8, "PuRd"))

num.colors <- 8
# fade colors from blue to red
xyz <- colorRampPalette(c("darkorange4", "#006400", "#8B7355"))
my.cols <- xyz(num.colors)
boxplot(rand.data, col=my.cols)
my.cols

plot(1:8, rep(1,8), axes=FALSE, pch=16, cex=4, col=my.cols, xlim = c(0,8))

f <- colorRampPalette(c("red", "blue"))
f(3)
f(9)
f(6)
plot(1:8, rep(1,8), axes=FALSE, pch=16, cex=4, col=f(9), xlim = c(0,8))


my.col.vec <- colorRampPalette(c("red", "white", "blue"))(10)
plot(1:10, rep(1,10), axes=FALSE, pch=16, cex=4, col=my.col.vec, xlim = c(0,10))

# olive color is incorrect
col.lst <- c("blue", "red", "green", "bisque", "grey20", "grey90", "green2", "olived")
plot(1:8, rep(1,8), axes=FALSE, pch=16, cex=4, col=col.lst, xlim = c(0,8))

install.packages("png")
library(png)
my.dir <- "/Users/joycewoznica/IST719/DataSets/Downloads/"
ima <-readPNG(paste0(my.dir,"bottles.png"))
r1 <- readPNG(paste0(my.dir, "r1.png"))
w2 <- readPNG(paste0(my.dir,"w1.png"))

table(sales$wine)
barplot(table(sales$wine))

agg.data <- aggregate()
agg.data <- 
colnames(agg.data) <- c("type", "wine", "units")
#agg.data.receipts <- aggregate((sales$income, list(type=sales$type, sales$wine), FUN=sum)

lim <- par()
lim$usr

