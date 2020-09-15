#
#  GK
#  Week 3 Synch Code
#  color 
#


fileName <- "Fictional_sales_data.csv"
#    or
fileName <- file.choose()

sales <- read.csv(file=fileName, header=TRUE, sep=",",stringsAsFactors = F)
colnames(sales)

#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(11,'RdYlBu')  # no of colors in palatte, palette name

#  http://colorbrewer2.org
#  https://moderndata.plot.ly/create-colorful-graphs-in-r-with-rcolorbrewer-and-plotly/
#  https://www.r-bloggers.com/r-using-rcolorbrewer-to-colour-your-figures-in-r/
#  https://data.library.virginia.edu/setting-up-color-palettes-in-r/

#  https://www.colorcodehex.com/1e90ff/                    search by hex
#  https://www.rapidtables.com/web/color/RGB_Color.html    search by RGB

rand.data <- replicate(8, rnorm(35,35,sd=1.5)) # num of arg, mean, stddev
rand.data
boxplot(rand.data, col=brewer.pal(8, "PuRd"),main="Random Data Distribution")
#                            numberof colors, name of palette

#         brewer.pal(8,"PuRd")   the number of colors you want and the pallette


#  colorRampPalette: Take a palette of colors and return a function that 
#  takes integer arguments and returns a vector of colors interpolating the palette
#  see my notes at end of code


num.colors <- 8
FUN <- colorRampPalette(c("darkorange4","#006400","#8B7355"))
my.cols <- FUN(num.colors)
my.cols

# let's see what colors look like
plot(1:8, rep(1,8), axes = FALSE, pch=16, cex=4, col=my.cols, xlab=NA, ylab=NA,
     xlim = c(0, 8))

boxplot(rand.data,col=my.cols)

# [1] "X"            "sale.date"    "sales.rep"    "rep.sex"      "rep.region"   "rep.feedback"
#[7] "wine"         "type"         "cost"         "unit.price"   "units.sold"   "income

plot(sales$expenses,sales$income)
col.vec <- rep(rgb(30,144,255,maxColorValue=255)  # creates 10000 color rgb 30,144,255
               , dim(sales)[1])
col.vec    # go to colorcodehex site with #1E90FF
plot(sales$expenses,sales$income,col=col.vec)

#  what's the gender prevalence of sales, male or female

col.vec <- rep(rgb(30,144,255,maxColorValue=255)  # same code as above, redundant
               , dim(sales)[1])
col.vec[sales$rep.sex == 0] <- rgb(255,64,64,maxColorValue = 255)
plot(sales$expenses,sales$income,col=col.vec)  # shows gender concentration

#  0 Female
#  rgb(255,64,64)  red     go to rgb color site


# what's the wine type prevalence of sales, red or white

col.vec <- rep(rgb(30,144,255,maxColorValue=255) # reset to blue
               , dim(sales)[1])
col.vec[sales$type == "red"] <- rgb(255,64,64,maxColorValue = 255)
plot(sales$expenses,sales$income,col=col.vec)  # shows red vs white wine concentration

#  type red
#  rgb(255,64,64)  red
#  blue is white wine


#  what's the price concentration relative to receits & expenses at $14/bottle

col.vec <- rep(rgb(30,144,255,maxColorValue=255)  # reset to blue
               , dim(sales)[1])
col.vec[sales$unit.price > 14] <- rgb(255,64,64,maxColorValue = 255)
plot(sales$expenses,sales$income,col=col.vec)   # shows price concentration 
#                                               # relative to recipts and expenses

col.vec <- rep(rgb(30,144,255,maxColorValue=255)  # reset to blue
               , dim(sales)[1])
col.vec[sales$unit.price > 14] <- rgb(255,64,64,maxColorValue = 255)
col.vec[sales$unit.price < 10] <- rgb(64,255,64,maxColorValue = 255)  # cheap wine <10
plot(sales$expenses,sales$income,col=col.vec)   # shows price concentration 
#                                               # relative to recipts and expenses


#  unit price > 14
#  rgb(255,64,64)  red


#hist(sales$unit.price)

###########

col.vec <- rep(rgb(30,144,255,maxColorValue=255)  # reset to blue
               , dim(sales)[1])
col.vec[sales$unit.price > 14] <- rgb(255,64,64,maxColorValue = 255)
plot(sales$expenses,sales$income,pch = 16, cex = 1.5)

over.plotting.cols <- rgb(0.8,0.15,0.15,alpha = 0.1)
plot(sales$expenses,sales$income,pch = 16, cex = 1.5,   #  cex larger
     col=over.plotting.cols)

over.plotting.cols <- rgb(0.8,0.15,0.15,alpha = 0.1)
plot(sales$expenses,sales$income,pch = 16, cex = 0.5,    #  cex  smaller
     col=over.plotting.cols)

#############

table(sales$wine)   # Reisling, Pinot, Chard,,,,,,   transactions
barplot(table(sales$wine))   # wine category Cab, Chard, Pinot   single dim plot

##  aggregate returns a data frame, tapply returns a matrix

agg.data <-aggregate(sales$units.sold, list(type = sales$type, wine = sales$wine)
                     , FUN = sum)
agg.data

barplot(agg.data$x,names.arg=agg.data$wine)  # no color
wine.colors <- c(rgb(255,240,150,maxColorValue = 255)   # red
                 ,rgb(160,30,65,maxColorValue = 255))   # white
wine.colors

pie(c(20,20), col=wine.colors)   #to check the colors

bar.colors <- rep("white",nrow(agg.data))   # initialize bar.colors
bar.colors
bar.colors[agg.data$type == "white"] <- wine.colors[1]  ##FFF096 white hex value
bar.colors
bar.colors[agg.data$type == "red"] <- wine.colors[2]     #A01E41 red hex value
bar.colors

barplot(agg.data$x,names.arg = agg.data$type,col=bar.colors,main="units sold") #to verify the wine colors
barplot(agg.data$x,names.arg = agg.data$wine,col=bar.colors,main="units sold")

par(mar = c(4, 10, 4, 2))
barplot(agg.data$x, names.arg = agg.data$wine
        , col=bar.colors, border = NA
        , horiz = T, las = 1, space = .8
        , main="units sold")


#####
#install.packages("png")
library(png)

ima <- readPNG(paste0("bottles.png"))  # look at image (background)
r1 <- readPNG(paste0("R1.png"))        # red glass of wine
w1 <- readPNG(paste0("W1.png"))        # white glass of wine
options(scipen=999)   # turn off scientific notation

#pch <- rep("w",7)
#pch
#pch[agg.data$type == "red"] <- "R"
#pch

agg.data.recipts <- aggregate(sales$income,              
                              list(type=sales$type, sales$wine),
                              FUN = sum)
agg.data.recipts
colnames(agg.data) <- c("type","wine","units")   
agg.data
agg.data$recipts <- agg.data.recipts$x  # add recipts (Rev) to df agg.data
agg.data

plot(agg.data$units,agg.data$recipts, bty = "n"      # template for image visual
     ,pch = 15, cex =2, col = bar.colors,    # pch 15 = square
     xlim=c(0, (1.25*max(agg.data$units))),
     ylim=c(0,(1.25*max(agg.data$recipts))),
     xlab="Units Sold", ylab ="Recipts",
     main = "IST 719 Simulated Wine Sales Dataset")

text(agg.data$units + 2000,
     agg.data$recipts
     ,labels = agg.data$wine,
     adj=0,
     cex=1.2)


# where do we put the images?
# we need the coordinates for the plot space

lim <- par()


# par can be used to set or query graphical parameters
# Each device has its own set of graphical parameters
# 

# par() (no arguments) or par(no.readonly = TRUE) 
# is used to get all the graphical parameters (as a named list). 
# Their names are currently taken from the unexported 
# variable graphics:::.Pars

# # Determine plot boundaries, 
# xmin <- par("usr")[1]
# xmax <- par("usr")[2]
# ymin <- par("usr")[3]
# ymax <- par("usr")[4]


# note that when we put a plot on the screen, we use lim$usr
# in a non intuitive way: 1, 3, 2, 4
# this is becuase the plot lim$usr returns  c(x1, x2, y1, y2)
# but the function wants xleft, ybottom, xright, ytop,

# rasterImage(image, xleft, ybottom, xright, ytop, angle=0, interpolate=TRUE)

rasterImage(ima,lim$usr[1],lim$usr[3],lim$usr[2],lim$usr[4])

rect(lim$usr[1],lim$usr[3],lim$usr[2],lim$usr[4], ## draws a rectangle
     col=rgb(1,1,1,.85),border="white")          ## .85 is the transparency value   

# An alpha transparency value can also be specified
# (as an opacity, so 0 means fully transparent and max means opaque).
# If alpha is not specified, an opaque colour is generated.

agg.data

#  create raster image values xleft, ybottom, xright, ytop
#  red wine glass

r1.x1 <- agg.data$units[agg.data$type == "red"]
r1.x1                                             #   xleft  units
r1.x2 <-r1.x1 + 3000
r1.x2                                             #   xright  units                                        
r1.y1 <- agg.data$recipts[agg.data$type == "red"]
r1.y1                                             #   ybottom recipts
r1.y2 <-r1.y1 + 65000
r1.y2                                             #   ytop recipts
rasterImage(r1,r1.x1,r1.y1,r1.x2,r1.y2)

# rasterImage(image, xleft, ybottom, xright, ytop, angle=0, interpolate=TRUE)
# The positions supplied, i.e., xleft, ., are relative to the current plotting region.
# If the x-axis goes from 100 to 200 then xleft should be larger than 100 
# and xright should be less than 200.

# recap
# r1  red wine glass image       Cab      Merlot  Shiraz
r1.x1  # units of red            22236    47315    9616            xleft
r1.y1  # recipts of red          271360.8 356437.1 194939.2        ybottom
r1.x2  # units of red + 3000     25236    50315    12616           xright (wide)
r1.y2  # recipts of red + 6500   336360.8 421437.1 259939.2        ytop  (tall)


agg.data

w1.x1 <- agg.data$units[agg.data$type=="white"]
w1.x1
w1.x2<- w1.x1 + 3000
w1.x2
w1.y1 <- agg.data$recipts[agg.data$type=="white"]
w1.y1
w1.y2<- w1.y1 + 65000
w1.y2
rasterImage(w1,w1.x1,w1.y1,w1.x2,w1.y2)

text(agg.data$units + 2000,
     agg.data$recipts
     ,labels = agg.data$wine,
     adj=0,
     cex=1.2)



#    Miscellaneaous Stuff
#
#    code to display color by name, hex, rgb
#
col.lst <- c("blue" , "red", "green" , "bisque", "grey20", "grey90", "green2", "olivedrab2")
plot(1:8, rep(1,8), axes = FALSE, pch=16, cex=8, col=col.lst, xlab=NA, ylab=NA,
     xlim = c(0, 10))
#axis(1, at=1:8, labels = sprintf("%s", col.lst), col="white", cex=0.6, padj=-3)

col.lst <- c("#FFFFCC","#C7E9B4","#7FCDBB","#40B6C4","#2C7FB8" ,"#253494")
plot(1:6, rep(1,6), axes = FALSE, pch=16, cex=8, col=col.lst, xlab=NA, ylab=NA,
     xlim = c(0, 10))
#axis(1, at=1:8, labels = sprintf("%s", col.lst), col="white", cex=0.6, padj=-3)

col.lst <- c(rgb(1,1,1,1,maxColorValue=255),rgb(197,27,138,maxColorValue=255),rgb(240,59,32,maxColorValue=255),"#40B6C4","#2C7FB8" ,"#253494")
plot(1:6, rep(1,6), axes = FALSE, pch=16, cex=8, col=col.lst, xlab=NA, ylab=NA,
     xlim = c(0, 10))
#
#
# Experiment
# 
col.lst <- c("#FF0000" ,"#DF001F", "#BF003F" ,"#9F005F", "#7F007F", "#5F009F", "#3F00BF", "#1F00DF", "#0000FF")
plot(1:9, rep(1,9), axes = FALSE, pch=16, cex=4, col=col.lst, xlab=NA, ylab=NA,
     xlim = c(0, 10))



# Gender  0, 1

wine.data <- tapply(sales$units.sold,list(sales$rep.sex),FUN=sum)
wine.data
#whle using tapply, put the smaller category variable before larger
barplot(wine.data,col=c("#FF4040","#1E90FF"),beside = T,
        main="Comparison of Wine Sales by Gender",
        legend.text = c("Female","Male"),args.legend=list(x="topright",bty="s"))

units.by.sex<- aggregate(sales$units.sold,list(type=sales$rep.sex), FUN = sum)
units.by.sex

#
#
#    colorRampPalette lesson
#
# 

# colorRampPalette takes any number of colors. 
# I used a mix of color name and hex colors to show 
# that easily deals with both. (above code)
# colorRampPalette then returns a function (run FUN to see).
# The function in FUN will build a palette of colors 
# that slides through the vector of colors passed to colorRampPalette.

#So, 

f <- colorRampPalette(c("red", "blue")) 

# will build a function that will take a number 
# and build a set of colors that runs from red to blue. so

f(3)  # red ,,,, blue
f(6)  # red ,,,, blue
f(9)  # red ,,,, blue

# will return 3, 6, 9 colors in hex values that run from red to purple to blue. 


##  example
f <- colorRampPalette(c("red", "blue"))
my.cols<-f(3)
# let's see what colors look like
plot(1:3, rep(1,3), axes = FALSE, pch=16, cex=4, col=my.cols, xlab=NA, ylab=NA,
     xlim = c(0, 3))




# If you give colorRampPalette 3 colors than 
# it will slide from the first, through the
# middle and to the end. So the ends of the 
# vector are the color anchors on either side, 
# but anything in between them will be slide through.


My.col.vec <- colorRampPalette(c("red", "white", "blue"))(10)
My.col.vec

plot(1:10, rep(1,10), axes = FALSE, pch=16, cex=6, col=My.col.vec, xlab=NA, ylab=NA,
     xlim = c(0, 10))

# would give me 10 colors that start at red, with white in the middle and blue on the end.





# pch Plotting Symbols
# Use the pch= option to specify symbols to use when plotting points. 

# 16 filled dot

# cex 	number indicating the amount by which 
# plotting text and symbols should be scaled 
# relative to the default. 
# 1=default, 1.5 is 50% larger, 0.5 is 50% smaller, etc. 

