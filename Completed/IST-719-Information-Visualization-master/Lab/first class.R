#
# Author: pan
# Purpose: Lab
#

pie(c(10,17,7,20))

pie(c(10,17,7,20), main="Pan Loves Pie")

pie(c(10,17,7,20), main="Pan Loves Pie"
    , labels = c("sales", "Marketing", "Management", "Data Team"))

pie(c(10,17,7,20), main="Pan Loves Pie"
    , labels = c("sales", "Marketing", "Management", "Data Team")
    , col = c("red", "tan", "brown", "orange"))

plot(c(1,2,4,1.5,1,3))

plot(c(1,2,4,1.5,1,3), pch = 8)

plot(c(1,2,4,1.5,1,3), pch = 4, cex = 2)

plot(c(1,2,4,1.5,1,3)
     , pch = c(1,2,3,4,5,6)
     , cex = c(1,2,1,2,2,1)
     , type = "l"
     , lwd = 3
     , col = "orange"
     , main = "da second plot"
     )

my.bucket <- rnorm(n = 1000)
par(bty="n")
plot(my.bucket, type="h", lwd=4, col = "cornflowerblue"
     , lend=2)

hist(my.bucket, col = "gold")

n <- 50
my.var <- sample(letters[1:3], size = n, replace= TRUE)

my.table <- table(my.var)

barplot(my.table)

barplot(my.table, col = c(2,3,4), space=.1, main= "lol", sub=2, border=NA,width=1)
