col.vec <- rep(rgb(255, 64, 64, maxColorValue=255), 117)
col.vec[y > 0.5] <- rgb(30,144,255, maxColorValue = 255)
plot(x,y, col=col.vec, pch=16, cex=1)

str(sales)
dim(sales)

d <- density(mtcars$mpg)
d
barplot(d)

density(mtcars$mpg) 
hist(mtcars$mpg) 
polygon(d, col="blue", border="blue") 
plot(d)
d <- density(mtcars$mpg) 
plot(d) 
polygon(d, col="blue", border="black") 

par(mfrow=c(2,2))

setwd("/Users/joycewoznica//IST719/Test")
save(mtcars, file="mymtcars.rda")
dir()

d <- density(mtcars$mpg)

barplot(d)

polygon(d, border="blue")

d <- density(mtcars$mpg)
plot(d)
polygon(d, col="blue", border="black")

