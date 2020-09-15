a<-read.csv(file.choose())
a
head(a)


#top 10 timezones

timezone<-as.data.frame(table(a$user_time_zone))
timezonesorted<-timezone[order(-timezone$Freq),]
timezonesortedtop10<-head(timezonesorted,11)
timezonesortedtop10<-timezonesortedtop10[c(2:11),]

#timezone plot


bp<-barplot(timezonesortedtop10$Freq, names.arg=timezonesortedtop10$Var1,main="Top 10 Timezones that Posts the Most Tweets", xlab="Timezone", ylab="Number of Tweets",cex.lab=1,cex.names =0.2,ylim=c(0,15000))
text(bp, timezonesortedtop10$Freq,labels = round(timezonesortedtop10$Freq), pos = 3, cex = 1)



#followersdata dist
qplot(data = a, x = user_followers_count) + xlab("User's followers count")+ylab("Number of Users") + xlim(c(0, 7500))+ggtitle("Histogram of Number of Followers")

#density of number of statuses
b<-a[a$user_statuses_count<20000,]
d <- density(b$user_statuses_count) 
plot(d, xlab="Number of Tweets",main="Density plot of how many tweets sent by users")
polygon(d, col="red", border="blue")


ajhatch@syr.edu

?barplot
order(table(a$user_time_zone))
unique(a$user_time_zone)
