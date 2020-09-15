
#Viz-a-thon
#IST 719

#team members:
##Adrian Hatch
##Pan Chen
##Xiao Song
##Suchitra Deekshitula 


climate = read.csv("ClimateTweets100k(1).csv", header = TRUE, stringsAsFactors = FALSE)

install.packages("lubridate")
library(lubridate)

View(head(climate))
dim(climate)
colnames(climate)
str(climate)

# plot of follower count vs age of the account
climate$today = Sys.Date()
climate$month = substr(climate$user_created_at, 5, 7) 
climate$year = substr(climate$user_created_at, 27, 30) 
climate$date = substr(climate$user_created_at, 9, 10)
climate$user_create_date =  paste( climate$year , climate$month , climate$date,sep = "-" )  #, format = "%Y-%b-%d" )
climate$user_create_date = parse_date_time(climate$user_create_date, orders = "ymd")

climate$user_age = difftime(climate$today , climate$user_create_date,units = "days"  )
climate$user_age_month = climate$user_age/30
options(scipen =999)
plot(climate$user_age_month, log(climate$user_followers_count), col = "#50A5DC", alpha = 200, xlab = " user account age", ylab = "log of the number of followers")

#plot of median number of users by user verified status
table(climate$user_is_verified)
climate$id= seq.int(nrow(climate))
m = tapply(climate$user_followers_count, climate$user_is_verified, median)
m
barplot(m, xlab = "user account verified", ylab = "median number of followers")


#wordcloud 
library(NLP)
library(tm)
library(stringr)
library(RColorBrewer)
library(wordcloud)

# read the file
tmp <- file.choose()
myfile <- read.csv(tmp, header = T, stringsAsFactors = F)

colnames(myfile)

# remove rows where there is no hashtags
hashset <- myfile [!(myfile$hashtags==""), ]
hashset <- hashset[hashset$is_retweet == "True",]

# a function to clean the hashtag text
clean_string <- function(string){
  # Lowercase
  temp <- tolower(string)
  # replace the pipe with white space
  temp <- str_replace_all(temp, "[|]"," ")
  # Shrink down to just one white space
  temp <- str_replace_all(temp,"[\\s]+", " ")
  return(temp)
}

# clean the hashtag
cleaned_hash <- clean_string(hashset$hashtags)
length(cleaned_hash)
cleaned_hash[4]

# convert string to vector
hash.vec <- VectorSource(cleaned_hash)
# convert vector to corpus
hash.corpus <- Corpus(hash.vec)
inspect(hash.corpus[2])

# convert corpus to a term document matrix
hash_tdm <- TermDocumentMatrix(hash.corpus)
hash_tdm
inspect(hash_tdm[1:10,1:10])
hash_tdm.m <- as.matrix(hash_tdm)

# calculate the word frequency in decresing order
hash.wordCounts <- rowSums(hash_tdm.m)
length(hash.wordCounts)
hash.wordCounts <- sort(hash.wordCounts, decreasing=TRUE)
head(hash.wordCounts)

# convert the matrix to a dataframe
hashfreq<- as.data.frame(hash.wordCounts)
hashfreq <- data.frame(token = row.names(hashfreq), freq = hashfreq$hash.wordCounts)

# create a word cloud of top 100 hashtags
tophash <- hashfreq[1:100,]
mycolors <- colorRampPalette(c("#156400","#1E8603","#30A312","#4DBE2F","#afafaf"))
wordcloud(tophash$token, tophash$freq,
          scale = c(2.2,1),
          random.order = FALSE,ordered.colors=TRUE,
          random.color = FALSE, rot.per = 0.3,
          colors = mycolors(length(tophash$token)))


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
