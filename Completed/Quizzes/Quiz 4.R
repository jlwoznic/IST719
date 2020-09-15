my.dir <- "/Users/joycewoznica/IST719/Labs/Lab3/"
sales <- read.csv(file=paste0(my.dir, "sales.csv"), header=TRUE, stringsAsFactors = FALSE)

# first need to subset by 2010 only
sales.2010 <- sales[which(sales$year == "2010"),]
# now get only white wine
sales.2010 <- sales[which(sales.2010$type == "white"),]
# find sum of all sellers by region
rep.by.region <- as.data.frame(tapply(sales.2010$units.sold,list(sales.2010$sales.rep,sales.2010$rep.region),sum))
# make rep a column
rep.by.region$sales.rep <- rownames(rep.by.region)
rownames(rep.by.region) <- c()
rep.by.region <- melt(rep.by.region)
colnames(rep.by.region) <- c("sales.rep", "rep.region", "units.sold")
# remove NAs
new.df <- rep.by.region[!is.na(rep.by.region$units.sold),]
# get max in each 
# but need to bring back the sale rep name!
df.grouped <- aggregate(new.df$units.sold, list(new.df$sales.rep,new.df$rep.region), max)
# how to get this to spit out the sales.rep!
tapply(new.df$units.sold, new.df$rep.region, max)
# need sales$sales.rep, sales$type and sales$units.sold and rank them in decreasing order


baseball.example <-
  data.frame(team = gl(5, 5,
                       labels = paste("Team", LETTERS[1:5])),
             player = sample(letters, 25),
             batting.average = runif(25, .200, .400))
summary(baseball.example)
tapply(baseball.example$batting.average, baseball.example$team,
       max)


# highest sales receipts
my.dir <- "/Users/joycewoznica/IST719/Labs/Lab3/"
sales <- read.csv(file=paste0(my.dir, "sales.csv"), header=TRUE, stringsAsFactors = FALSE)
tapply (sales$income, sales$wine, max)
