my.dir <- "/Users/joycewoznica/IST719/Labs/Lab3/"
sales <- read.csv(file=paste0(my.dir, "sales.csv"), header=TRUE, stringsAsFactors = FALSE)
#Write a piece of R code to find the names of the sales representatives that sold the most units of red wine in each region in 2012. (sales.csv)
#subset the data
# first need to subset by 2010 only
sales.2010 <- sales[which(sales$year == "2010"),]
# now get only white wine
sales.2010 <- sales[which(sales.2010$type == "white"),]
# find sum of all sellers by region
list.2010 <- as.data.frame(tapply(sales.2010$units.sold, list(sales.rep=sales.2010$sales.rep,sales.2010$rep.region),FUN=sum))
#Central region
rownames(list.2010[which.max(list.2010$Central),])
#East region
rownames(list.2010[which.max(list.2010$East),])
#North region
rownames(list.2010[which.max(list.2010$North),])
#South region
rownames(list.2010[which.max(list.2010$South),])
#West region
rownames(list.2010[which.max(list.2010$West),])


my.dir <- "/Users/joycewoznica/IST719/Labs/Lab3/"
sales <- read.csv(file=paste0(my.dir, "sales.csv"), header=TRUE, stringsAsFactors = FALSE)
sales.2012 <- sales[which(sales$year == "2012"),]
list.2012 <- as.data.frame(tapply(sales.2012$income, list(sales.2012$wine,sales.2012$rep.region),FUN=sum))
#Central region
rownames(list.2012[which.max(list.2012$Central),])
#East region
rownames(list.2012[which.max(list.2012$East),])
#North region
rownames(list.2012[which.max(list.2012$North),])
#South region
rownames(list.2012[which.max(list.2012$South),])
#West region
rownames(list.2012[which.max(list.2012$West),])
