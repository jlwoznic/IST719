# Joyce Quiz 8

# Question 3
# data frame (my.df) has 739 rows and one of columns - called date.time contains dates and times.
# The line below contains for the first three lines (out of the 739) dates in the dataset

my.df <- NULL
my.df$id <- c(1, 2, 3)
my.df$date.time <- c("2014, Aug, Fri the 16 at 18:40",  
                     "2014, Jun, Sat the 24 at 11:51",  
                     "2014, Jun, Sun the 25 at 7:22")
my.df <- as.data.frame(my.df)

# define the conversion string
conv.str <- "%Y, %b, %a the %d at %R"
# convert the convert the format of the data in the date column to a new column
my.df$conv.date <- as.POSIXct(strptime(my.df$date.time, conv.str))

# find the minimum date
min.date <- min(my.df$conv.date)
min.date
# find the maximum date
max.date <- max(my.df$conv.date)
max.date

# Question 2
# add mode, x and y to dataframe (I created a new one to be safe)
# x and y are both continuous
my.df <- data.frame(replicate(2,sample(0:200,739,rep=TRUE)))
# Mode has three categories (T, H, Q) 
my.df$mode<- as.factor(replicate(1,sample(c("T","H","Q"),739,rep=TRUE)))
colnames(my.df) <- c("x", "y","mode")
# Write the code to make a scatter plot of x and y where 
# - points related to the category T are a light green
# - points related to H are a dark red 
# - points related to Q are a shade of purple
# Use the rgb function to make your colors, and set the transparency to about 50%. 
# Hint: Find colors on the R color chart, and then use those RGB values. 
#       Recall also that your dataset has 739 rows.

# light green color: (144,238,144) Mode = T
# dark red color: (139,0,0) Mode = H
# purple color: (147,112,219) Mode = Q
# alpha needs to be 50% transparency, so 255/2 = 127.5
my.trans <- 255/2
plot(my.df$x, my.df$y, col = c( rgb(144, 238, 144, my.trans, maxColorValue = 255),
                                rgb(139, 0, 0, my.trans, maxColorValue = 255),
                                rgb(147, 112, 219, my.trans, maxColorValue=255))[my.df$mode], pch = 16)
# add a legend to the plot
legend("topright", c("T","H","Q"), cex = 0.6, 
       fill = c(rgb(144, 238, 144, my.trans, maxColorValue = 255),
                rgb(139, 0, 0, my.trans, maxColorValue = 255),
                rgb(147, 112, 219, my.trans, maxColorValue=255)))


# QUestion 9
plot(rnorm(100), col = "#FF7733", pch = 16, cex = 3)
# the parameter for col could be replaced with (roughly)
plot(rnorm(100), col = rgb(1,5,2,255, maxColorValue = 255), pch = 16, cex = 3)
plot(rnorm(100), col = rgb(3,7,1,255, maxColorValue = 255), pch = 16, cex = 3)
plot(rnorm(100), col = "blue", pch = 16, cex = 3)
plot(rnorm(100), col = "lavenderblush3", pch = 16, cex = 3)

