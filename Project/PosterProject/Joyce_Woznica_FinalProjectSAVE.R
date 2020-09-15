# 
# Course: IST719
# Name: Joyce Woznica
# Project Code: Equine Death and Breakdown Final Project
# Due Date: 
#
# ----------------  Package Section --------------------------------------------------
#specify the packages of interest
packages=c("reshape2", "ggplot2", "dplyr", "RColorBrewer", "lubridate", "stringr", "tm",
           "wordcloud", "alluvial", "treemap", "ggmap", "tidyverse", "jsonlite", "viridis")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()
require(dplyr)

install.packages("plyr",dependencies=TRUE)
library(plyr)

#------------------ Functions ----------------------------------------------------

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

#dplyr will be used to stack lists together into a data.frame and to get the pipe operator '%>%'
suppressPackageStartupMessages(library(dplyr))

NewLatLon<-function(addresses){
  d <- suppressWarnings(lapply(addresses, function(address) {
    #set the elapsed time counter to 0
    t <- Sys.time()
    #calling the nominatim OSM API
    api_output <- nominatim_osm(address)
    #get the elapsed time
    t <- difftime(Sys.time(), t, 'secs')
    #return data.frame with the input address, output of the nominatim_osm function and elapsed time
    return(data.frame(address = address, api_output, elapsed_time = t))
  }) %>%
    #stack the list output into data.frame
    bind_rows() %>% data.frame())
  #output the data.frame content into console
  return(d)
}

# helpful functions for removing spaces
# need to clean out extra spaces from ends of lines
trim.leading<-function(x) {sub("^\\s+","",x)}
trim.trailing<-function(x) {sub("\\s+$","",x)}
trim<-function(x) {sub("^\\s+|\\s+$","",x)}
trimCity<-function(x) {sub("\\,.*$","",x)}

# maybe add weather station?
# should have just put this in a excel file!!!
trackAddress <- function(track.df)
{
  track.address <- NULL
  if (track == "Aqueduct Racetrack (NYRA)") track.address <- c("110-00 rockaway blvd", "south ozone park", "ny", "11420")
  if (track == "Batavia Downs") track.address <- c("8315 park road", "batavia", "ny", "14020")
  if (track == "Belmont Park (NYRA)") track.address <- c("2150 hempstead turnpike", "elmont", "ny", "11003")
  if (track == "Buffalo Raceway") track.address <- c("5600 mckinley parkway", "hamburg", "ny", "14075")
  if (track == "Finger Lakes Gaming & Racetrack") track.address <- c("5857 ny-96", "farmington", "ny", "14425")
  if (track == "Monticello Raceway & Mighty M Gaming") track.address <- c("204 ny-17b", "monticello", "ny", "12701")
  if (track == "Saratoga Racecourse (NYRA)") track.address <- c("267 union ave", "saratoga springs", "ny", "12866")
  if (track == "Saratoga Gaming & Raceway") track.address <- c("342 jefferson street", "saratoga springs", "ny", "12866")
  if (track == "Tioga Downs") track.address <- c("2384 w river road", "nichols", "ny", "13812")
  if (track == "Vernon Downs") track.address <- c("4229 stuhlman road", "vernon", "ny", "13476")
  if (track == "Yonkers Raceway") track.address <- c("810 mclean ave", "yonkers", "ny", "10704")
  track.address
}

# should have just put this in a excel file!!!
SetTrackID <- function(track.df)
{
  id.df <- NULL
  if (track == "Aqueduct Racetrack (NYRA)") id.df <- c(1)
  if (track == "Batavia Downs") id.df <- c(2)
  if (track == "Belmont Park (NYRA)") id.df <- c(3)
  if (track == "Buffalo Raceway") id.df <- c(4)
  if (track == "Finger Lakes Gaming & Racetrack") id.df <- c(5)
  if (track == "Monticello Raceway & Mighty M Gaming") id.df <- c(6)
  if (track == "Saratoga Racecourse (NYRA)") id.df <- c(7)
  if (track == "Saratoga Gaming & Raceway") id.df <- c(8)
  if (track == "Tioga Downs") id.df <- c(9)
  if (track == "Vernon Downs") id.df <- c(10)
  if (track == "Yonkers Raceway") id.df <- c(11)
  id.df
}

# ----------------- Data Loading -------------------------------------------------
rData <- read.csv("/Users/joycewoznica/IST719/Project/Equine_Death_and_Breakdown.csv", 
                  header = TRUE, na.strings = "NA", 
                  stringsAsFactors = FALSE)

# review missing data
tMissing <-sum(is.na(rData))
cat("The number of missing values in Equine Death and Breakdown Data is ", tMissing)
rData$Weather.Conditions[is.na(rData$Weather.Conditions)] <- "Clear" 

# since 2020 is not complete, remove all rows with Year = 2020
index <- which(rData$Year != "2020")
rData <- rData[index,]

# --------------- Interrogte Data ---------------------------------------------------
# force no scientific notation
options(scipen=999)
# General Statistics and Analysis
summary(rData)
str(rData)

# A little more summary information
test<-as.matrix(rData)
summary(test)

unique(rData$Inv.Location)

# for Stats in Title
# what is percent of total incidents that are equine death
for.perc <- table(rData$Incident.Type)
for.perc <- as.data.frame(for.perc)
colnames(for.perc) <- c("Incident.Type", "Frequency")
total.incidents <- sum(for.perc$Frequency)
perc.death <- 100*(for.perc[which(for.perc$Incident.Type == "EQUINE DEATH"),2]/total.incidents)

# find percent that happen on track
str(rData)
unique(rData$Racing.Type.Description)
for.perc <- table(rData$Racing.Type.Description)
for.perc <- as.data.frame(for.perc)
colnames(for.perc) <- c("Racing.Type.Description", "Frequency")
total.incidents <- sum(for.perc$Frequency)
perc.racing <- 100*(for.perc[which(for.perc$Racing.Type.Description == "Racing"),2]/total.incidents)

# -------------- Bar Chart - Frequency of Incidents by Type -------------------
# ?? What is the most common incident happening on racetracks ??
# SORT PLOT BY BIGGEST TO SMALLEST
inc.type <- table(rData$Incident.Type)
inc.type <- as.data.frame(inc.type)
colnames(inc.type) <- c("Incident.Type", "Frequency")
num.colors <- length(inc.type$Incident.Type)
# gives error, in future, pick greens, tans, beige, and browns
# need to change colors
my.cols <- brewer.pal(num.colors, "BrBG")
# determine tick marks
tlen <- 1500
nticks <- 10
tick.dist <- tlen/nticks
ticks <- seq(0, tlen, by = tick.dist)

par(mar=c(5,12,4,2))
my.bar <- barplot(sort(table(rData$Incident.Type)), col=my.cols,
                  xlab = "Incident Frequency",
                  main = "Number of Incidents by Type",
                  las = 1, cex.names = 0.65,
                  horiz = TRUE, xaxt = "n",
                  xlim = c(0, tlen), 
                  args.legend = list(x = "topleft", bty = "n", cex=0.75))
# enter correct axis tick marks
axis(1, at = ticks)
# just change white bar to RED in Adobe

# --------------  Pie Chart - Frequency of Incidents by Track -------------------
# ?? Are certain tracks having more issues than others?
# aggregate to get just track, death, total incidents in new dataframe
for.pie<-as.data.frame(table(rData$Track))
colnames(for.pie) <- c("Track", "Frequency")
# need more colors, but for now - okay
my.cols <- brewer.pal(num.colors, "Set3")

# maybe do for death of total percent of incidents?
pie(for.pie$Frequency, labels = for.pie$Track, 
    main = "Single Dimensional: Frequency of Incidents by Track",
    angle = 45, cex = .75,
    col = my.cols)

# do Not steward list/vet divided by total incidents at track?
# by percent of incidents at each track
# how to normalize this data as a percentage of the total
# create a table of incident numbers 
# Maybe death/total incidents by track in pie?
perc.incidents <- 100 * (round(table(rData$Track) / sum(table(rData$Track)), 4))
pie (perc.incidents)
# for Stats in Title
# what is percent of total incidents that are equine death
for.perc <- table(rData$Incident.Type)
for.perc <- as.data.frame(for.perc)
colnames(for.perc) <- c("Incident.Type", "Frequency")
total.incidents <- sum(for.perc$Frequency)
perc.death <- 100*(for.perc[which(for.perc$Incident.Type == "EQUINE DEATH"),2]/total.incidents)

# find percent that happen on track
str(rData)
unique(rData$Racing.Type.Description)
for.perc <- table(rData$Racing.Type.Description)
for.perc <- as.data.frame(for.perc)
colnames(for.perc) <- c("Racing.Type.Description", "Frequency")
total.incidents <- sum(for.perc$Frequency)
perc.racing <- 100*(for.perc[which(for.perc$Racing.Type.Description == "Racing"),2]/total.incidents)

# --------------  Incidents per year - time series --------------------
# 
# ?? Are Racing Incidents decreasing ??
for.plot  <-as.data.frame(table(rData$Year))
colnames (for.plot) <- c("Year", "Frequency")

plot(for.plot$Frequency, 
     type = "s", # for step graph
     #type = "b", # for connect the dots
     main = "Single Dimensional: Racetrack Incidents by Year", 
     sub = "Decline of Equine Racetrack Incidents noted by Year",
     xaxt = "n", pch = 19, lwd = 2, 
     col = "darkgreen", bg = "yellow",
     xlab = "Year", ylab = "Incident Frequency")
# add year to x axis
axis(1, at=1:11, labels = for.plot$Year)

# line graph of races time series by division
# ?? Are the declining on both types of tracks ??
# how to get a sum of all incidents by division by year
tsp.df <- as.data.frame(table(rData$Year, rData$Division, rData$Incident.Type))
# can I do something by type (color) freq (y), year (x)
colnames(tsp.df) <- c("Year", "Division", "Incident.Type", "Frequency")
test.df <- aggregate(tsp.df$Frequency, 
                     list(Year=tsp.df$Year, Division=tsp.df$Division), sum)
#ggplot(df2) + aes(x = year, y = x, color = region) +
#  geom_line() + ylim(c(0,10000))

# close - but needs to provide by year!
tot.by.division <- tapply(tsp.df$Frequency, tsp.df$Division, FUN = sum)
# This is the one!
another.df <- as.data.frame(table(tsp.df$Year, tsp.df$Division ))

plot(another.df$Frequency, type = "b", 
     main = "Multi- Dimensional: Racetrack Incidents by Division by Year", 
     sub = "Decline of Equine Racetrack Incidents by Division noted by Year",
     xaxt = "n", pch = 19, lwd = 2,
     col = "darkgreen", bg = "yellow",
     xlab = "Year", ylab = "Incident Frequency")
# add year to x axis
axis(1, at=1:11, labels = for.plot$Year)

# --------------  (Incidents by Type by Division --------------------
# not used - changed to plot by 
num.colors <- length(unique(rData$Incident.Type))
xtz <- colorRampPalette(c("navy", "darkblue", "darkred", "orange", "tan", "darkgreen"))
my.cols <- xtz(num.colors)
barplot(table(rData$Incident.Type, rData$Division), beside=TRUE, legend=TRUE,
        main="Mulit-Dimensional: Incident Type by Racetrack Division", 
        sub="Frequency of Each Incident Type by Racetrack Division - Harness or Thoroughbred",
        col = my.cols,
        axisnames=TRUE, las = 1, cex.names = 1,
        args.legend = list(x = "topleft", bty = "n", cex=0.85))

# Need to make this type on X and Division on Y, beside
par(mar=c(12, 5, 4, 2))
barplot(table(rData$Division, rData$Incident.Type), beside=TRUE, legend=TRUE,
        main="Frequency of Each Incident Type by Racetrack Division - Harness or Thoroughbred", 
        col = c("tan", "darkred"),
        axisnames=TRUE, 
        las = 2, 
        cex.names = .65,
        args.legend = list(x = "topleft", bty = "n", cex=0.85))

par(mar(12,12,4,4))
# maybe remove stewards/vet list
g <- ggplot(rData, aes(x=Track, fill=factor(Incident.Type)))
g <- g + labs(title = "Multi-Dimensional: Incident Type by Track",
              subtitle="Number of each Incident Type that occured at each Track in New York State")
#g <- g + geom_bar(position="stack")
g <- g + geom_bar(position = "fill")
#g <- g + geom_bar(position="dodge")
g <- g + scale_fill_manual(name="Incident Types", values= my.cols)
g <- g + xlab("Track") + ylab("Number of Incidents")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))
g

# --------------  Incidents by Type by Track -------------------------
# 
num.colors <- length(unique(rData$Incident.Type))
xtz <- colorRampPalette(c("navy", "blue", "darkgreen", "green", "yellow", "brown"))
my.cols <- xtz(num.colors)

# find a better plot for this - not a bar plot
g <- ggplot(rData, aes(x=Track, fill=factor(Incident.Type)))
g <- g + labs(title = "Multi-Dimensional: Incident Type by Track",
              subtitle="Number of each Incident Type that occured at each Track in New York State")
g <- g + geom_bar(position="stack")
#g <- g + geom_bar(position="dodge")
g <- g + scale_fill_manual(name="Incident Types", values= my.cols)
g <- g + xlab("Track") + ylab("Number of Incidents")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.title = element_text(hjust=0.5))
g

# need to take EACH indident and divide by TOTAL incidents at track and plot that!
# Need to get this to be 100% of the bar for each track (check homework) - stacked
# How to do that!!!

# this is what I want to do for my plot!
df <- aggregate(sales$units.sold, list(year=sales$year), sum)
df2 <- aggregate(sales$units.sold,
                 list(year=sales$year, region=sales$rep.region), sum)
ggplot(df2) + aes(x = year, y = x, color = region) +
  geom_line() + ylim(c(0,10000))


# --------------  TO DO Single-Dimensional 2 (% of incident type compared to all incidents) -------


#--------- Word Cloud ---------------
# need to grab only those rows that fit the accident profile
# Need to maybe only use rows where incident is NOT "equine death - infectious disease" or "stewards/vets list"
index <- which(rData$Incident.Type != "STEWARDS/VETS LIST" |
                 rData$Incident.Type != "EQUINE DEATH - INFECTIOUS DISEASE")
descripts.df <- as.data.frame(rData$Incident.Description[index])
colnames(descripts.df) <- c("description")
# to lower case
descripts.df <- as.data.frame(gsub("\\.", " ", as.character(descripts.df$description)))
colnames(descripts.df) <- c("description")
descripts.df <- as.data.frame(gsub("-", " ", as.character(descripts.df$description)))
colnames(descripts.df) <- c("description")
descripts.df <- as.data.frame(gsub(":", " ", as.character(descripts.df$description)))
colnames(descripts.df) <- c("description")
descripts.df <- tolower(strsplit(as.character(descripts.df$description), " ", 2))
# remove punctuation
descripts.df <- sapply(descripts.df, removePunctuation)
docs <- Corpus(VectorSource(descripts.df))
docs <- tm_map(docs, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#--------- TO FINISH - Weather Conditions ---------------
# weather
# need to replace weather with certain word for mapping
# so, "contains" word -> then map out weather
# first do degree mapping
# weather.temp = cold, cool, warm, hot
# weather.percip = sunny, clear, cloudy, rain, windy

rData$Weather.Conditions <- tolower(rData$Weather.Conditions)
# need to create a new columns weather.temp, weather
# need to build the new weather columns
str_detect(rData$Weather.Conditions[1], "clear", negate=FALSE)
# ?? Does weather play a factor in equine deaths ??
# get historical weather using rnoaa 
install.packages("devtools")
library(devtools)
sessionInfo()
devtools::install_github("ropensci/rnoaa")
install.packages("rnoaa")
library(rnoaa)

# for NOAA
# Email: 	jlwoznic@syr.edu
# Token: 	MmIlMbbJvDnYOMxgeImRlJTbLperrfgK
options(noaakey = "MmIlMbbJvDnYOMxgeImRlJTbLperrfgK")

stations <- ghcnd_stations()
# remove all by NY
new.stations <- stations[which(stations$state == "NY"), ]
# now need only those stations that have start date <= 2009 and end date >= 2019
#index <- which((tweets$date > start.date) & (tweets$date < end.date))
index <- which((new.stations$first_year < 2010) & (new.stations$last_year > 2018))
new.stations <- new.stations[index, ]

# build a lat/lon matrix
weather.geocode.df <- cbind.data.frame(geocode.df$lat, geocode.df$lon)
weather.geocode.df$id <- rownames(weather.geocode.df) 
colnames(weather.geocode.df) <- c("lat", "lon", "id")
abc <- meteo_nearby_stations(weather.geocode.df, lat_colname = "lat", lon_colname = "lon",
                             station_data = new.stations, var = "all", year_min = 2009, year_max = 2019,
                             radius = 35, limit=NULL)
# gave up on the loop - LOL
# grab first station which is closest to the lat/lon of the track
# need to use the 3rd index for Monticello because there was no data for the first two stations
wstation.track <- c(abc$`1`$id[1], abc$`2`$id[1], abc$`3`$id[1], abc$`4`$id[1],
                    abc$`5`$id[1], abc$`6`$id[3], abc$`7`$id[1], abc$`8`$id[1],
                    abc$`9`$id[1], abc$`10`$id[1], abc$`11`$id[1])

# now we have a list of the closest weather station for each track, we can use the associated CSV
# file that I downloaded from Climate.gov for each station by reading the mapping stations2csv.csv
stations2csv.df <- read.csv("/Users/joycewoznica/IST719/Project/noaa/stations2csv.csv", header = TRUE, stringsAsFactors = FALSE)
# loop through here
weather.dir <- "/Users/joycewoznica/IST719/Project/noaa/"
i <- 1
for (i in 11)
{
  csv.file <- paste0(weather.dir, stations2csv.df$CSV[i], ".csv")
  ttt <- read.csv(csv.file, header = TRUE, stringsAsFactors = FALSE)
  # somehow need to map this to the name of the track or trackid
}

weather.interest <- c("PSUN", "PRCP", "TMAX", "TMIN", "ACSC", "AWND", 
                      "WT01", "WT02", "WT03", "WT05", "WT07", "WT08", "WT11", 
                      "WT12", "WT13", "WT14", "WT15", "WT16", "WT17", "WT21", "WT22")
# PSUN = Daily percent of possible sunshine (percent)
# PRCP = Precipitation (tenths of mm)
# TMAX = Maximum temperature (tenths of degrees C)
# TMIN = Minimum temperature (tenths of degrees C)
# ACSC = Average cloudiness sunrise to sunset from 30-second 
#       ceilometer data (percent)
# AWND = Average daily wind speed (tenths of meters per second)
# 
# WT** = Weather Type where ** has one of the following values:
#  
# 01 = Fog, ice fog, or freezing fog (may include heavy fog)
# 02 = Heavy fog or heaving freezing fog (not always 
#                                        distinquished from fog)
# 03 = Thunder
# 05 = Hail (may include small hail)
# 07 = Dust, volcanic ash, blowing dust, blowing sand, or 
#      blowing obstruction
# 08 = Smoke or haze 
# 11 = High or damaging winds
# 12 = Blowing spray
# 13 = Mist
# 14 = Drizzle
# 15 = Freezing drizzle 
#16 = Rain (may include freezing rain, drizzle, and
#           freezing drizzle) 
# 17 = Freezing rain 
# 21 = Ground fog 
# 22 = Ice fog or freezing fog

# for all WT - there is a 1 or blank - if 1, then this condition happened
# date is 'YYYY-MM-DD' (character)
test.date <- ttt[which(ttt$DATE == "2009-01-22"),]

# this means that for getting weather by date, you will need to open the corresponding stations2csv.df$CSV 
# which maps to each track and then find the important information from that data.
# Thinking of storing weather data separately from the incident data, but not sure

# ggplot defines its own set of colors
ggplot(sales) + 
  aes(x=expenses, y=income, color=unit.price) + 
  geom_point()


ggplot(sales) + 
  aes(x=expenses, y=income, 
      color=rep.region,
      shape = type,
      alpha = unit.price,
      size = units.sold) + 
  geom_point()

#---------- Map of Tracks ---------------------
# Does location make a difference?
# ?? Where in New York are the highest incidents of Equine Death on the track ??
index <- which(rData$Incident.Type == "EQUINE DEATH")
map.df <- as.data.frame(rData$Track[index])
colnames(map.df) <- c("Track")
# now just get the counts 
counts.by.track <- as.data.frame(table(map.df))
colnames(counts.by.track) <- c("Track", "Deaths")
# call function to add ciy/state to each track
# need to match up address with each track
# function to get address
i <- 1
city.state.df <- NULL
while (i <= length(counts.by.track$Track))
{
  track <- counts.by.track$Track[i]
  city.state.df <- rbind(city.state.df, trackAddress(track))
  i <- i+1
}
colnames(city.state.df) <- c("StreetAdd", "City", "State", "Zip")
city.state.df <- as.data.frame(city.state.df)
city.state.df$Addr <- sprintf("%s, %s %s %s",
                              city.state.df$StreetAdd, 
                              city.state.df$City, 
                              city.state.df$State, 
                              city.state.df$Zip)
city.state.df <- as.data.frame(city.state.df)
geocode.df <- NewLatLon(city.state.df$Addr)
# then combine using
total.map.df <- cbind(counts.by.track, city.state.df, geocode.df)
# remove unnecessary columns
total.map.df[,c("address", "elapsed_time")] <- list(NULL)

# map it
ny.map <- map_data("state", region = "new york")
oplot <- ggplot(total.map.df,aes(lon,lat))
oplot <- oplot + geom_polygon(data = ny.map, aes(x = long, y = lat, group = group),
                              color="darkgreen", 
                              fill = "white")
oplot <- oplot + geom_point(aes(color = Deaths),size=5) + 
  scale_color_viridis(option = "inferno", direction = -1)
oplot <- oplot + geom_text(data=total.map.df, aes(label = Track, x = lon, y = lat),
                           position = position_nudge(x = 0, y = .12),
                           size = 3, color = "saddlebrown")
oplot <- oplot +  xlim(-80,-72.3)+ylim(40.5,45.05)
oplot <- oplot + ylab("Latitude") + xlab("Longitude")
oplot <- oplot + ggtitle("Incidents by Track Location in New York State")
oplot

#------------------ TO DO - Deaths by Trainer -----------------------

