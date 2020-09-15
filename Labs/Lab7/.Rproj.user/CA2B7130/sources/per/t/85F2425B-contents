#
# Author: Joyce Woznica
# Class: IST 719
# Date: 2/23/2020
# Subject: Lab 7, Week 7
#
# Lab 7
#---------------- Package Load -------------------
library (maps)
library (mapproj)
install.packages("raster")
library(raster)
library(plotrix)

#---------------- Natural Earth Replacement ----------------
#library(rnaturalearth) # does not work

install.packages("devtools")
devtools::install_github("ropenscilabs/rnaturalearthdata", force=TRUE)
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")

install.packages("rnaturalearth")
library(rnaturalearth)
library(sp)

#---------------- New Lat Lon -----------------------------
## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 

library(jsonlite)
library(tidyverse)

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
#input addresses
addresses <- c("syracuse University, syracuse")
#
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
d
d[2]
d[3]

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

# test
NewLatLon(addresses)
latlon<-NewLatLon(addresses)
latlon$lon
latlon$lat

#---------------- Start with Maps --------------------------
df <- read.csv ("/Users/joycewoznica/IST719/Labs/Lab7/maplecturedata.csv")

# plot NY state
plot (df$x, df$y)
polygon(df$x, df$y, col = "firebrick1", border = NA)

map(database = "world")

map("world", regions = "India")
map("world", regions = "China")
map("world", regions = c("India", "Pakinstan"),
    fill = TRUE, col = c("orange", "brown"))
map("world", regions = "Finland")
    
m <- map("state")
plot(m$x, m$y)

map("state", fill =TRUE, col=c("orange", "red", "yellow"))
map("county", region = "New York", fill = TRUE, col = terrain.colors(20))

# Natural Earth
india <- ne_states(country="India")
map(india)

attributes(india)
names(india)

india$name
map(india, namefield = "name", region = "Gujarat")
map(india, namefield = "name", region = c("Gujarat", "Rajasthan", "Madhya", "Pradesh"),
    fill = TRUE,
    col = c("orangered", "white", "springgreen4"))

# using raster
india <- raster::getData("GADM", country="IND", level =1)
map(india)
india$NAME_2
map(india, namefield = "NAME_1", region="Gujarat")

india <- raster::getData("GADM", country="IND", level =2)
map(india)
india$NAME_2
map(india, namefield = "NAME_2", region="North 24 Parganas",
    fill = TRUE, col = "springgreen4")

china <- raster::getData("GADM", country="CHN", level =2)
map(china)


#--------------- Choropleth Map Problem -------------------
# thermatic maps, color maps to
fname <- "/Users/joycewoznica/IST719/Labs/Lab7/shootings.rda"
load(fname)
View(shootings)
shootings$Total.Number.of.Victims

sort(shootings$State)
# need to trim the state
tmp.vec <- gsub("^\\s+|\\s+$", "", shootings$State)
sort(tmp.vec)

shootings$State <- tmp.vec

agg.dat <- aggregate(shootings$Total.Number.of.Victims,
                     list(shootings$State),
                     sum)
colnames(agg.dat) <- c("state", "victims")
# can go higher in colors if you want
num.cols <- 10
my.color.vec <- rev(heat.colors(num.cols))
pie(rep(1, num.cols), col = my.color.vec)

# plotrix
agg.dat$index <- round(rescale(x = agg.dat$victims, c(1, num.cols)),0)
# assign the color based on the index
agg.dat$color <- my.color.vec[agg.dat$index]

# data frame has each state, # of victims and colors for each state by # of victims
m <- map("state")
m$names
state.order <- match.map(database="state", regions = agg.dat$state,
                         exact = FALSE, warn = TRUE)
cbind(m$namesk, agg.dat$state[state.order])

map("state", col = agg.dat$color[state.order], fill = TRUE,
    resolution = 0, lty = 1, projection = "polyconic", border = "tan")

#-------------------- Points and geocoding --------------------------
library(ggmap)
libs <- read.csv("/Users/joycewoznica/IST719/Labs/Lab7/newyorklibraries.csv",
                 header=TRUE, quote ="\"",
                 stringsAsFactors = FALSE)

map(database = "world")
mqp("world")
points(0,0, col="red", cex=3, pch=8)
abline(h = 43, col = "blue", lty = 3) #lat
abline(v = 076, col = "blue", lty = 3) #long

us.cities
map("state")
my.cols <- rep(rgb(1, 0., 0.2, 0.7), length(us.cities$name))
my.cols[us.cities$capital > 0] <- rgb(0.2, 0.6, 1, 0.9)

points(us.cities$long, us.cities$lat, col = my.cols, pch = 16,
       cex = rescale(us.cities$pop, c(0.5,7)))

# using NewLatLon instead of geocode
# these do not work - need zip code capability
NewLatLon(c("349 erie blvd east, dewit, ny"))
# just city, state works
NewLatLon(c("dewitt, ny"))

table(libs$CITY)
index <- which(libs$CITY %in% c("SYRACUSE", "DEWITT", "FAYETTEVILLE"))
addy <- paste(libs$ADDRESS[index], libs$CITY[index], libs$STABR[index],
              sep = ", ")

# map no longer works when you run NewLatLon - must be a conflict with packages
map(database = "county", "new york", fill = TRUE, col = "orange")

g.codes <- NewLatLon(addy)
points(g.codes$lon, g.codes$lat, col = "blue",
       cex = 1.1, pch = 16)

#---------------- Rworldmaps ------------------------
install.packages("rworldmap")
library(rworldmap)

countries <- read.delim("/Users/joycewoznica/IST719/Labs/Lab7/countries.csv",
                        quote = "\"",
                        header = TRUE,
                        sep = ";",
                        stringsAsFactors = FALSE)

range(countries$Life.expectancy)
# get rid of 0.0 life expectancy
zap <- which(countries$Life.expectancy == 0.0)
# remove zap from memory, so don't run again
rm(zap)
countries <- countries[-zap,]

num.cat <- 10
# get 3 character code for country
# this uses tapply to loop through something
iso3.codes <- tapply(countries$Country..en., 
                     1:length(countries$Country..en.),
                     rwmGetISO3)

df <- data.frame(country=iso3.codes, labels=countries$Country..en.,
                 life = countries$Life.expectancy)

# now join our data to the map information
df.map <- joinCountryData2Map(df, joinCode = "ISO3",
                              nameJoinColumn = "country")

par(mar = c(0,0,1,0))
mapCountryData(df.map,
               nameColumnToPlot = "life",
               numCats = num.cat,
               catMethod = c("pretty", "fixedwidth", "diverging", "quantiles")[2],
               colourPalette = colorRampPalette(c("orangered", "palegoldenrod", "forestgreen"),
                                         )(num.cat),
               oceanCol = "royalblue4",
               borderCol = "peachpuff4",
               mapTitle = "Life Expectancy")

#------------------- ggmap ----------------------------
library(ggmap)

reported <- read.csv("/Users/joycewoznica/IST719/Labs/Lab7/indiareportedrapes.csv",
                     header = TRUE,
                     quote = "\"",
                     stringsAsFactors = FALSE)

india <- raster::getData("GADM", country="IND", level=1)
cbind(unique(reported$Area_Name), india$NAME_1)
india$NAME_1[india$NAME_1 == "NCT of Delhi"] <- "Delhi"
india$NAME_1 <- gsub(" and ", " & ", india$NAME_1)

install.packages("gpclib")
library(gpclib)
library(rgeos)
library(maptools)
install.packages("broom")
library(broom)

# this does not work - so not providing this plot
#map.df <- fortify(india, region = "NAME_1")
#map.df <- as.data.frame(tidy(index,region="NAME_1"))
# my map ends up with 500,977 rows
head(map.df)
crimes <- aggregate(reported$Cases, list(reported$Area_Name), sum)
colnames(crimes) <- c("id", "ReportedRapes")
crimes[order(crimes$ReportedRapes),]
# this is incorrect!
my.map <- merge(x=map.df, y = crimes, by = "id")

g <- ggplot() 
g <- g + geom_map(data = my.map, map = my.map)
g <- g + aes(x = long, y = lat, map_id = id, group = group, fill = ReportedRapes)
g <- g + theme_minimal() 
g <- g + ggtitle("Reported Rapes in India")
g

#---------------- Shape Files and Geo JSON Files -----------------------
shape.data.dir <- "/Users/joycewoznica/IST719/Labs/Lab7/shapefiles/"
library(stringr)
install.packages("rgdal")
library(rgdal)
install.packages("raster")
library(raster)
install.packages("TeachingDemos")
library(TeachingDemos)

bikes <- readRDS(paste0(shape.data.dir, "bikes.rds"))
nypp <- readOGR(paste0(shape.data.dir, "nyct2010_17a"),
                "nyct2010", stringsAsFactors = FALSE)

syr.neighborhood <- readOGR("/Users/joycewoznica/IST719/Labs/Lab7/syracuse-neighborhoods_ny.geojson")

par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(nypp, border = "bisque4", lwd = 0.5)
# zoom in
zoomplot(c(978000, 999800), ylim = c(185000,225000))

df <- data.frame(lat = bikes$start.station.latitude,
                 lon = bikes$start.station.longitude)
head(df)

point.tab <- sort(table(paste(df$lat, df$lon)), decreasing = TRUE)
point.tab[1:3]

df.2 <- data.frame(lat = as.numeric(word(names(point.tab),1)),
                   lon = as.numeric(word(names(point.tab),2)))
df.2$size <- as.numeric(point.tab)

# now add the points
coordinates(df.2) <- ~lon + lat
crs(df.2) <- CRS("+proj=longlat + datum=WG584")
df.2 <- spTransform(df.2, crs(nypp))
tmp.size <- 0.2 + (2*df.2$size/max(df.2$size))
points(df.2$lon, df.2$lat, col = "red", pch = 19, cex = tmp.size)


