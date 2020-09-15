#
# lab 10: App 6


#--------------- Code for Map with shiny ----------------------------

#
library(shiny)
#install.packages("leaflet")
library(leaflet)
library(ggmap)

my.dir <- "/Users/joycewoznica/IST719/DataSets/forLabs/"

libs <- read.csv(paste0(my.dir, "NewYorkLibraries.csv"), stringsAsFactors = FALSE, quote ="\"", header = TRUE)
ny.libs <- nrow(libs)

# server
server <- function(input, output, session)
{
  print("server:: start")
  
  points <- eventReactive(input$num.libs,{
    index <- sample(1:nrow(libs), input$num.libs)
    #index <- sample(1:nrow(libs), 3)
    addys <- paste(libs$ADDRESS[index], libs$CITY[index], libs$STABR[index], sep = ", ")
    g.codes <- NewLatLon(addys)
    df <- data.frame(lon = g.codes$lon, lat = g.codes$lat, addy = addys)
    # return the dataframe
    df
    }, ignoreNULL = FALSE
  )
  
  output$mymap <- renderLeaflet({
    M <- leaflet()
    M <- addProviderTiles(M, providers$OpenStreetMap,
                          options = providerTileOptions(noWrap = TRUE))
    df <- points()
    addMarkers(M, lng = df[, 1], lat = df[, 2], popup = df[, 3])
    
  })
}

ui <- fluidPage(
  leafletOutput("mymap"),
  numericInput("num.libs", "Number of Libraries", 10, min = 1, max = ny.libs)
)

shinyApp(ui, server)

# doesn't quite work, but no time to find error.





