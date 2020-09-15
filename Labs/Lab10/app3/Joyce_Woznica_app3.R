#
# lab 10: App 3
#
library(shiny)

fname <- "/Users/joycewoznica/IST719/DataSets/forLabs/art.csv"

# server
artServer <- function(input, output)
{
 art <- read.csv(fname, header=TRUE, stringsAsFactors = FALSE)
 watercolor.col <- "cadetblue1"
 drawing.col <- "antiquewhite"
 
 #plotOutput("yearlyReceipts")
 output$yearlyReceipts <- renderPlot(
   {
     print("Inside yearlyReceipts")
     my.title <- "Number of Sales per Year"
     barplot(table(art$year), main=my.title, border = "white",
             col = "chartreuse4")
   } )
 
 # plot for StorePaper 
 output$storePaper <- renderPlot(
   {
     print("Inside storePaper")
     if (input$store != "None")
     {
       print(paste("storePaper:: store", input$store))
       sub.index <- which(art$store == input$store)
       tmp.data <- art[sub.index, ]
       pie(table(tmp.data$paper), col=c(watercolor.col, drawing.col),
           border = NA)
     }
   }
 )
}

# UI
artUI <- fluidPage(
  titlePanel("ACME Art Company Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      plotOutput("yearlyReceipts"),
      selectInput("store","Select Store:", choices = c("None", "Portland", "Davenport","Syracuse", "Dublin")),
      ),
      
      mainPanel(
        plotOutput("storePaper")  
      )
  )
)

shinyApp(ui = artUI, server = artServer)
