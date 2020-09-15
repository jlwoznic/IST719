#
# lab 10: App 2
#
library(shiny)

#fname <- "/Users/joycewoznica/IST719/DataSets/forLabs/art.csv"
fname <- "/Users/Joyce/Desktop/Syracuse/IST719/Labs/Lab10/art.csv"

artServer <- function(input, output)
{
 art <- read.csv(fname, header=TRUE, stringsAsFactors = FALSE)
 
 plotOutput("yearlyReceipts")
 output$yearlyReceipts <- renderPlot(
   {
     print("Inside yearlyReceipts")
     my.title <- "Number of Sales per Year"
     barplot(table(art$year), main=my.title, border = "white",
             col = "chartreuse4")
   } )
}

artUI <- fluidPage(
  titlePanel("ACME Art Company Dashboard"),
  mainPanel(
    plotOutput("yearlyReceipts")
  )
)

shinyApp(ui = artUI, server = artServer)

