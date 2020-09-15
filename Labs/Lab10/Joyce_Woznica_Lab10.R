#
# Author: Joyce Woznica
# Class: IST 719
# Date: 3/16/2020
# Subject: Lab 10, Week 10
#
# Lab 10
#---------------- Package Load -------------------
library(shiny)

server <- function(input, output)
{
  output$myPie <- renderPlot(
    {
      pie (c(8,12,3), main = "Hello World")
  })
}

ui <- fluidPage(
  mainPanel(plotOutput("myPie"))
)

shinyApp(ui,server)

