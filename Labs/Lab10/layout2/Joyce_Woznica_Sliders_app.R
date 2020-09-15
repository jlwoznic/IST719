# lab 10: layout2
# make sliders

library(shiny)

server <- function(input, output) 
{
  
  output$plot1 <- renderPlot({
    A <- sample(LETTERS[3:(2 + input$slider2)], input$slider1, replace = TRUE)
    B <- sample(letters[12:(11 + input$slider3)], input$slider1, replace = TRUE)
    barplot(table(A, B), beside = TRUE)
  })

  output$plot2 <- renderPlot({
    C.num.letters <- input$slider5
    D.num.letters <- input$slider6
    print(C.num.letters, D.num.letters)
    
    C <- sample(LETTERS[7:(6 + C.num.letters)], input$slider4, replace = TRUE)
    D <- sample(letters[17:(16 + D.num.letters)], input$slider4, replace = TRUE)
    barplot(table(D, C), beside = TRUE)
  })  

}

ui <- fluidPage(
  titlePanel("Hellow Shiny!"),
  
  wellPanel(
    fluidRow(
      column(6, 
      sliderInput("slider1", "P1 Observations", min = 10, max = 1000, value = 20),
      sliderInput("slider2", "A cats", min = 2, max = 5, value = 2),
      sliderInput("slider3", "B cats", min = 1, max = 5, value = 3)
      ),
      column(6, 
             sliderInput("slider4", "P2 Observations", min = 10, max = 1000, value = 20),
             sliderInput("slider5", "A cats", min = 2, max = 5, value = 2),
             sliderInput("slider6", "B cats", min = 1, max = 5, value = 3)
      )
    ),
    
    fluidRow(
      column(6, plotOutput("plot1")),
      column(6, plotOutput("plot2"))
    )
  )
)

shinyApp(ui, server)