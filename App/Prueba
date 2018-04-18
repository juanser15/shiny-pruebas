library(shiny)

ui <- fluidPage(
  

  titlePanel("App de prueba: Julian/ Juan!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "Especifique el num de bins:",
                  min = 100,
                  max = 150,
                  value = 25)    
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot")
      
    )
  )
)


server <- function(input, output) {
  

  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "darkblue", border = "black",
         xlab = "Tiempo en minutos",
         main = "Histograma")
    
  })
  
}
shinyApp(ui = ui, server = server)
