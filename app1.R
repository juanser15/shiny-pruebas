ui <- dashboardPage(
  dashboardHeader(title = "Probando maaaan"),
  dashboardSidebar(sidebarMenu(
    menuItem("Mean of revenues", tabName = "tab", icon = icon("globe")),
    menuItem("Va otro plot", tabName = "tab1", icon = icon("globe")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab",
              fluidRow(
                box(dygraphOutput("graph"), width=15),
                box(textOutput("legendDivID"), title = "Legend", collapsible = TRUE, width=15)
              )
      ),
      tabItem(tabName = "tab1",
              fluidRow(
                box(dygraphOutput("graph1"), width=15),
                box(textOutput("legendDivID1"), title = "Legend", collapsible = TRUE, width=15)
              )
      )
    )
  )
)

server <- function(input, output) {
  library(archivist)
  library(dplyr)
  output$graph <- renderDygraph({
    withProgress(message = "Loading...", {
      dygraph(Mean_nor, main = "Normalized mean of revenues", ylab = "Value") %>% 
        dyRangeSelector() %>%
        dyLegend(labelsDiv = "legendDivID")
    })
  })
  output$graph1 <- renderDygraph({
    withProgress(message = "Loading...", {
      dygraph(Mean_nor, main = "Otro plot", ylab = "Value") %>% 
        dyRangeSelector() %>%
        dyLegend(labelsDiv = "legendDivID1")
    })
  })
}

shinyApp(ui, server)

