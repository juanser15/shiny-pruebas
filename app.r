library(shinyjs)
library(readxl)
library(rhandsontable)
library(shinyURL)
library(plotly)
library(shinydashboard)
library(dplyr)
library(dbplyr)
library(purrr)
library(shiny)
library(highcharter)
library(htmltools)
library(scales)
my_username <- "Molca"
my_password <- "Molca"

jsCode <- ("shinyjs.pageCol = function(params){
           $('body').css('background', params);
           $('h3.colorLabel').text(params); 
           };")

js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")

Bold_function <- ("function (instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.TextRenderer.apply(this, arguments);
                  td.style.background = 'lightblue';
                  td.style.fontWeight = 'bold';
                  }
                  ")
Bold_function_row <- ("function (instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.TextRenderer.apply(this, arguments);
                      td.style.background = 'grey';
                      td.style.fontWeight = 'bold';
                      }
                      ")

BBB <- ("
        function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        tbl = this.HTMLWidgets.widgets[0]
        hrows = tbl.params.row_highlight
        hrows = hrows instanceof Array ? hrows : [hrows]
        if (hrows.includes(row)) {
        td.style.background = 'lightgrey';
        td.style.fontWeight = 'bold';
        }
        return td;
        }")

Cell_editable <- ("function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.NumericCell.renderer.apply(this, arguments);
                  tbl = this.HTMLWidgets.widgets[0]
                  if (tbl.params.Row_numbers.includes(row)) td.style.background = 'lightblue';
                  }")

# Fuente <- "C:/Users/juana/Desktop/APP MOLCA Last/Data.xlsx"                  
# tab <- data.frame(read_excel(Fuente, sheet = "Business Inputs"))
# tab <- data.frame(tab)
# colnames(tab) <- c("Variables", gsub("X","",colnames(tab)[-1]))

# CPI.Argentina <- (1+tab[which(tab$Variables == "CPI Argentina"),paste0("",2017:2021)])    

ui <- dashboardPage( skin='blue',
                     # dashboardHeader(title = "Molino Cañuelas S.A.C.I.F.I.A",
                     dashboardHeader(title = "EMPRESA",
                                     titleWidth = 200),
                     dashboardSidebar(uiOutput("sidebarpanel")),
                     dashboardBody(uiOutput("body"),
                                   tabsetPanel(id = "tabs",
                                               tabPanel(
                                                 title = "Main Dashboard",
                                                 value = "page1",
                                                 fluidRow(
                                                   valueBoxOutput("total_flights"),
                                                   valueBoxOutput("per_day"),
                                                   valueBoxOutput("Ebitdapercentahe")
                                                 ),
                                                 fluidRow(
                                                   column(width = 3, offset = 0,
                                                          plotlyOutput("Sales", height = "300")),
                                                   column(width = 3,  offset = 0,
                                                          plotlyOutput("Cogs", height = "300")),
                                                   column(width = 3,  offset = 0,
                                                          plotlyOutput("barsplot", height = "300", width = "600"))
                                                   
                                                 ),
                                                 fluidRow(
                                                   column(width = 3, offset = 0,
                                                          plotlyOutput("Ebitda", height = "300")),
                                                   column(width = 3, offset = 0,
                                                          plotlyOutput("Ebitdapct", height = "300")),
                                                   column(width = 3, offset = 0,
                                                          plotlyOutput("Ebitdatime", height = "300", width = "600" ))
                                                 ),
                                                 tags$footer("Notes"),
                                                 tags$footer("RP = Retail Products"),
                                                 tags$footer("BIP = Branded and Industrial Products"),
                                                 tags$footer("ASSS = Agro Services and Sustainable Sourcing")
                                               ),
                                               tabPanel(
                                                 title = "Valuation Dashboard",
                                                 value = "page11",
                                                 fluidRow(
                                                   valueBoxOutput("total_flights1"),
                                                   valueBoxOutput("per_day1"),
                                                   valueBoxOutput("Ebitdapercentahe1")
                                                 ),
                                                 fluidRow(
                                                   valueBoxOutput("Sales1"),
                                                   valueBoxOutput("Cogs1"),
                                                   valueBoxOutput("barsplot1")
                                                 ),
                                                 fluidRow(
                                                   valueBoxOutput("Ebitda1"),
                                                   valueBoxOutput("Ebitdapct1"),
                                                   valueBoxOutput("Ebitdatime1")
                                                 ),
                                                 tags$footer("Notes"),
                                                 tags$footer("RP = Retail Products"),
                                                 tags$footer("BIP = Branded and Industrial Products"),
                                                 tags$footer("ASSS = Agro Services and Sustainable Sourcing")
                                               ),
                                               
                                               tabPanel(
                                                 title = "Tables",
                                                 tabPanel('Financial Statements', tabsetPanel(tabPanel("Balance Sheet", rHandsontableOutput('BS')),
                                                                                              tabPanel("Income Statement", rHandsontableOutput('IS')),
                                                                                              tabPanel("Cash Flows", rHandsontableOutput('CF')))),
                                                 tabPanel('By segment', tabsetPanel(tabPanel("Retail Products", rHandsontableOutput('RP')),
                                                                                    tabPanel("Branded Industrial Products", rHandsontableOutput('BIP')),
                                                                                    tabPanel("Agro Services", rHandsontableOutput('AGRO')))),
                                                 tabPanel('Inputs', tabsetPanel(tabPanel("Production Inputs", rHandsontableOutput('tabEdit2')),
                                                                                tabPanel("Business Inputs", rHandsontableOutput('tabEdit1'))))
                                               )  
                                   )
                     )
)

## server.R :

server <- function(input, output, session) {
  
  Logged <- FALSE
  USER <<- reactiveValues(Logged = Logged)
  observe({ 
    
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <<- TRUE
            } 
          }
          
        } 
      }
    }    
  })

}



shinyApp(ui, server)


