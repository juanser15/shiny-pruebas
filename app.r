library(shinycssloaders)
library(rhandsontable)
library(plotly)
library(shinydashboard)
library(shiny)
library(quantmod)
library(dplyr)
library(dbplyr)
library(purrr)
library(shinyjs)
library(readxl)
library(highcharter)
library(htmltools)
library(scales)

DATA_DIR <- file.path("data")
Fuente <-   file.path(DATA_DIR, "Datos.xlsx")
tab <- data.frame(read_excel(Fuente, sheet = "Business Inputs"))
tab <- data.frame(tab)
colnames(tab) <- c("Variables", gsub("X","",colnames(tab)[-1])) 
CPI.Argentina <- (1+tab[which(tab$Variables == "CPI Argentina"),paste0("",2017:2021)])


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

# options(shiny.sanitize.errors = FALSE)

ui <- dashboardPage( skin='blue',
                     dashboardHeader(title = "Molino Cañuelas S.A.C.I.F.I.A",
                                     titleWidth = 200),
                     dashboardSidebar(uiOutput("sidebarpanel")),
                     dashboardBody(uiOutput("body"),
                                   tags$style(type="text/css",
                                              ".shiny-output-error { visibility: hidden; }",
                                              ".shiny-output-error:before { visibility: hidden; }"
                                   ),
                                   
                                   tabsetPanel(id = "tabs",
                                               tabPanel(
                                                 tags$style(type="text/css", "
                                                           #loadmessage {
                                                             position: fixed;
                                                             top: 0px;
                                                             left: 0px;
                                                             width: 100%;
                                                             padding: 5px 0px 5px 0px;
                                                             text-align: center;
                                                             font-weight: bold;
                                                             font-size: 100%;
                                                             color: #000000;
                                                             background-color: #CCFF66;
                                                             z-index: 105;
                                                           }
                                                  "),
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
                                                   #%>% withSpinner(color="#0dc5c1"),
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
                                                 # Sidebar panel for inputs ----
                                                 fluidRow( 
                                                   sidebarPanel(
                                                     sliderInput("RFreeR",
                                                                 "Risk-Free Rate :",
                                                                 value = 0.03,
                                                                 min = 0,
                                                                 step = 0.01, 
                                                                 max = 0.1),
                                                     sliderInput("CRiskP",
                                                                 "Country Risk Premium:",
                                                                 value = 0.03,
                                                                 min = 0,
                                                                 step = 0.01, 
                                                                 max = 0.1),
                                                     sliderInput("Beta",
                                                                 "Company's beta:",
                                                                 value = 1,
                                                                 min = 0,
                                                                 step = 0.1, 
                                                                 max = 3)
                                                   ),
                                                   column(width = 6, offset = 1, style='padding:0px;',
                                                          valueBoxOutput("EVEBITDA", width = 6),
                                                          valueBoxOutput("WACC1", width = 6),
                                                          valueBoxOutput("EV1", width = 6),
                                                          valueBoxOutput("DEBEV", width = 6),
                                                          valueBoxOutput("Beta", width = 6),
                                                          valueBoxOutput("PRICEPER", width = 6)
                                                   ) 
                                                 ),
                                                 fluidRow(
                                                   plotlyOutput("WACC")
                                                   
                                                 )
                                               ),
                                               
                                               # tabPanel(
                                               #   title = "Tables",
                                               #   tabPanel('Financial Statements', tabsetPanel(tabPanel("Balance Sheet", rHandsontableOutput('BS')),
                                               #                                                tabPanel("Income Statement", rHandsontableOutput('IS')),
                                               #                                                tabPanel("Cash Flows", rHandsontableOutput('CF')))),
                                               #   tabPanel('By segment', tabsetPanel(tabPanel("Retail Products", rHandsontableOutput('RP')),
                                               #                                      tabPanel("Branded Industrial Products", rHandsontableOutput('BIP')),
                                               #                                      tabPanel("Agro Services", rHandsontableOutput('AGRO')))),
                                               #   tabPanel('Inputs', tabsetPanel(tabPanel("Production Inputs", rHandsontableOutput('tabEdit2')),
                                               #                                  tabPanel("Business Inputs", rHandsontableOutput('tabEdit1'))))
                                               # ),
                                               tabPanel(
                                                 title = "Inputs panel",
                                                 value = "page11",
                                                 # Sidebar panel for inputs ----
                                                 fluidRow( 
                                                   sidebarPanel(
                                                       sliderInput("Activity2019",
                                                                   "Economic Growth 2019:",
                                                                   value = tab[which(tab == "CPI Argentina"), "2019"],
                                                                   min = 0.032,
                                                                   step = 0.001, 
                                                                   max = 0.10),
                                                       sliderInput("Activity2020",
                                                                   "Economic Growth 2020:",
                                                                   value = tab[which(tab == "CPI Argentina"), "2020"],
                                                                   min = 0.031,
                                                                   step = 0.001, 
                                                                   max = 0.10),
                                                       sliderInput("Activity2021",
                                                                   "Economic Growth 2021:",
                                                                   value = tab[which(tab == "CPI Argentina"), "2021"],
                                                                   min = 0.032,
                                                                   step = 0.001, 
                                                                   max = 0.10)
                                                     ),
                                                   fluidRow( 
                                                     sidebarPanel(
                                                       sliderInput("CPI.USA2019",
                                                                   "CPI USA 2019:",
                                                                   value = 0.03,
                                                                   min = 0,
                                                                   step = 0.001, 
                                                                   max = 0.05),
                                                       sliderInput("CPI.USA2020",
                                                                   "CPI USA 2020:",
                                                                   value = 0.03,
                                                                   min = 0,
                                                                   step = 0.001, 
                                                                   max = 0.05),
                                                       sliderInput("CPI.USA2021",
                                                                   "CPI USA 2021:",
                                                                   value = 0.03,
                                                                   min = 0,
                                                                   step = 0.001, 
                                                                   max = 0.05)
                                                     ),
                                                 fluidRow( 
                                                   sidebarPanel(
                                                 sliderInput("inflation2019",
                                                             "Inflation 2019:",
                                                             value = 0.15,
                                                             min = 0,
                                                             step = 0.01, 
                                                             max = 0.5),
                                                 sliderInput("inflation2020",
                                                             "Inflation 2020:",
                                                             value = 0.10,
                                                             min = 0,
                                                             step = 0.01, 
                                                             max = 0.5),
                                                 sliderInput("inflation2021",
                                                             "Inflation 2021:",
                                                             value = 0.09,
                                                             min = 0,
                                                             step = 0.01, 
                                                             max = 0.5)
                                                 )
                                             )
                                        )
                                    ),
                                    fluidRow(
                                      plotlyOutput("Inputplot")
                                      
                                    )
                               )
                     )
                )
)


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
  #########################################################LOGIN
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {

      dashboardSidebar(
        # actionButton("Logout", "Logout", icon("sign-out-alt"), style='padding:6px; font-size:80%',
        # class = "btn btn-primary"),
        textInput("caption", "Inserte su nombre"),
        verbatimTextOutput("valuename"),
        selectInput(
          inputId = "Segment",
          label = "By segment",
          choices =
            list(
              "Summarize" = "All",
              "By segment" = "By Segment"
            ),
          selected =  "By Segment",
          selectize = FALSE),
        sidebarMenu(
          selectInput(inputId = "month",label = "Select year:",
                      choices =
                        list(
                          "Año 2018" = "2018",
                          "Año 2019" = "2019",
                          "Año 2020" = "2020",
                          "Año 2021" = "2021"
                        ),
                      selected =  "2018",
                      selectize = FALSE)
        ),
        # numericInput("dollar", "Tipo de cambio:", 21, min = 1, max = 100),
        # verbatimTextOutput("value"),
        # numericInput("inflation", "CPI Argentina:", tab[which(tab == "CPI Argentina"), "2017"], min = 1, max = 100),
        # verbatimTextOutput("value1"),
        # actionButton("saveBtn", "Save changes", icon("save"), style='padding:6px; font-size:80%',
        # class = "btn btn-primary"),
        actionButton("submit1" ,"Submit changes", icon("refresh"), style='padding:6px; font-size:80%',
                     class = "btn btn-primary"),
        checkboxInput("Default", "Default", value = TRUE, width = NULL)
      )
    }
  })
  ##################################################################
  output$body <- renderUI({
    if (USER$Logged == TRUE) {


      Value<-reactiveValues(mat=NULL)
      # observe({
      # if (!is.null(input$tabEdit1))
      # Value$mat <- hot_to_r(input$tabEdit1)
      # })

      #######################################
      # observeEvent(input$submit1, {
      # observe({
      tab <- data.frame(read_excel(Fuente, sheet = "Valuacion"))
      tab <- data.frame(tab)
      colnames(tab) <- c("Variables", gsub("X","",colnames(tab)[-1]))
      Value$mat <- tab

      DF1<-reactiveValues(mat=NULL)
      # observe({
      if (!is.null(input$tabEdit1))
        DF1$mat <- hot_to_r(input$tabEdit1)
      # })

      #######################################
      # observeEvent(input$submit1, {
      # observe({
      tab <- data.frame(read_excel(Fuente, sheet = "Business Inputs"))
      tab <- data.frame(tab)
      colnames(tab) <- c("Variables", gsub("X","",colnames(tab)[-1]))
      DF1$mat<-tab
      # })
      MEAN <- NULL
      BADLAR <- NULL
      output$tabEdit1 <- renderRHandsontable({
        #Determino como formula los business inputs que son dependientes (Tipo de cambio promedio y Badlar)
        for(i in 1:length(2017:2020)){MEAN[i] <- mean(as.numeric(DF1$mat[2,paste0("",2017:2021)])[i:(i+1)])}
        for(i in 1:length(2017:2020)){BADLAR[i] <- (as.numeric(DF1$mat[5,paste0("",2017:2021)])[(i+1)]/
                                                      as.numeric(DF1$mat[5,paste0("",2017:2021)])[i]*
                                                      as.numeric(DF1$mat[8,paste0("",2017:2021)])[i])}


        DF1$mat[which(DF1$mat == "ARS/USD (EOP)"), input$month] <- input$dollar
        DF1$mat[which(DF1$mat == "CPI Argentina"), input$month] <- input$inflation

        # DF1$mat[which(DF1$mat == "ARS/USD (EOP)"), input$month] <- input$inflation
        DF1$mat[3,paste0("",2018:2021)] <- MEAN
        DF1$mat[8,paste0("",2018:2021)] <- BADLAR
        DF1 <- DF1$mat
        # row_highlight <- c(which(DF1 == "Exchange Rates"),which(DF1 == "Inflation"),
        #                    which(DF1 == "Interest Rates"),
        #                    which(DF1 == "Industry Growth")) -1
        rhandsontable(DF1, width = 10000, readOnly = TRUE) %>%
          hot_col(col = "2017",readOnly = FALSE) %>%
          hot_col(col = "2018",readOnly = FALSE) %>%
          hot_col(col = "2019",readOnly = FALSE) %>%
          hot_col(col = "2020",readOnly = FALSE) %>%
          hot_col(col = "2021",readOnly = FALSE) %>%
          hot_row(row = 8, readOnly = TRUE) %>%
          hot_row(row = 3, readOnly = TRUE)
        # hot_cols( renderer = BBB)
      })
      # })

      DF2<-reactiveValues(mat=NULL)
      observe({
        if (!is.null(input$tabEdit2))
          DF2$mat <- hot_to_r(input$tabEdit2)
      })
      observe({
        tab <- data.frame(read_excel(Fuente, sheet = "Production Inputs"))
        tab <- data.frame(Productos = tab$Producto,
                          Ratio = tab$Ratio,
                          Porcentaje = tab$Pct)
        DF2$mat<-tab
      })
      output$tabEdit2 <- renderRHandsontable({
        DF2$mat[,3]<- 1/DF2$mat[,2]
        DF2 <- DF2$mat
        rhandsontable(DF2,  selectCallback = TRUE, width = 500) %>%
          hot_col(2:ncol(DF2), format = "0,0[.]") %>%
          hot_col(1,renderer = Bold_function)
      })


      DF.Retail <-reactiveValues(mat=NULL)
      observe({
        if (!is.null(input$RP))
          DF.Retail$mat <- hot_to_r(input$RP)
      })
      observe({
        tab <- data.frame(read_excel(Fuente, sheet = "Retail Products"))
        tab <- data.frame(tab)
        colnames(tab) <- c("Productos", gsub("X","",colnames(tab)[-1]))
        DF.Retail$mat <- tab
      })
      output$RP <- renderRHandsontable({

        # Input Variables  --------------------------------------------------------------
        CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2018:2021)])
        CPI.Argentina[input$month] <- input$inflation
        Industry.Growth <- DF1$mat[which(DF1$mat$Variables == "Industry Growth")+1,paste0("",2018:2021)]
        # Product names  --------------------------------------------------------------
        Product.Retail <- c("Flour Products", "Oil", "Biscuits, Cookies and Crackers", "Bread Crumbs and Premixes (& Pasta)", "Frozen Products")
        # Growth Volume Rates  --------------------------------------------------------------
        Flour.Growth <- cumprod(1+as.numeric(DF.Retail$mat[max(which(DF.Retail$mat$Productos == "Flour Products")),paste0("",2018:2021)]))
        Oil.Growth <- cumprod(1+as.numeric(DF.Retail$mat[max(which(DF.Retail$mat$Productos == "Oil")),paste0("",2018:2021)]))
        BCC.Growth <- cumprod(1+as.numeric(DF.Retail$mat[max(which(DF.Retail$mat$Productos == "Biscuits, Cookies and Crackers")),paste0("",2018:2021)]))
        Prem.Growth <- cumprod(1+as.numeric(DF.Retail$mat[max(which(DF.Retail$mat$Productos == "Bread Crumbs and Premixes (& Pasta)")),paste0("",2018:2021)]))
        Frozen.Growth <- cumprod(1+as.numeric(DF.Retail$mat[max(which(DF.Retail$mat$Productos == "Frozen Products")),paste0("",2018:2021)]))
        # Volume  --------------------------------------------------------------
        DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Flour Products")),paste0("",2018:2021)] <-
          DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Flour Products")),"2017"]*Flour.Growth

        DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Oil")),paste0("",2018:2021)] <-
          DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Oil")),"2017"]*Oil.Growth

        DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Biscuits, Cookies and Crackers")),paste0("",2018:2021)] <-
          DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Biscuits, Cookies and Crackers")),"2017"]*BCC.Growth

        DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Bread Crumbs and Premixes (& Pasta)")),paste0("",2018:2021)] <-
          DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Bread Crumbs and Premixes (& Pasta)")),"2017"]*Prem.Growth

        DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Frozen Products")),paste0("",2018:2021)] <-
          DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Frozen Products")),"2017"]*Frozen.Growth
        #Total Volume  --------------------------------------------------------------
        DF.Retail$mat[max(which(DF.Retail$mat$Productos == "Total Volume")),paste0("",2018:2021)] <-
          colSums(DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Flour Products")):
                                  min(which(DF.Retail$mat$Productos == "Frozen Products")),paste0("",2018:2021)])
        #Average Prices  --------------------------------------------------------------
        DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2019:2021)] <-
          as.numeric(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),"2018"])%*%
          t(cumprod(as.numeric(CPI.Argentina[,paste0("",2019:2021)])))
        #Sales  --------------------------------------------------------------
        DF.Retail$mat[match(paste0("Sales - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2018:2021)]*
          DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),paste0("",2018:2021)]/1000000


        #Total Sales  --------------------------------------------------------------
        DF.Retail$mat[match("Total Sales", DF.Retail$mat$Productos),paste0("",2018:2021)] <-
          colSums(DF.Retail$mat[match(paste0("Sales - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2018:2021)])
        #COGS  --------------------------------------------------------------
        DF.Retail$mat[match("COGS",DF.Retail$mat$Productos),paste0("",2018:2021)] <-
          -colSums(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2018:2021)]*(1-
                    DF.Retail$mat[match(paste0("Average Margins - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2018:2021)])*
                     DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),paste0("",2018:2021)]/1000000)
        DF.Retail$mat[match("COGS",DF.Retail$mat$Productos),paste0("",2018)] <- sum(DF.Retail$mat[match("COGS",DF.Retail$mat$Productos),c("9M2018","4Q2018")])
        #Gross Margin  --------------------------------------------------------------
        DF.Retail$mat[match("Gross Margin",DF.Retail$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Total Sales",DF.Retail$mat$Productos),paste0("",2018:2021)] +
          DF.Retail$mat[match("COGS",DF.Retail$mat$Productos),paste0("",2018:2021)] +
          DF.Retail$mat[match("Depreciation and Amortization",DF.Retail$mat$Productos),paste0("",2018:2021)]
        #Gross Margin  %--------------------------------------------------------------
        DF.Retail$mat[match("Margin Before Operating Expenses",DF.Retail$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Gross Margin",DF.Retail$mat$Productos),paste0("",2018:2021)]/
          DF.Retail$mat[match("Total Sales", DF.Retail$mat$Productos),paste0("",2018:2021)]
        #Selling and Administrative Costs --------------------------------------------------------------
        Vol.Change.Retail <- DF.Retail$mat[max(which(DF.Retail$mat$Productos == "Total Volume")),paste0("",2017:2021)][-1]/
          DF.Retail$mat[max(which(DF.Retail$mat$Productos == "Total Volume")),paste0("",2017:2021)][-length(2017:2021)]-1 #Pass-Through Inflation -> Volume Growth

        DF.Retail$mat[match("Selling Expense", DF.Retail$mat$Productos),paste0("",2019:2021)] <-
                                                                                              DF.Retail$mat[match("Selling Expense",DF.Retail$mat$Productos),"2018"]*
                                                                                              cumprod(as.numeric(CPI.Argentina[,paste0("",2019:2021)]))*
                                                                                              cumprod(as.numeric(Vol.Change.Retail[,paste0("",2019:2021)])+1)
        
        DF.Retail$mat[match("Administrative Expense", DF.Retail$mat$Productos),paste0("",2019:2021)] <-
          DF.Retail$mat[match("Administrative Expense", DF.Retail$mat$Productos),"2018"]*cumprod(as.numeric(CPI.Argentina[,paste0("",2019:2021)]))
        #EBITDA and mor e --------------------------------------------------------------
        DF.Retail$mat[match("Results from Operations Before Financing and Tax", DF.Retail$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Gross Margin",DF.Retail$mat$Productos),paste0("",2019:2021)] +
          DF.Retail$mat[match("Selling Expense",DF.Retail$mat$Productos),"2018"]*
          cumprod(as.numeric(CPI.Argentina[,paste0("",2019:2021)]))*
          cumprod(as.numeric(Vol.Change.Retail[,paste0("",2019:2021)])+1) +
          DF.Retail$mat[match("Administrative Expense", DF.Retail$mat$Productos),"2018"]*cumprod(as.numeric(CPI.Argentina[,paste0("",2019:2021)])) +
          DF.Retail$mat[match("Other Income, Net", DF.Retail$mat$Productos),"2018"]
        

        DF.Retail$mat[match("Reported Adjusted Segment EBITDA", DF.Retail$mat$Productos),paste0("",2019:2021)] <-
          DF.Retail$mat[match("Results from Operations Before Financing and Tax", DF.Retail$mat$Productos),paste0("",2019:2021)] -
          DF.Retail$mat[match("Depreciation and Amortization",DF.Retail$mat$Productos),paste0("",2019:2021)]





        DF.Retail$mat[match("EBITDA Margin (%)", DF.Retail$mat$Productos),paste0("",2019:2021)] <-
          DF.Retail$mat[match("Reported Adjusted Segment EBITDA", DF.Retail$mat$Productos),paste0("",2019:2021)]/
          DF.Retail$mat[match("Total Sales", DF.Retail$mat$Productos),paste0("",2019:2021)]

        # Inserr final table   ------------------------------------------------------------
        DF.Retail <- DF.Retail$mat
        rhandsontable(DF.Retail,  selectCallback = TRUE, width = 100000) %>%
          hot_col(2:ncol(DF.Retail), format = "0,0[.]") %>%
          hot_col(1,renderer = Bold_function)
      })

      DF.BIP<-reactiveValues(mat=NULL)
      observe({
        if (!is.null(input$BIP))
          DF.BIP$mat <- hot_to_r(input$BIP)
      })
      observe({
        tab <- data.frame(read_excel(Fuente, sheet = "Branded Industrial Products"))
        tab <- data.frame(tab)
        colnames(tab) <- c("Productos", gsub("X","",colnames(tab)[-1]))
        DF.BIP$mat <- tab
      })
      output$BIP <- renderRHandsontable({
        # Input Variables  --------------------------------------------------------------
        CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2018:2021)])
        # CPI.Argentina[input$month] <- input$inflation
        Industry.Growth <- DF1$mat[which(DF1$mat$Variables == "Industry Growth")+1,paste0("",2018:2021)]

        Row_numbers <- (which(DF.BIP$mat$Productos == "Volume growth")+1):
          (which(DF.BIP$mat$Productos == "Volume growth")+3)

        Products_names <- DF.BIP$mat$Productos[Row_numbers]
        DF.BIP$mat[Row_numbers[2],paste0("",2018:2021)] <- Industry.Growth
        Growth_rates <- DF.BIP$mat[Row_numbers,paste0("",2018:2021)]
        Growth_rates_cum <- t(apply((Growth_rates+1),1,cumprod))

        DF.BIP$mat[min(which(DF.BIP$mat$Productos==Products_names[1])),paste0("",2018:2021)] <-
          DF.BIP$mat[min(which(DF.BIP$mat$Productos==Products_names[1])),"2017"]*Growth_rates_cum[1,]

        DF.BIP$mat[min(which(DF.BIP$mat$Productos==Products_names[2])),paste0("",2018:2021)] <-
          DF.BIP$mat[min(which(DF.BIP$mat$Productos==Products_names[2])),"2017"]*Growth_rates_cum[2,]

        DF.BIP$mat[min(which(DF.BIP$mat$Productos=="Total Wheat Flour")),paste0("",2018:2021)] <-
          colSums(DF.BIP$mat[3:4,paste0("",2018:2021)])

        DF.BIP$mat[min(which(DF.BIP$mat$Productos==Products_names[3])),paste0("",2018:2021)] <-
          DF.BIP$mat[min(which(DF.BIP$mat$Productos==Products_names[3])),"2017"]*Growth_rates_cum[3,]
        # Total BIP Sold --------------------------------------------------------------
        DF.BIP$mat[min(which(DF.BIP$mat$Productos=="Total BIP Sold")),paste0("",2018:2021)] <-
          colSums(DF.BIP$mat[5:6,paste0("",2018:2021)])
        # Wheat + MRP --------------------------------------------------------------
        DF.BIP$mat[9,paste0("",2017:2021)] <-
          DF.BIP$mat[3,paste0("",2017:2021)]*DF.BIP$mat[38,paste0("",2017:2021)]/1000000
        DF.BIP$mat[10,paste0("",2017:2021)] <-
          DF.BIP$mat[4,paste0("",2017:2021)]*DF.BIP$mat[39,paste0("",2017:2021)]/1000000
        # Total Wheat --------------------------------------------------------------
        DF.BIP$mat[11,paste0("",2017:2021)] <- colSums(DF.BIP$mat[9:10,paste0("",2017:2021)])
        # Co-Wheat goods --------------------------------------------------------------
        DF.BIP$mat[12,paste0("",2017:2021)]   <-
          DF.BIP$mat[6,paste0("",2017:2021)]*DF.BIP$mat[40,paste0("",2017:2021)]/1000000
        # Total Sales --------------------------------------------------------------
        DF.BIP$mat[14,paste0("",2017:2021)]   <-
          colSums(DF.BIP$mat[11:13,paste0("",2017:2021)])
        # COGS -------------------------------------------------------------------
        DF.BIP$mat[15,paste0("",2018:2021)] <-
          -colSums(DF.BIP$mat[38:40,paste0("",2018:2021)]*
                     (1-DF.BIP$mat[50:52,paste0("",2018:2021)])*
                     DF.BIP$mat[c(3,4,6),paste0("",2018:2021)]/1000000) -
          DF.BIP$mat[13,paste0("",2018:2021)]*
          (1 - DF.BIP$mat[53,paste0("",2018:2021)])
        # Gross Margin ------------------------------------------------------------
        DF.BIP$mat[17,paste0("",2018:2021)] <- colSums(DF.BIP$mat[14:16,paste0("",2018:2021)])
        # Margin Before Operating Expenses------------------------------------------------------------
        DF.BIP$mat[18,paste0("",2018:2021)] <- DF.BIP$mat[17,paste0("",2018:2021)]/
          DF.BIP$mat[14,paste0("",2018:2021)]
        # Selling and ADM expenses  ------------------------------------------------------------
        Vol.Change <- DF.BIP$mat[min(which(DF.BIP$mat$Productos=="Total BIP Sold")),paste0("",2017:2021)][-1]/
          DF.BIP$mat[min(which(DF.BIP$mat$Productos=="Total BIP Sold")),paste0("",2017:2021)][-length(2017:2021)]
        DF.BIP$mat[19,paste0("",2018:2021)] <- DF.BIP$mat[19,"2017"]*cumprod(as.numeric(Vol.Change))*
          cumprod(as.numeric(CPI.Argentina))

        DF.BIP$mat[20,paste0("",2018:2021)] <- DF.BIP$mat[20,"2017"]*cumprod(as.numeric(CPI.Argentina))
        # Other Net Income  ------------------------------------------------------------
        DF.BIP$mat[21,paste0("",2018:2021)] <- 0
        # EBITDA  ------------------------------------------------------------
        DF.BIP$mat[22,paste0("",2018:2021)] <- DF.BIP$mat[17,paste0("",2018:2021)] +
          colSums(DF.BIP$mat[19:21,paste0("",2018:2021)])
        # Adjusted EBITDA  ------------------------------------------------------------
        DF.BIP$mat[28,paste0("",2018:2021)] <- DF.BIP$mat[22,paste0("",2018:2021)] -
          DF.BIP$mat[min(which(DF.BIP$mat$Productos == "Depreciation and Amortization")),
                     paste0("",2018:2021)]
        # Average Prices  ------------------------------------------------------------
        DF.BIP$mat[38:40,paste0("",2018:2021)] <-
          DF.BIP$mat[38:40,"2017"]%*%
          t(cumprod((1+as.numeric(DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2018:2021)]))))

        # Inserr final table   ------------------------------------------------------------
        DF.BIP <- DF.BIP$mat
        rhandsontable(DF.BIP,  selectCallback = TRUE, width = 100000) %>%
          hot_col(2:ncol(DF.BIP), format = "0,0[.]") %>%
          hot_col(1,renderer = Bold_function)
      })

      DF.AGRO<-reactiveValues(mat=NULL)
      observe({
        if (!is.null(input$AGRO))
          DF.AGRO$mat <- hot_to_r(input$AGRO)
      })
      observe({
        tab <- data.frame(read_excel(Fuente, sheet = "Agro Services "))
        tab <- data.frame(tab)
        colnames(tab) <- c("Productos", gsub("X","",colnames(tab)[-1]))
        DF.AGRO$mat<-tab
      })
      output$AGRO <- renderRHandsontable({
        # Common Variables   ------------------------------------------------------------
        CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2018:2021)])
        CPI.Argentina[input$month] <- input$inflation
        Industry.Growth <- DF1$mat[which(DF1$mat$Variables == "Industry Growth")+1,paste0("",2018:2021)]
        # Sustainable Sourcing   ------------------------------------------------------------
        Sustainable.Sourcing <- rbind(DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Flour Products")),paste0("",2018:2021)] +
                                        DF.BIP$mat[min(which(DF.BIP$mat$Productos=="Total Wheat Flour")),paste0("",2018:2021)],
                                      DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Oil")),paste0("",2018:2021)],
                                      DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Biscuits, Cookies and Crackers")),paste0("",2018:2021)],
                                      DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Bread Crumbs and Premixes (& Pasta)")),paste0("",2018:2021)],
                                      DF.Retail$mat[min(which(DF.Retail$mat$Productos == "Frozen Products")),paste0("",2018:2021)],
                                      DF.BIP$mat[min(which(DF.BIP$mat$Productos=="Soybean Flour and Co-Products")),paste0("",2018:2021)])
        SS1 <- match("Agro Services and Sustainable Sourcing for Intersegment  (1)",DF.AGRO$mat$Productos)
        SS2 <- match("Agro Services and Sustainable Sourcing for Thirt Parties (2)",DF.AGRO$mat$Productos)
        DF.AGRO$mat[SS1, paste0("",2018:2021)] <- colSums(Sustainable.Sourcing*DF2$mat$Porcentaje)

        DF.AGRO$mat[SS2, paste0("",2018:2021)] <- colSums(as.numeric(DF.AGRO$mat[grep("Third parties - ",
                                                                                      DF.AGRO$mat$Productos),"2017"])*
                                                            t(apply(1+apply(DF.AGRO$mat[grep("Average Growth - ",
                                                                                             DF.AGRO$mat$Productos),paste0("",2018:2021)],2,as.numeric),1,cumprod)))

        # Sales   ------------------------------------------------------------
        Dev <- as.numeric(DF1$mat[match("Devaluación ARS/USD (Avg)",DF1$mat$Variables),paste0("",2018:2021)])

        Fact.Adjusted1 <- c(1963, 3795, 1984, 3708)

        DF.AGRO$mat[match("Sustainable Sourcing",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          colSums((Fact.Adjusted1%*%t(cumprod(1+Dev))*as.numeric(DF.AGRO$mat[grep("Third parties - ",
                                                                                  DF.AGRO$mat$Productos),"2017"])*t(apply(1+apply(DF.AGRO$mat[grep("Average Growth - ",
                                                                                                                                                   DF.AGRO$mat$Productos),paste0("",2018:2021)],2,as.numeric),1,cumprod)))/1000000)

        DF.AGRO$mat[match("Agro-services",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Agro-services",DF.AGRO$mat$Productos),"2017"]*
          cumprod(1+Dev+apply(DF.AGRO$mat[grep("Average Growth - ",
                                               DF.AGRO$mat$Productos),paste0("",2018:2021)],2,mean))

        DF.AGRO$mat[match("Port and Logistics",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Port and Logistics",DF.AGRO$mat$Productos),"2017"]*
          cumprod(1+Dev+apply(DF.AGRO$mat[grep("Average Growth - ",
                                               DF.AGRO$mat$Productos),paste0("",2018:2021)],2,mean))

        DF.AGRO$mat[match("Total Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Sustainable Sourcing",DF.AGRO$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Agro-services",DF.AGRO$mat$Productos),paste0("",2018:2021)]  +
          DF.AGRO$mat[match("Port and Logistics",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        # COGS   ------------------------------------------------------------
        DF.AGRO$mat[match("COGS",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          -(DF.AGRO$mat[match("Sustainable Sourcing",DF.AGRO$mat$Productos),paste0("",2018:2021)]*
              (1-DF.AGRO$mat[match("Gross Margin - Sustainable Sourcing",DF.AGRO$mat$Productos),paste0("",2018:2021)]) +
              DF.AGRO$mat[match("Agro-services",DF.AGRO$mat$Productos),paste0("",2018:2021)]*
              (1-DF.AGRO$mat[match("Gross Margin - Agro-services",DF.AGRO$mat$Productos),paste0("",2018:2021)])+
              DF.AGRO$mat[match("Port and Logistics",DF.AGRO$mat$Productos),paste0("",2018:2021)]*
              (1-DF.AGRO$mat[match("Gross Margin - Port and Logistics",DF.AGRO$mat$Productos),paste0("",2018:2021)]))
        # Gross Margin   ------------------------------------------------------------
        DF.AGRO$mat[match("Gross Margin",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Total Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)]+
          DF.AGRO$mat[match("COGS",DF.AGRO$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Depreciation and Amortization",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        DF.AGRO$mat[match("Margin Before Operating Expenses",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Gross Margin",DF.AGRO$mat$Productos),paste0("",2018:2021)]/
          DF.AGRO$mat[match("Total Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)]
        # Selling and Administrative expenses   ------------------------------------------------------------
        Vol.Growth.Agro <- as.numeric(DF.AGRO$mat[SS2, paste0("",2018:2021)]/
                                        DF.AGRO$mat[SS2, paste0("",2017:2020)])
        DF.AGRO$mat[match("Selling Expense",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Selling Expense", DF.AGRO$mat$Productos),"2017"]*
          cumprod(1+Dev)*cumprod(Vol.Growth.Agro)
        DF.AGRO$mat[match("Administrative Expense",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Administrative Expense", DF.AGRO$mat$Productos),"2017"]*cumprod(as.numeric(CPI.Argentina))
        # EBITDA   ------------------------------------------------------------
        DF.AGRO$mat[match("Results from Operations Before Financing and Tax",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Gross Margin",DF.AGRO$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Selling Expense",DF.AGRO$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Other Income, Net",DF.AGRO$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Administrative Expense",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        DF.AGRO$mat[match("Adjusted Segment EBITDA to Third Parties",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Results from Operations Before Financing and Tax",DF.AGRO$mat$Productos),paste0("",2018:2021)] -
          DF.AGRO$mat[match("Depreciation and Amortization",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        DF.AGRO$mat[match("Margin on Intersegment Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Intersegment Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Intersegment COGS",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        DF.AGRO$mat[match("Reported Adjusted Segment EBITDA",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Adjusted Segment EBITDA to Third Parties",DF.AGRO$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Margin on Intersegment Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        DF.AGRO$mat[match("EBITDA Margin (%)",DF.AGRO$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Reported Adjusted Segment EBITDA",DF.AGRO$mat$Productos),paste0("",2018:2021)]/
          DF.AGRO$mat[match("Total Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)]
        # Inserr final table   ------------------------------------------------------------
        DF.AGRO <- DF.AGRO$mat
        DF.AGRO[is.na(DF.AGRO)] <- 0
        rhandsontable(DF.AGRO,  selectCallback = TRUE, width = 100000) %>%
          hot_col(2:ncol(DF.AGRO), format = "0,0[.]") %>%
          hot_col(1,renderer = Bold_function)
      })

      DF.BS <-reactiveValues(mat=NULL)
      observe({
        if (!is.null(input$BS))
          DF.BS$mat <- hot_to_r(input$BS)
      })
      observe({
        tab <- data.frame(read_excel(Fuente, sheet = "Balance Sheet"))
        tab <- data.frame(tab)
        colnames(tab) <- c("Productos", gsub("X","",colnames(tab)[-1]))
        DF.BS$mat<-tab
      })
      output$BS <- renderRHandsontable({
        DF.BS$mat[,3]<- 1/DF.BS$mat[,2]
        DF.BS <- DF.BS$mat
        rhandsontable(DF.BS,  selectCallback = TRUE, width = 100000) %>%
          hot_col(2:ncol(DF.BS), format = "0,0[.]") %>%
          hot_col(1,renderer = Bold_function)
      })

      DF.IS <-reactiveValues(mat=NULL)
      observe({
        if (!is.null(input$IS))
          DF.IS$mat <- hot_to_r(input$IS)
      })
      observe({
        
        tab <- data.frame(read_excel(Fuente, sheet = "Income Statement"))
        tab <- data.frame(tab)
        colnames(tab) <- c("Productos", gsub("X","",colnames(tab)[-1]))
        DF.IS$mat<-tab
      })
      output$IS <- renderRHandsontable({
        # Common Variables   ------------------------------------------------------------
        CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2018:2021)])
        Industry.Growth <- DF1$mat[which(DF1$mat$Variables == "Industry Growth")+1,paste0("",2018:2021)]

        # Sales   ------------------------------------------------------------
        DF.IS$mat[match("Sales - Retail Products",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Total Sales",DF.Retail$mat$Productos),paste0("",2018:2021)]

        DF.IS$mat[match("Sales - Branded Industrial Products",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.BIP$mat[match("Total Sales",DF.BIP$mat$Productos),paste0("",2018:2021)]

        DF.IS$mat[match("Sales - Agro Services and Sustainable Sourcing",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("Total Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)]
        ##### Total Sales
        DF.IS$mat[match("Total Sales",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Total Sales",DF.Retail$mat$Productos),paste0("",2018:2021)] +
          DF.BIP$mat[match("Total Sales",DF.BIP$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Total Sales",DF.AGRO$mat$Productos),paste0("",2018:2021)]
        ##### Con ProForma
        DF.IS$mat[match("Total Proforma Sales",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Total Sales",DF.Retail$mat$Productos),paste0("",2018:2021)] +
          DF.BIP$mat[match("Total Sales",DF.BIP$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("PROFORMA Agro Services and Sustainable Sourcing",DF.IS$mat$Productos),paste0("",2018:2021)]
        #### Gain from biological assets


        # COGS   ------------------------------------------------------------
        DF.IS$mat[match("Retail Products",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("COGS",DF.Retail$mat$Productos),paste0("",2018:2021)] +
          DF.Retail$mat[match("Depreciation and Amortization",DF.Retail$mat$Productos),paste0("",2018:2021)]

        DF.IS$mat[match("Branded Industrial Products",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.BIP$mat[match("COGS",DF.BIP$mat$Productos),paste0("",2018:2021)] +
          DF.BIP$mat[match("Depreciation and Amortization",DF.BIP$mat$Productos),paste0("",2018:2021)]


        DF.IS$mat[match("Agro Services and Sustainable Sourcing",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.AGRO$mat[match("COGS",DF.AGRO$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Depreciation and Amortization",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        ########### Total COGS
        DF.IS$mat[match("Total COGS",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.IS$mat[match("Retail Products",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Branded Industrial Products",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Agro Services and Sustainable Sourcing",DF.IS$mat$Productos),paste0("",2018:2021)]

        # Total Margin Before Operating Expenses ------------------------------------------------------------
        DF.IS$mat[match("Total Margin Before Operating Expenses",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.IS$mat[match("Total Sales",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Gain Biological asset",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Total COGS",DF.IS$mat$Productos),paste0("",2018:2021)]

        # Selling and Administrative Expenses ------------------------------------------------------------
        DF.IS$mat[match("Selling Expenses",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Selling Expense",DF.Retail$mat$Productos),paste0("",2018:2021)] +
          DF.BIP$mat[match("Selling Expense",DF.BIP$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Selling Expense",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        DF.IS$mat[match("Administrative Expense",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Administrative Expense",DF.Retail$mat$Productos),paste0("",2018:2021)] +
          DF.BIP$mat[match("Administrative Expense",DF.BIP$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Administrative Expense",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        DF.IS$mat[match("Other Operating Income, Net",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.Retail$mat[match("Other Income, Net",DF.Retail$mat$Productos),paste0("",2018:2021)] +
          DF.BIP$mat[match("Other Income, Net",DF.BIP$mat$Productos),paste0("",2018:2021)] +
          DF.AGRO$mat[match("Other Income, Net",DF.AGRO$mat$Productos),paste0("",2018:2021)]

        # Operating Results Before Financing and Tax --------------------------------------

        DF.IS$mat[match("Operating Results Before Financing and Tax",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.IS$mat[match("Total Margin Before Operating Expenses",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Selling Expenses",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Administrative Expense",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Other Operating Income, Net",DF.IS$mat$Productos),paste0("",2018:2021)]
        # Financial Results, Net -----------------------------------------------------------
        DF.IS$mat[match("Financial Results, Net",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.IS$mat[match("Financial Income",DF.IS$mat$Productos),paste0("",2018:2021)]  +
          DF.IS$mat[match("Financial Costs",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Exchange Differences, Net",DF.IS$mat$Productos),paste0("",2018:2021)]

        # Profit Before Income Tax -------------------------------------------------------------
        DF.IS$mat[match("Profit Before Income Tax",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.IS$mat[match("Operating Results Before Financing and Tax",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Financial Results, Net",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Gain on Acquisition of Business",DF.IS$mat$Productos),paste0("",2018:2021)]
        ############# Income Tax
        DF.IS$mat[match("Income Tax",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          -DF.IS$mat[match("Profit Before Income Tax",DF.IS$mat$Productos),paste0("",2018:2021)]*0.35
        ############# Net Profit
        DF.IS$mat[match("Net Profit",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.IS$mat[match("Profit Before Income Tax",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Income Tax",DF.IS$mat$Productos),paste0("",2018:2021)]
        ############# Adjusted EBITDA
        DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2018:2021)] <-
          DF.IS$mat[match("Operating Results Before Financing and Tax",DF.IS$mat$Productos),paste0("",2018:2021)] +
          DF.IS$mat[match("Depreciation and Amortization",DF.IS$mat$Productos),paste0("",2018:2021)]
        # Inserr final table   ------------------------------------------------------------
        DF.IS <- DF.IS$mat
        rhandsontable(DF.IS,  selectCallback = TRUE, width = 100000) %>%
          hot_col(2:ncol(DF.IS), format = "0,0[.]") %>%
          hot_col(1,renderer = Bold_function)
      })


      DF.CF <-reactiveValues(mat=NULL)
      observe({
        if (!is.null(input$CF))
          DF.CF$mat <- hot_to_r(input$CF)
      })
      observe({
        nb<-input$dim
        tab <- data.frame(read_excel(Fuente, sheet = "CashFlows"))
        tab <- data.frame(tab)
        colnames(tab) <- c("Productos", gsub("X","",colnames(tab)[-1]))
        DF.CF$mat<-tab
      })
      output$CF <- renderRHandsontable({
        DF.CF$mat[,3]<- 1/DF.CF$mat[,2]
        DF.CF <- DF.CF$mat
        rhandsontable(DF.CF,  selectCallback = TRUE, width = 100000)   %>%
          hot_col(2:ncol(DF.CF), format = "0,0[.]") %>%
          hot_col(1,renderer = Bold_function)
      })

      
      output$Inputplot = renderPlotly({
        Product.Retail <- c("Flour Products", "Oil", "Biscuits, Cookies and Crackers", "Bread Crumbs and Premixes (& Pasta)", "Frozen Products")
        Product.Agro <- c("Wheat","Soybean","Corn","Others")
        Product.BIP <- c("Wheat Flour","MRP","Soybean Flour and Co-Products","Cañuelas Pack")
        
        Value <-  eventReactive(input$submit1, {
          c(input$inflation2019,input$inflation2020,input$inflation2021) })
        
        Value1 <-  eventReactive(input$submit1, {
          c(input$CPI.USA2019,input$CPI.USA2020,input$CPI.USA2021) })
        
        Value2 <-  eventReactive(input$submit1, {
          c(input$Activity2019,input$Activity2020,input$Activity2021) })
        
        if(input$Default == TRUE) {
          CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2019:2021)])
          CPI.USA <- (1+DF1$mat[which(DF1$mat$Variables == "CPI US"),paste0("",2019:2021)])
          Economic.Growth <- (1+DF1$mat[which(DF1$mat$Variables == "Argentina"),paste0("",2019:2021)]) }
        else { CPI.Argentina <- (1+Value())
        CPI.USA <- (1+Value1())
        Economic.Growth <- (1+Value2())}
        
        if(input$Default == TRUE) {
          Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),paste0("",2019:2021)]
          Devaluation.Argentina.AVG <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2019:2021)]}
        else { Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"]*cumprod(as.numeric((CPI.Argentina/CPI.USA)))
        Devaluation.Argentina.AVG <- (0.5*(c(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP))[1:3]+
                                               as.numeric(Devaluation.Argentina.EOP)))) }
        
        DEV <- data.frame(Devaluación = c(as.numeric(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),paste0("",2014:2018)]),as.numeric(Devaluation.Argentina.EOP)),
                          Time = 2014:2021)
        qplot(Time, Devaluación, data=DEV) + geom_smooth(method = "glm", formula = y~x)  
        })
      
      output$Sales = renderPlotly({
        Product.Retail <- c("Flour Products", "Oil", "Biscuits, Cookies and Crackers", "Bread Crumbs and Premixes (& Pasta)", "Frozen Products")
        Product.Agro <- c("Wheat","Soybean","Corn","Others")
        Product.BIP <- c("Wheat Flour","MRP","Soybean Flour and Co-Products","Cañuelas Pack")
        
        Value <-  eventReactive(input$submit1, {
          c(input$inflation2019,input$inflation2020,input$inflation2021) })
        
        Value1 <-  eventReactive(input$submit1, {
          c(input$CPI.USA2019,input$CPI.USA2020,input$CPI.USA2021) })
        
        Value2 <-  eventReactive(input$submit1, {
          c(input$Activity2019,input$Activity2020,input$Activity2021) })

        if(input$Default == TRUE) {
          CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2019:2021)])
          CPI.USA <- (1+DF1$mat[which(DF1$mat$Variables == "CPI US"),paste0("",2019:2021)])
          Economic.Growth <- (1+DF1$mat[which(DF1$mat$Variables == "Argentina"),paste0("",2019:2021)]) }
        else { CPI.Argentina <- (1+Value())
               CPI.USA <- (1+Value1())
               Economic.Growth <- (1+Value2())}
        
       if(input$Default == TRUE) {
          Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),paste0("",2019:2021)]
          Devaluation.Argentina.AVG <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2019:2021)]}
        else { Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"]*cumprod(as.numeric((CPI.Argentina/CPI.USA)))
               Devaluation.Argentina.AVG <- (0.5*(c(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP))[1:3]+
                                                      as.numeric(Devaluation.Argentina.EOP)))) }

        RP.TotalSales <- colSums(as.numeric(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),"2018"])%*%
                                    t(cumprod(as.numeric(CPI.Argentina)))*
                                    as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                       rbind(cumprod(as.numeric(Economic.Growth)),
                                             cumprod(as.numeric(Economic.Growth)),
                                             cumprod(c(1.10,1.10,1.10)),
                                             cumprod(c(1.10,1.10,1.10)),
                                             cumprod(c(1.30,1.30,1.30)))/1000000)
        
                  
        
        RP.TotalSales2018 <- as.numeric(DF.Retail$mat[match("Total Sales", DF.Retail$mat$Productos),"2018"])
        RP.TotalSales <- c(RP.TotalSales2018, RP.TotalSales)
        names(RP.TotalSales) <- c("2018","2019","2020","2021")
        
        Sustainable.Sourcing <- colSums(as.numeric(DF.AGRO$mat[match(paste0("Third parties - ",Product.Agro), DF.AGRO$mat$Productos),"2018"])%*%
                                    t(cumprod(1+c(0.008,0.008,0.008)))* 
                                    (DF.AGRO$mat[match(paste0("Third partiesAdj - ",Product.Agro), DF.AGRO$mat$Productos),"2018"]/20.59250)%*%
                                    t(as.numeric(Devaluation.Argentina.AVG))/1000000)
        
        TotalSales.AgroServices <- 4166.78554012*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))
        TotalSales.LogisticandPort <- 1172.445667*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))
        
        AG.TotalSales <- Sustainable.Sourcing + TotalSales.AgroServices + TotalSales.LogisticandPort
        
        AG.TotalSales2018 <- DF.AGRO$mat[match("Total Sales", DF.AGRO$mat$Productos),"2018"]
        AG.TotalSales <- c(AG.TotalSales2018, AG.TotalSales)
        names(AG.TotalSales) <- c("2018","2019","2020","2021")
        
        BIP.Growth <- as.numeric(ifelse(CPI.Argentina > Delt(as.numeric(c(DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),"2018"],Devaluation.Argentina.EOP)))[-1],
                                          CPI.Argentina - Delt(as.numeric(c(DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),"2018"],Devaluation.Argentina.EOP)))[-1],1))
        
        Groth.Packaging <- pmax(Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1],CPI.Argentina)  
        
        BIP.TotalSales <-   colSums((as.numeric(DF.BIP$mat[match(paste0("Average Price - ",Product.BIP[-4]), DF.BIP$mat$Productos),"2018"]/20.592500)%*%
                                       t(cumprod(as.numeric(BIP.Growth))))*
                                       rbind(Devaluation.Argentina.AVG,
                                            Devaluation.Argentina.AVG,
                                            Devaluation.Argentina.AVG)*
                                       apply(DF.BIP$mat[match(Product.BIP[-4], DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric)/1000000) + 
                                       DF.BIP$mat[match(Product.BIP[4], DF.BIP$mat$Productos),"2018"]*cumprod(1+as.numeric((Groth.Packaging*
                                       (1+Delt(c(sum(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"]),
                                                 colSums(as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                                           rbind(cumprod(as.numeric(Economic.Growth)),
                                                                 cumprod(as.numeric(Economic.Growth)),
                                                                 cumprod(c(1.10,1.10,1.10)),
                                                                 cumprod(c(1.10,1.10,1.10)),
                                                                 cumprod(c(1.30,1.30,1.30))))))[-1]))-1))
        
        # cumprod(as.numeric(((Economic.Growth-1)*c(3.1084862,3.2041012,3.1318509)+1))),
        # cumprod(as.numeric(((Economic.Growth-1)*c(3.1084862,3.2041012,3.1318509)+1))),
        # cumprod(as.numeric(((Economic.Growth-1)*c(9.3254585,9.6123037,9.3955528))+1))
        
        
        BIP.TotalSales2018 <- DF.BIP$mat[match("Total Sales", DF.BIP$mat$Productos),"2018"]
        BIP.TotalSales <- c(BIP.TotalSales2018, BIP.TotalSales)
        names(BIP.TotalSales) <- c("2018","2019","2020","2021")
        
        Sales <- data.frame(Segment = c("RP","BIP","ASSS"),
                            Sales =   c(RP.TotalSales[paste0("",input$month)],
                                        BIP.TotalSales[paste0("",input$month)],
                                        AG.TotalSales[paste0("",input$month)]))
        colors=c("#0059b3", "#0086b3", "#001f4d")
        plot_ly(Sales, labels = ~Segment, values = ~Sales, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste('$', round(Sales,0), ' Millions'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE) %>%
          layout(title = paste('Sales by Segment -', paste0("",input$month)),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
          layout(plot_bgcolor='transparent') %>%
          layout(paper_bgcolor='transparent')



      })

      output$Cogs = renderPlotly({

        Product.Retail <- c("Flour Products", "Oil", "Biscuits, Cookies and Crackers", "Bread Crumbs and Premixes (& Pasta)", "Frozen Products")
        Product.Agro <- c("Wheat","Soybean","Corn","Others")
        Product.BIP <- c("Wheat Flour","MRP","Soybean Flour and Co-Products","Cañuelas Pack")
        
        Value <-  eventReactive(input$submit1, {
          c(input$inflation2019,input$inflation2020,input$inflation2021) })
        
        Value1 <-  eventReactive(input$submit1, {
          c(input$CPI.USA2019,input$CPI.USA2020,input$CPI.USA2021) })
        
        Value2 <-  eventReactive(input$submit1, {
          c(input$Activity2019,input$Activity2020,input$Activity2021) })
        
        if(input$Default == TRUE) {
          CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2019:2021)])
          CPI.USA <- (1+DF1$mat[which(DF1$mat$Variables == "CPI US"),paste0("",2019:2021)])
          Economic.Growth <- (1+DF1$mat[which(DF1$mat$Variables == "Argentina"),paste0("",2019:2021)]) }
        else { CPI.Argentina <- (1+Value())
        CPI.USA <- (1+Value1())
        Economic.Growth <- (1+Value2())}
        
        if(input$Default == TRUE) {
          Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),paste0("",2019:2021)]
          Devaluation.Argentina.AVG <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2019:2021)]}
        else { Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"]*cumprod(as.numeric((CPI.Argentina/CPI.USA)))
        Devaluation.Argentina.AVG <- (0.5*(c(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP))[1:3]+
                                               as.numeric(Devaluation.Argentina.EOP)))) }
        
        RP.COGS <- colSums(as.numeric(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),"2018"])%*%
                  t(cumprod(as.numeric(CPI.Argentina)))*
                  (1-apply(DF.Retail$mat[match(paste0("Average Margins - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2019:2021)],2,as.numeric))* 
                  as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                    rbind(cumprod(as.numeric(Economic.Growth)),
                         cumprod(as.numeric(Economic.Growth)),
                         cumprod(c(1.10,1.10,1.10)),
                         cumprod(c(1.10,1.10,1.10)),
                         cumprod(c(1.30,1.30,1.30)))/1000000)
        
                
        RP.COGS2018 <- -as.numeric(DF.Retail$mat[match("COGS", DF.Retail$mat$Productos),"2018"])
        RP.COGS <- c(RP.COGS2018, RP.COGS)
        names(RP.COGS) <- c("2018","2019","2020","2021")
        
        Sustainable.Sourcing <- colSums(as.numeric(DF.AGRO$mat[match(paste0("Third parties - ",Product.Agro), DF.AGRO$mat$Productos),"2018"])%*%
                                          t(cumprod(1+c(0.008,0.008,0.008)))* 
                                          (DF.AGRO$mat[match(paste0("Third partiesAdj - ",Product.Agro), DF.AGRO$mat$Productos),"2018"]/20.59250)%*%
                                          t(as.numeric(Devaluation.Argentina.AVG))/1000000)*
                                          (1 - apply(DF.AGRO$mat[match("Gross Margin - Sustainable Sourcing", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        TotalCOGS.AgroServices <- 4166.78554012*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))*
                                   (1 - apply(DF.AGRO$mat[match("Gross Margin - Agro-services", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        TotalCOGS.LogisticandPort <- 1172.445667*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))*
                                       (1 - apply(DF.AGRO$mat[match("Gross Margin - Port and Logistics", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        AG.TotalCOGS <- Sustainable.Sourcing + TotalCOGS.AgroServices + TotalCOGS.LogisticandPort
        
        AG.TotalCOGS2018 <- -DF.AGRO$mat[match("COGS", DF.AGRO$mat$Productos),"2018"]
        AG.TotalCOGS <- c(AG.TotalCOGS2018, AG.TotalCOGS)
        names(AG.TotalCOGS) <- c("2018","2019","2020","2021")
        
        BIP.Growth <- as.numeric(ifelse(CPI.Argentina > Delt(as.numeric(c(DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),"2018"],Devaluation.Argentina.EOP)))[-1],
                                        CPI.Argentina - Delt(as.numeric(c(DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),"2018"],Devaluation.Argentina.EOP)))[-1],1))
        
        Groth.Packaging <- pmax(Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1],CPI.Argentina)  
        
        BIP.COGS <-   colSums((as.numeric(DF.BIP$mat[match(paste0("Average Price - ",Product.BIP[-4]), DF.BIP$mat$Productos),"2018"]/20.592500)%*%
                                 t(cumprod(as.numeric(BIP.Growth))))*
                                rbind(Devaluation.Argentina.AVG,
                                      Devaluation.Argentina.AVG,
                                      Devaluation.Argentina.AVG)*
                                (as.numeric(DF.BIP$mat[match(Product.BIP[-4], DF.BIP$mat$Productos),"2018"])*
                                   rbind(cumprod(c(1.10145,1.09202,1.08339)),
                                         cumprod(c(1.03217,1.03121,1.03193)),
                                         cumprod(c(.70,0.80,0.80)))/1000000)*
                                (1 - apply(DF.BIP$mat[match(paste0("Average Margins - ",Product.BIP[-4]), DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric)))+ 
          DF.BIP$mat[match(Product.BIP[4], DF.BIP$mat$Productos),"2018"]*cumprod(1+as.numeric((Groth.Packaging*
                                                                                                 (1+Delt(c(sum(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"]),
                                                                                                           colSums(as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                                                                                                     rbind(cumprod(as.numeric(Economic.Growth)),
                                                                                                                           cumprod(as.numeric(Economic.Growth)),
                                                                                                                           cumprod(c(1.10,1.10,1.10)),
                                                                                                                           cumprod(c(1.10,1.10,1.10)),
                                                                                                                           cumprod(c(1.30,1.30,1.30))))))[-1]))-1))*
          (1 - apply(DF.BIP$mat[match(paste0("Average Margins - ",Product.BIP[4]), DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        BIP.COGS2018 <- -DF.BIP$mat[match("COGS", DF.BIP$mat$Productos),"2018"]
        BIP.COGS <- c(BIP.COGS2018, BIP.COGS)
        names(BIP.COGS) <- c("2018","2019","2020","2021")

        
        Cogs <- data.frame(Segment = c("RP", "BIP","ASSS"),
                           Cogs =   c(RP.COGS[paste0("",input$month)],
                                      BIP.COGS[paste0("",input$month)],
                                      AG.TotalCOGS[paste0("",input$month)]))
        colors=c("#0059b3", "#0086b3", "#001f4d")
        plot_ly(Cogs, labels = ~Segment, values = ~Cogs, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste('$', round(Cogs,0), ' Millions'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE) %>%
          layout(title = paste('COGS by Segment -', paste0("",input$month)),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
          layout(plot_bgcolor='transparent') %>%
          layout(paper_bgcolor='transparent')

      })

      output$Ebitda = renderPlotly({
        # The following code runs inside the databas
        Product.Retail <- c("Flour Products", "Oil", "Biscuits, Cookies and Crackers", "Bread Crumbs and Premixes (& Pasta)", "Frozen Products")
        Product.Agro <- c("Wheat","Soybean","Corn","Others")
        Product.BIP <- c("Wheat Flour","MRP","Soybean Flour and Co-Products","Cañuelas Pack")
        
        Value <-  eventReactive(input$submit1, {
          c(input$inflation2019,input$inflation2020,input$inflation2021) })
        
        Value1 <-  eventReactive(input$submit1, {
          c(input$CPI.USA2019,input$CPI.USA2020,input$CPI.USA2021) })
        
        Value2 <-  eventReactive(input$submit1, {
          c(input$Activity2019,input$Activity2020,input$Activity2021) })
        
        if(input$Default == TRUE) {
          CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2019:2021)])
          CPI.USA <- (1+DF1$mat[which(DF1$mat$Variables == "CPI US"),paste0("",2019:2021)])
          Economic.Growth <- (1+DF1$mat[which(DF1$mat$Variables == "Argentina"),paste0("",2019:2021)]) }
        else { CPI.Argentina <- (1+Value())
        CPI.USA <- (1+Value1())
        Economic.Growth <- (1+Value2())}
        
        if(input$Default == TRUE) {
          Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),paste0("",2019:2021)]
          Devaluation.Argentina.AVG <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2019:2021)]}
        else { Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"]*cumprod(as.numeric((CPI.Argentina/CPI.USA)))
        Devaluation.Argentina.AVG <- (0.5*(c(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP))[1:3]+
                                               as.numeric(Devaluation.Argentina.EOP)))) }
       
        Capacity.Expan <- c(25.00,41.00,25.00)
        Depreciation <- NULL
        Final.Period <- NULL
        Maintenance <- NULL
        
        for(i in 1:length(Capacity.Expan)){ 
          if(i == 1){
            Final.Period[i]  <- 17630.48
            Maintenance[i] <- Final.Period[i]*0.02/as.numeric(Devaluation.Argentina.AVG)[i]
            Depreciation[i] <- -as.numeric(((Devaluation.Argentina.AVG*(Capacity.Expan + Maintenance))[[i]] + Final.Period[[i]] + Final.Period[[i]]*(Delt(c(as.numeric(DF1$mat[
              which(DF1$mat$Variables == "ARS/USD (EOP)"),paste0("",2018:2020)])[i],as.numeric(Devaluation.Argentina.EOP)[i]))[[-1]]))/22.10) 
          } else {
            Final.Period[i] <- as.numeric(Final.Period[[i-1]] +  Depreciation[[i-1]] + (Devaluation.Argentina.AVG*(Capacity.Expan[i-1] + Maintenance[i-1]))[i-1] + 
                                          Final.Period[[i-1]]*(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP))[i]/
                                          as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP)[i-1]))-Final.Period[[i-1]])
            Maintenance[i] <- Final.Period[i]*0.02/as.numeric(Devaluation.Argentina.AVG)[i]
            
            Depreciation[i] <- -as.numeric((Devaluation.Argentina.AVG*(Capacity.Expan[i] + Maintenance[i]))[[i]] + Final.Period[[i]] + Final.Period[[i]]*
                                (1+Delt(c(as.numeric(Devaluation.Argentina.EOP[i-1]),as.numeric(Devaluation.Argentina.EOP)[i]))[[-1]])-Final.Period[[i]])/
                                  22.102698 
            
            
            
          }
        }
        
        
         
        RP.TotalSales <- colSums(as.numeric(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),"2018"])%*%
                                   t(cumprod(as.numeric(CPI.Argentina)))*
                                   as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                   rbind(cumprod(as.numeric(Economic.Growth)),
                                         cumprod(as.numeric(Economic.Growth)),
                                         cumprod(c(1.10,1.10,1.10)),
                                         cumprod(c(1.10,1.10,1.10)),
                                         cumprod(c(1.30,1.30,1.30)))/1000000)
        
        
        
        RP.TotalSales2018 <- as.numeric(DF.Retail$mat[match("Total Sales", DF.Retail$mat$Productos),"2018"])
        RP.TotalSales <- c(RP.TotalSales2018, RP.TotalSales)
        names(RP.TotalSales) <- c("2018","2019","2020","2021")
        
        
        RP.COGS <- colSums(as.numeric(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),"2018"])%*%
                             t(cumprod(as.numeric(CPI.Argentina)))*
                             (1-apply(DF.Retail$mat[match(paste0("Average Margins - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2019:2021)],2,as.numeric))* 
                             as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                             rbind(cumprod(as.numeric(Economic.Growth)),
                                   cumprod(as.numeric(Economic.Growth)),
                                   cumprod(c(1.10,1.10,1.10)),
                                   cumprod(c(1.10,1.10,1.10)),
                                   cumprod(c(1.30,1.30,1.30)))/1000000)
        
        RP.COGS2018 <- -as.numeric(DF.Retail$mat[match("COGS", DF.Retail$mat$Productos),"2018"])
        RP.COGS <- c(RP.COGS2018, RP.COGS)
        names(RP.COGS) <- c("2018","2019","2020","2021")
        
        RP.Gross.Margin <- c(DF.Retail$mat[match("Depreciation and Amortization",DF.Retail$mat$Productos),"2018"],Depreciation*0.41360) + RP.TotalSales - RP.COGS
                      
        Vol.Change <-c(sum(as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])),
                       colSums(as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                rbind(cumprod(as.numeric(Economic.Growth)),
                                      cumprod(as.numeric(Economic.Growth)),
                                      cumprod(c(1.10,1.10,1.10)),
                                      cumprod(c(1.10,1.10,1.10)),
                                      cumprod(c(1.30,1.30,1.30)))))
        
        Selling.Expense <- DF.Retail$mat[match("Selling Expense", DF.Retail$mat$Productos),"2018"]*
                           cumprod(as.numeric(CPI.Argentina)*(((1+Delt(Vol.Change)[-1]))))
        
        Administrative.Expense <- DF.Retail$mat[match("Administrative Expense", DF.Retail$mat$Productos),"2018"]*
                                  cumprod(as.numeric(CPI.Argentina))
        
        RP.EBITDA <- (RP.Gross.Margin) + 
                        c(DF.Retail$mat[match("Selling Expense", DF.Retail$mat$Productos),"2018"],Selling.Expense) +
                        c(DF.Retail$mat[match("Administrative Expense", DF.Retail$mat$Productos),"2018"],Administrative.Expense) +
                        -c(DF.Retail$mat[match("Depreciation and Amortization",DF.Retail$mat$Productos),"2018"],Depreciation*0.41360)
        
        ######## BIP
        BIP.Growth <- as.numeric(ifelse(CPI.Argentina > Delt(as.numeric(c(DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),"2018"],Devaluation.Argentina.EOP)))[-1],
                                        CPI.Argentina - Delt(as.numeric(c(DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),"2018"],Devaluation.Argentina.EOP)))[-1],1))
        
        Groth.Packaging <- pmax(Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1],CPI.Argentina)  
        
        BIP.TotalSales <-   colSums((as.numeric(DF.BIP$mat[match(paste0("Average Price - ",Product.BIP[-4]), DF.BIP$mat$Productos),"2018"]/20.592500)%*%
                                       t(cumprod(as.numeric(BIP.Growth))))*
                                      rbind(Devaluation.Argentina.AVG,
                                            Devaluation.Argentina.AVG,
                                            Devaluation.Argentina.AVG)*
                                      apply(DF.BIP$mat[match(Product.BIP[-4], DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric)/1000000) + 
          DF.BIP$mat[match(Product.BIP[4], DF.BIP$mat$Productos),"2018"]*cumprod(1+as.numeric((Groth.Packaging*
                                                                                                 (1+Delt(c(sum(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"]),
                                                                                                           colSums(as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                                                                                                     rbind(cumprod(as.numeric(Economic.Growth)),
                                                                                                                           cumprod(as.numeric(Economic.Growth)),
                                                                                                                           cumprod(c(1.10,1.10,1.10)),
                                                                                                                           cumprod(c(1.10,1.10,1.10)),
                                                                                                                           cumprod(c(1.30,1.30,1.30))))))[-1]))-1))
        
        
        BIP.TotalSales2018 <- DF.BIP$mat[match("Total Sales", DF.BIP$mat$Productos),"2018"]
        BIP.TotalSales <- c(BIP.TotalSales2018, BIP.TotalSales)
        names(BIP.TotalSales) <- c("2018","2019","2020","2021")
        
        BIP.COGS <-   colSums((as.numeric(DF.BIP$mat[match(paste0("Average Price - ",Product.BIP[-4]), DF.BIP$mat$Productos),"2018"]/20.592500)%*%
                                 t(cumprod(as.numeric(BIP.Growth))))*
                                rbind(Devaluation.Argentina.AVG,
                                      Devaluation.Argentina.AVG,
                                      Devaluation.Argentina.AVG)*
                                (as.numeric(DF.BIP$mat[match(Product.BIP[-4], DF.BIP$mat$Productos),"2018"])*
                                   rbind(cumprod(c(1.10145,1.09202,1.08339)),
                                         cumprod(c(1.03217,1.03121,1.03193)),
                                         cumprod(c(.70,0.80,0.80)))/1000000)*
                                (1 - apply(DF.BIP$mat[match(paste0("Average Margins - ",Product.BIP[-4]), DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric)))+ 
          DF.BIP$mat[match(Product.BIP[4], DF.BIP$mat$Productos),"2018"]*cumprod(1+as.numeric((Groth.Packaging*
 (1+Delt(c(sum(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"]),
           colSums(as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                     rbind(cumprod(as.numeric(Economic.Growth)),
                           cumprod(as.numeric(Economic.Growth)),
                           cumprod(c(1.10,1.10,1.10)),
                           cumprod(c(1.10,1.10,1.10)),
                           cumprod(c(1.30,1.30,1.30))))))[-1]))-1))*
       (1 - apply(DF.BIP$mat[match(paste0("Average Margins - ",Product.BIP[4]), DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        BIP.COGS2018 <- -DF.BIP$mat[match("COGS", DF.BIP$mat$Productos),"2018"]
        BIP.COGS <- c(BIP.COGS2018, BIP.COGS)
        names(BIP.COGS) <- c("2018","2019","2020","2021")
        
        BP.Gross.Margin <- c(DF.BIP$mat[match("Depreciation and Amortization",DF.BIP$mat$Productos),"2018"],Depreciation*0.44875) + BIP.TotalSales - BIP.COGS
        
        Growth.Vol.Wheat <- (1+Delt(c(sum(as.numeric(DF.BIP$mat[match(Product.BIP[-c(3,4)], DF.BIP$mat$Productos),"2018"])),
                            colSums(as.numeric(DF.BIP$mat[match(Product.BIP[-c(3,4)], DF.BIP$mat$Productos),"2018"])*
                            rbind(cumprod(1+as.numeric((Economic.Growth-1)*c(3.15359,2.94845,2.61171))),cumprod(1+as.numeric(Economic.Growth-1))))))[-1])*
                            CPI.Argentina
        
         
          
        Selling.Expense <-  colSums(rbind(-1568.14593002*cumprod(as.numeric(Growth.Vol.Wheat)),
                                    -(as.numeric(DF.BIP$mat[match(Product.BIP[3], DF.BIP$mat$Productos),"2018"])*
                                      cumprod(1+as.numeric(Economic.Growth-1)*(-c(9.32546,6.40820,6.26370)))*
                                      (as.numeric(DF.BIP$mat[match(paste0("Average Price - ",Product.BIP[3]), DF.BIP$mat$Productos),"2018"]/20.592500)*
                                         t(cumprod(as.numeric(BIP.Growth))))*Devaluation.Argentina.AVG)/1000000*c(0.29,0.29,0.29)))
        
        Administrative.Expense <- DF.BIP$mat[match("Administrative Expense", DF.BIP$mat$Productos),"2018"]*
                                  cumprod(as.numeric(CPI.Argentina))
                      
        BP.EBITDA <-  (BP.Gross.Margin) + 
                      c(DF.BIP$mat[match("Selling Expense", DF.BIP$mat$Productos),"2018"],Selling.Expense) +
                      c(DF.BIP$mat[match("Administrative Expense", DF.BIP$mat$Productos),"2018"],Administrative.Expense) +
                      -c(DF.BIP$mat[match("Depreciation and Amortization",DF.BIP$mat$Productos),"2018"],Depreciation*0.44875)
        
        
        #################AGROBusiness
        
        Sustainable.Sourcing <- colSums(as.numeric(DF.AGRO$mat[match(paste0("Third parties - ",Product.Agro), DF.AGRO$mat$Productos),"2018"])%*%
                                          t(cumprod(1+c(0.008,0.008,0.008)))* 
                                          (DF.AGRO$mat[match(paste0("Third partiesAdj - ",Product.Agro), DF.AGRO$mat$Productos),"2018"]/20.59250)%*%
                                          t(as.numeric(Devaluation.Argentina.AVG))/1000000)*
          (1 - apply(DF.AGRO$mat[match("Gross Margin - Sustainable Sourcing", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        TotalCOGS.AgroServices <- 4166.78554012*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))*
                                      (1 - apply(DF.AGRO$mat[match("Gross Margin - Agro-services", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        TotalCOGS.LogisticandPort <- 1172.445667*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))*
                                       (1 - apply(DF.AGRO$mat[match("Gross Margin - Port and Logistics", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        AG.TotalCOGS <- Sustainable.Sourcing + TotalCOGS.AgroServices + TotalCOGS.LogisticandPort
        
        AG.TotalCOGS2018 <- -DF.AGRO$mat[match("COGS", DF.AGRO$mat$Productos),"2018"]
        AG.TotalCOGS <- c(AG.TotalCOGS2018, AG.TotalCOGS)
        names(AG.TotalCOGS) <- c("2018","2019","2020","2021")
        
        
        Sustainable.Sourcing <- colSums(as.numeric(DF.AGRO$mat[match(paste0("Third parties - ",Product.Agro), DF.AGRO$mat$Productos),"2018"])%*%
                                          t(cumprod(1+c(0.008,0.008,0.008)))* 
                                          (DF.AGRO$mat[match(paste0("Third partiesAdj - ",Product.Agro), DF.AGRO$mat$Productos),"2018"]/20.59250)%*%
                                          t(as.numeric(Devaluation.Argentina.AVG))/1000000)
        
        TotalSales.AgroServices <- 4166.78554012*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))
        TotalSales.LogisticandPort <- 1172.445667*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))
        
        AG.TotalSales <- Sustainable.Sourcing + TotalSales.AgroServices + TotalSales.LogisticandPort
        
        AG.TotalSales2018 <- DF.AGRO$mat[match("Total Sales", DF.AGRO$mat$Productos),"2018"]
        AG.TotalSales <- c(AG.TotalSales2018, AG.TotalSales)
        names(AG.TotalSales) <- c("2018","2019","2020","2021")
        
        Biological.Assets <- c(DF.AGRO$mat[match("Gain Biological asset",DF.AGRO$mat$Productos),"2018"],
                               DF.AGRO$mat[match("Gain Biological asset",DF.AGRO$mat$Productos),"2018"]*
                                 cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),
                                 paste0("",2018)],Devaluation.Argentina.AVG)))[-1])))
        
        ASSS.Gross.Margin <- c(DF.AGRO$mat[match("Depreciation and Amortization",DF.AGRO$mat$Productos),"2018"],Depreciation*0.13765) + Biological.Assets + AG.TotalSales - AG.TotalCOGS
        
        Selling.Expense <- c(DF.AGRO$mat[match("Selling Expense",DF.AGRO$mat$Productos),"2018"],
                             DF.AGRO$mat[match("Selling Expense",DF.AGRO$mat$Productos),"2018"]*
                               cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),
                               paste0("",2018)],Devaluation.Argentina.AVG)))[-1]))*
                               cumprod(1+c(0.008,0.008,0.008)))
        
        Administrative.Expense <- c(DF.AGRO$mat[match("Administrative Expense",DF.AGRO$mat$Productos),"2018"],
                                    DF.AGRO$mat[match("Administrative Expense",DF.AGRO$mat$Productos),"2018"]*
                                      cumprod(as.numeric(CPI.Argentina)))
          
        Others <- DF.AGRO$mat[match("Other Income, Net",DF.AGRO$mat$Productos),paste0("",2018:2021)]
        
        ASSS.EBITDA <- ASSS.Gross.Margin + Selling.Expense + Administrative.Expense + Others - 
                         c(DF.AGRO$mat[match("Depreciation and Amortization",DF.AGRO$mat$Productos),"2018"],Depreciation*0.13765)
        
        Ebitda <- data.frame(Segment = c("RP", "BIP","ASSS"),
                             Ebitda = c(as.numeric(RP.EBITDA[paste0("",input$month)]),
                                        as.numeric(BP.EBITDA[paste0("",input$month)]),
                                        as.numeric(ASSS.EBITDA[paste0("",input$month)])))
        
        colors=c("#0059b3", "#0086b3", "#001f4d")
        plot_ly(Ebitda, labels = ~Segment, values = ~Ebitda, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste('$', round(Ebitda,0), ' Millions'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE) %>%
          layout(title = paste('EBITDA by Segment -', paste0("",input$month)),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
          layout(plot_bgcolor='transparent') %>%
          layout(paper_bgcolor='transparent')

      })

      output$Ebitdapct = renderPlotly({

        # The following code runs inside the databas
        Product.Retail <- c("Flour Products", "Oil", "Biscuits, Cookies and Crackers", "Bread Crumbs and Premixes (& Pasta)", "Frozen Products")
        Product.Agro <- c("Wheat","Soybean","Corn","Others")
        Product.BIP <- c("Wheat Flour","MRP","Soybean Flour and Co-Products","Cañuelas Pack")
        
        Value <-  eventReactive(input$submit1, {
          c(input$inflation2019,input$inflation2020,input$inflation2021) })
        
        Value1 <-  eventReactive(input$submit1, {
          c(input$CPI.USA2019,input$CPI.USA2020,input$CPI.USA2021) })
        
        Value2 <-  eventReactive(input$submit1, {
          c(input$Activity2019,input$Activity2020,input$Activity2021) })
        
        if(input$Default == TRUE) {
          CPI.Argentina <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),paste0("",2019:2021)])
          CPI.USA <- (1+DF1$mat[which(DF1$mat$Variables == "CPI US"),paste0("",2019:2021)])
          Economic.Growth <- (1+DF1$mat[which(DF1$mat$Variables == "Argentina"),paste0("",2019:2021)]) }
        else { CPI.Argentina <- (1+Value())
        CPI.USA <- (1+Value1())
        Economic.Growth <- (1+Value2())}
        
        if(input$Default == TRUE) {
          Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),paste0("",2019:2021)]
          Devaluation.Argentina.AVG <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2019:2021)]}
        else { Devaluation.Argentina.EOP <- DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"]*cumprod(as.numeric((CPI.Argentina/CPI.USA)))
        Devaluation.Argentina.AVG <- (0.5*(c(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP))[1:3]+
                                               as.numeric(Devaluation.Argentina.EOP)))) }
        
        
        Capacity.Expan <- c(25.00,41.00,25.00)
        Depreciation <- NULL
        Final.Period <- NULL
        Maintenance <- NULL
        for(i in 1:length(Capacity.Expan)){ 
          if(i == 1){
            Final.Period[i]  <- 17630.48
            Maintenance[i] <- Final.Period[i]*0.02/as.numeric(Devaluation.Argentina.AVG)[i]
            Depreciation[i] <- -as.numeric(((Devaluation.Argentina.AVG*(Capacity.Expan + Maintenance))[[i]] + Final.Period[[i]] + Final.Period[[i]]*(Delt(c(as.numeric(DF1$mat[
              which(DF1$mat$Variables == "ARS/USD (EOP)"),paste0("",2018:2020)])[i],as.numeric(Devaluation.Argentina.EOP)[i]))[[-1]]))/22.10) 
          } else {
            Final.Period[i] <- as.numeric(Final.Period[[i-1]] +  Depreciation[[i-1]] + (Devaluation.Argentina.AVG*(Capacity.Expan[i-1] + Maintenance[i-1]))[i-1] + 
                                            Final.Period[[i-1]]*(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP))[i]/
                                                                   as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (EOP)"),"2018"],Devaluation.Argentina.EOP)[i-1]))-Final.Period[[i-1]])
            Maintenance[i] <- Final.Period[i]*0.02/as.numeric(Devaluation.Argentina.AVG)[i]
            
            Depreciation[i] <- -as.numeric((Devaluation.Argentina.AVG*(Capacity.Expan[i] + Maintenance[i]))[[i]] + Final.Period[[i]] + Final.Period[[i]]*
                                             (1+Delt(c(as.numeric(Devaluation.Argentina.EOP[i-1]),as.numeric(Devaluation.Argentina.EOP)[i]))[[-1]])-Final.Period[[i]])/
              22.102698 
            
            
            
          }
        }
        
        
        
        RP.TotalSales <- colSums(as.numeric(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),"2018"])%*%
                                   t(cumprod(as.numeric(CPI.Argentina)))*
                                   as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                   rbind(cumprod(as.numeric(Economic.Growth)),
                                         cumprod(as.numeric(Economic.Growth)),
                                         cumprod(c(1.10,1.10,1.10)),
                                         cumprod(c(1.10,1.10,1.10)),
                                         cumprod(c(1.30,1.30,1.30)))/1000000)
        
        
        RP.TotalSales2018 <- as.numeric(DF.Retail$mat[match("Total Sales", DF.Retail$mat$Productos),"2018"])
        RP.TotalSales <- c(RP.TotalSales2018, RP.TotalSales)
        names(RP.TotalSales) <- c("2018","2019","2020","2021")
        
        
        RP.COGS <- colSums(as.numeric(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),"2018"])%*%
                             t(cumprod(as.numeric(CPI.Argentina)))*
                             (1-apply(DF.Retail$mat[match(paste0("Average Margins - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2019:2021)],2,as.numeric))* 
                             as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                             rbind(cumprod(as.numeric(Economic.Growth)),
                                   cumprod(as.numeric(Economic.Growth)),
                                   cumprod(c(1.10,1.10,1.10)),
                                   cumprod(c(1.10,1.10,1.10)),
                                   cumprod(c(1.30,1.30,1.30)))/1000000)
        
        
        RP.COGS2018 <- -as.numeric(DF.Retail$mat[match("COGS", DF.Retail$mat$Productos),"2018"])
        RP.COGS <- c(RP.COGS2018, RP.COGS)
        names(RP.COGS) <- c("2018","2019","2020","2021")
        
        RP.Gross.Margin <- c(DF.Retail$mat[match("Depreciation and Amortization",DF.Retail$mat$Productos),"2018"],Depreciation*0.41360) + RP.TotalSales - RP.COGS
        
        Selling.Expense <- DF.Retail$mat[match("Selling Expense", DF.Retail$mat$Productos),"2018"]*
                           cumprod(as.numeric(CPI.Argentina)*c(1.058922,1.072717,1.058103))
        
        Administrative.Expense <- DF.Retail$mat[match("Administrative Expense", DF.Retail$mat$Productos),"2018"]*
                                  cumprod(as.numeric(CPI.Argentina))
        
        RP.EBITDA <-  (RP.Gross.Margin) + 
                      c(DF.Retail$mat[match("Selling Expense", DF.Retail$mat$Productos),"2018"],Selling.Expense) +
                      c(DF.Retail$mat[match("Administrative Expense", DF.Retail$mat$Productos),"2018"],Administrative.Expense) +
                      -c(DF.Retail$mat[match("Depreciation and Amortization",DF.Retail$mat$Productos),"2018"],Depreciation*0.41360)
        
        ######## BIP
        BIP.Growth <- as.numeric(ifelse(CPI.Argentina > Delt(as.numeric(c(DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),"2018"],Devaluation.Argentina.EOP)))[-1],
                                        CPI.Argentina - Delt(as.numeric(c(DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),"2018"],Devaluation.Argentina.EOP)))[-1],1))
        
        Groth.Packaging <- pmax(Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1],CPI.Argentina)  
        
        BIP.TotalSales <-   colSums((as.numeric(DF.BIP$mat[match(paste0("Average Price - ",Product.BIP[-4]), DF.BIP$mat$Productos),"2018"]/20.592500)%*%
                                       t(cumprod(as.numeric(BIP.Growth))))*
                                      rbind(Devaluation.Argentina.AVG,
                                            Devaluation.Argentina.AVG,
                                            Devaluation.Argentina.AVG)*
                                      apply(DF.BIP$mat[match(Product.BIP[-4], DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric)/1000000) + 
                               DF.BIP$mat[match(Product.BIP[4], DF.BIP$mat$Productos),"2018"]*cumprod(1+as.numeric((Groth.Packaging*
                                 (1+Delt(c(sum(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"]),
                                           colSums(as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                                     rbind(cumprod(as.numeric(Economic.Growth)),
                                                           cumprod(as.numeric(Economic.Growth)),
                                                           cumprod(c(1.10,1.10,1.10)),
                                                           cumprod(c(1.10,1.10,1.10)),
                                                           cumprod(c(1.30,1.30,1.30))))))[-1]))-1))
                                  
        BIP.TotalSales2018 <- DF.BIP$mat[match("Total Sales", DF.BIP$mat$Productos),"2018"]
        BIP.TotalSales <- c(BIP.TotalSales2018, BIP.TotalSales)
        names(BIP.TotalSales) <- c("2018","2019","2020","2021")
        
        BIP.COGS <-   colSums((as.numeric(DF.BIP$mat[match(paste0("Average Price - ",Product.BIP[-4]), DF.BIP$mat$Productos),"2018"]/20.592500)%*%
                                 t(cumprod(as.numeric(BIP.Growth))))*
                                rbind(Devaluation.Argentina.AVG,
                                      Devaluation.Argentina.AVG,
                                      Devaluation.Argentina.AVG)*
                                (as.numeric(DF.BIP$mat[match(Product.BIP[-4], DF.BIP$mat$Productos),"2018"])*
                                   rbind(cumprod(c(1.10145,1.09202,1.08339)),
                                         cumprod(c(1.03217,1.03121,1.03193)),
                                         cumprod(c(.70,0.80,0.80)))/1000000)*
                                (1 - apply(DF.BIP$mat[match(paste0("Average Margins - ",Product.BIP[-4]), DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric)))+ 
                              DF.BIP$mat[match(Product.BIP[4], DF.BIP$mat$Productos),"2018"]*cumprod(1+as.numeric((Groth.Packaging*
                              (1+Delt(c(sum(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"]),
                                        colSums(as.numeric(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),"2018"])*
                                                  rbind(cumprod(as.numeric(Economic.Growth)),
                                                        cumprod(as.numeric(Economic.Growth)),
                                                        cumprod(c(1.10,1.10,1.10)),
                                                        cumprod(c(1.10,1.10,1.10)),
                                                        cumprod(c(1.30,1.30,1.30))))))[-1]))-1))*
                              (1 - apply(DF.BIP$mat[match(paste0("Average Margins - ",Product.BIP[4]), DF.BIP$mat$Productos),paste0("",2019:2021)],2,as.numeric))
                            
        BIP.COGS2018 <- -DF.BIP$mat[match("COGS", DF.BIP$mat$Productos),"2018"]
        BIP.COGS <- c(BIP.COGS2018, BIP.COGS)
        names(BIP.COGS) <- c("2018","2019","2020","2021")
        
        BP.Gross.Margin <- c(DF.BIP$mat[match("Depreciation and Amortization",DF.BIP$mat$Productos),"2018"],Depreciation*0.44875) + BIP.TotalSales - BIP.COGS
        
        Growth.Vol.Wheat <- (1+Delt(c(sum(as.numeric(DF.BIP$mat[match(Product.BIP[-c(3,4)], DF.BIP$mat$Productos),"2018"])),
                                      colSums(as.numeric(DF.BIP$mat[match(Product.BIP[-c(3,4)], DF.BIP$mat$Productos),"2018"])*
                                     rbind(cumprod(1+as.numeric((Economic.Growth-1)*c(3.15359,2.94845,2.61171))),
                                           cumprod(1+as.numeric(Economic.Growth-1))))))[-1])*CPI.Argentina
        
        Selling.Expense <-  colSums(rbind(-1568.14593002*cumprod(as.numeric(Growth.Vol.Wheat)),
                                          -(as.numeric(DF.BIP$mat[match(Product.BIP[3], DF.BIP$mat$Productos),"2018"])*
                                           cumprod(1+as.numeric(Economic.Growth-1)*(-c(9.32546,6.40820,6.26370)))*
                                           (as.numeric(DF.BIP$mat[match(paste0("Average Price - ",Product.BIP[3]), DF.BIP$mat$Productos),"2018"]/20.592500)*
                                           t(cumprod(as.numeric(BIP.Growth))))*Devaluation.Argentina.AVG)/1000000*c(0.29,0.29,0.29)))
        
        Administrative.Expense <- DF.BIP$mat[match("Administrative Expense", DF.BIP$mat$Productos),"2018"]*
                                  cumprod(as.numeric(CPI.Argentina))
        BP.EBITDA <-  (BP.Gross.Margin) + 
                      c(DF.BIP$mat[match("Selling Expense", DF.BIP$mat$Productos),"2018"],Selling.Expense) +
                      c(DF.BIP$mat[match("Administrative Expense", DF.BIP$mat$Productos),"2018"],Administrative.Expense) +
                      -c(DF.BIP$mat[match("Depreciation and Amortization",DF.BIP$mat$Productos),"2018"],Depreciation*0.44875)
        
        #################AGROBusiness
        
        Sustainable.Sourcing <- colSums(as.numeric(DF.AGRO$mat[match(paste0("Third parties - ",Product.Agro), DF.AGRO$mat$Productos),"2018"])%*%
                                          t(cumprod(1+c(0.008,0.008,0.008)))* 
                                          (DF.AGRO$mat[match(paste0("Third partiesAdj - ",Product.Agro), DF.AGRO$mat$Productos),"2018"]/20.59250)%*%
                                          t(as.numeric(Devaluation.Argentina.AVG))/1000000)*
          (1 - apply(DF.AGRO$mat[match("Gross Margin - Sustainable Sourcing", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        TotalCOGS.AgroServices <- 4166.78554012*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))*
          (1 - apply(DF.AGRO$mat[match("Gross Margin - Agro-services", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        TotalCOGS.LogisticandPort <- 1172.445667*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))*
          (1 - apply(DF.AGRO$mat[match("Gross Margin - Port and Logistics", DF.AGRO$mat$Productos),paste0("",2019:2021)],2,as.numeric))
        
        AG.TotalCOGS <- Sustainable.Sourcing + TotalCOGS.AgroServices + TotalCOGS.LogisticandPort
        
        AG.TotalCOGS2018 <- -DF.AGRO$mat[match("COGS", DF.AGRO$mat$Productos),"2018"]
        AG.TotalCOGS <- c(AG.TotalCOGS2018, AG.TotalCOGS)
        names(AG.TotalCOGS) <- c("2018","2019","2020","2021")
        
        
        Sustainable.Sourcing <- colSums(as.numeric(DF.AGRO$mat[match(paste0("Third parties - ",Product.Agro), DF.AGRO$mat$Productos),"2018"])%*%
                                          t(cumprod(1+c(0.008,0.008,0.008)))* 
                                          (DF.AGRO$mat[match(paste0("Third partiesAdj - ",Product.Agro), DF.AGRO$mat$Productos),"2018"]/20.59250)%*%
                                          t(as.numeric(Devaluation.Argentina.AVG))/1000000)
        
        TotalSales.AgroServices <- 4166.78554012*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))
        TotalSales.LogisticandPort <- 1172.445667*cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),paste0("",2018)],Devaluation.Argentina.AVG)))[-1])*(1+c(0.008,0.008,0.008)))
        
        AG.TotalSales <- Sustainable.Sourcing + TotalSales.AgroServices + TotalSales.LogisticandPort
        
        AG.TotalSales2018 <- DF.AGRO$mat[match("Total Sales", DF.AGRO$mat$Productos),"2018"]
        AG.TotalSales <- c(AG.TotalSales2018, AG.TotalSales)
        names(AG.TotalSales) <- c("2018","2019","2020","2021")
        
        Biological.Assets <- c(DF.AGRO$mat[match("Gain Biological asset",DF.AGRO$mat$Productos),"2018"],
                               DF.AGRO$mat[match("Gain Biological asset",DF.AGRO$mat$Productos),"2018"]*
                                 cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),
                                                                      paste0("",2018)],Devaluation.Argentina.AVG)))[-1])))
        
        ASSS.Gross.Margin <- c(DF.AGRO$mat[match("Depreciation and Amortization",DF.AGRO$mat$Productos),"2018"],Depreciation*0.13765) + Biological.Assets + AG.TotalSales - AG.TotalCOGS
        
        Selling.Expense <- c(DF.AGRO$mat[match("Selling Expense",DF.AGRO$mat$Productos),"2018"],
                             DF.AGRO$mat[match("Selling Expense",DF.AGRO$mat$Productos),"2018"]*
                               cumprod((1+Delt(as.numeric(c(DF1$mat[which(DF1$mat$Variables == "ARS/USD (Avg)"),
                                                                    paste0("",2018)],Devaluation.Argentina.AVG)))[-1]))*
                               cumprod(1+c(0.008,0.008,0.008)))
        
        Administrative.Expense <- c(DF.AGRO$mat[match("Administrative Expense",DF.AGRO$mat$Productos),"2018"],
                                    DF.AGRO$mat[match("Administrative Expense",DF.AGRO$mat$Productos),"2018"]*
                                      cumprod(as.numeric(CPI.Argentina)))
        
        Others <- DF.AGRO$mat[match("Other Income, Net",DF.AGRO$mat$Productos),paste0("",2018:2021)]
        
        ASSS.EBITDA <- ASSS.Gross.Margin + Selling.Expense + Administrative.Expense + Others - 
          c(DF.AGRO$mat[match("Depreciation and Amortization",DF.AGRO$mat$Productos),"2018"],Depreciation*0.13765)
        
        RP.EBITDA.MARGIN <- RP.EBITDA/RP.TotalSales
        BIP.EBITDA.MARGIN <- BP.EBITDA/BIP.TotalSales
        ASSS.EBITDA.MARGIN <- ASSS.EBITDA/AG.TotalSales
        
        
        Ebitda.Margin <- data.frame(Segment = c("RP", "BIP","ASSS"),
                                    Ebitda.Margin =    c(as.numeric(RP.EBITDA.MARGIN[input$month]),
                                                         as.numeric(BIP.EBITDA.MARGIN[input$month]),
                                                         as.numeric(ASSS.EBITDA.MARGIN[input$month])))
        colors=c("#0059b3", "#0086b3", "#001f4d")
        plot_ly(Ebitda.Margin, labels = ~Segment, values = ~Ebitda.Margin, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~percent(round(Ebitda.Margin,4)),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE) %>%
          layout(title = paste('EBITDA(%) by Segment -', paste0("",input$month)),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
          layout(plot_bgcolor='transparent') %>%
          layout(paper_bgcolor='transparent')

      })


      output$total_flights <- renderValueBox({
        # The following code runs inside the databas
        Product.Retail <- c("Flour Products", "Oil", "Biscuits, Cookies and Crackers", "Bread Crumbs and Premixes (& Pasta)", "Frozen Products")

        CPI.Argentina[1] <- 1
        Value <-  eventReactive(input$submit1, {
          input$inflation
        })

        if(input$Default == TRUE) {

          CPI.Argentina[input$month] <- (1+DF1$mat[which(DF1$mat$Variables == "CPI Argentina"),input$month])     }
        else {
          CPI.Argentina[input$month] <- (1+Value()) }


        RP.TotalSales <- colSums(apply(DF.Retail$mat[match(paste0("Average Price - ",Product.Retail), DF.Retail$mat$Productos),paste0("",2017:2021)],2,as.numeric)*
                                   apply(DF.Retail$mat[match(Product.Retail, DF.Retail$mat$Productos),paste0("",2017:2021)],2,as.numeric)/1000000)
        
        Sales2021 <- as.numeric(  RP.TotalSales[input$month]  +
                                    DF.BIP$mat[match("Total Sales",DF.BIP$mat$Productos),input$month] +
                                    DF.AGRO$mat[match("Total Sales",DF.AGRO$mat$Productos),input$month])

        Year11 <- paste0("",input$month)
        Year111 <- as.numeric(Year11) - 1
        Sales2021.lag <- as.numeric(DF.IS$mat[match("Total Sales",DF.IS$mat$Productos),paste0("",Year111)])

        Icon <- ifelse(Sales2021 > Sales2021.lag,"arrow-up","arrow-down")
        Col <- ifelse(Sales2021 > Sales2021.lag,"aqua","red")

        valueBox(value = tags$p(prettyNum(Sales2021, big.mark = ","), style = "font-size: 70%;"),
                 subtitle = paste('Last year Sales:', prettyNum(round(Sales2021.lag,0), big.mark = ",")),
                 color = Col,
                 icon = icon(list(name = Icon, width="10px")))
      })

      output$per_day <- renderValueBox({
        # The following code runs inside the databas
        EBITDA <- as.numeric(DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",input$month)])

        Year11 <- paste0("",input$month)
        Year111 <- as.numeric(Year11) - 1
        EBITDA.lag <- as.numeric(DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",Year111)])

        Icon <- ifelse(EBITDA > EBITDA.lag,"arrow-up","arrow-down")
        Col <- ifelse(EBITDA > EBITDA.lag,"aqua","red")

        valueBox(value = tags$p(prettyNum(EBITDA, big.mark = ","), style = "font-size: 70%;"),
                 subtitle = paste('Last year EBITDA:', prettyNum(round(EBITDA.lag,0), big.mark = ",")),
                 color = Col,
                 icon = icon(list(name = Icon, width="10px")))
      })

      output$Ebitdapercentahe <- renderValueBox({
        # The following code runs inside the databas
        EBITDA <- as.numeric(DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",input$month)])
        EBITDA.Margin <- round(EBITDA/as.numeric(DF.IS$mat[match("Total Sales",DF.IS$mat$Productos),paste0("",input$month)]),4)
        # EBITDA.Margin <- DF1$mat[which(DF1$mat == "ARS/USD (EOP)"), input$month]
        Year11 <- input$month
        Year111 <- as.numeric(Year11) -1
        EBITDA.Margin.lag <- round(
          as.numeric(DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",Year111)])/
            as.numeric(DF.IS$mat[match("Total Sales",DF.IS$mat$Productos),paste0("",Year111)]),4)
        Icon <- ifelse(EBITDA.Margin > EBITDA.Margin.lag,"arrow-up","arrow-down")
        Col <- ifelse(EBITDA.Margin > EBITDA.Margin.lag,"aqua","red")
        valueBox(value = tags$p(percent(EBITDA.Margin), style = "font-size: 70%;"),
                 subtitle = paste('Last year EBITDA(%): ', percent(EBITDA.Margin.lag)),
                 color = Col,
                 icon = icon(list(name = Icon, width="10px")))
      })

      output$barsplot <- renderPlotly({
        y <- paste0("", 2017:2021)

        x1 <- DF.Retail$mat[match("Total Sales",DF.Retail$mat$Productos),paste0("",2017:2021)]
        x2 <- DF.BIP$mat[match("Total Sales",DF.BIP$mat$Productos),paste0("",2017:2021)]
        x3 <- DF.AGRO$mat[match("Total Sales",DF.AGRO$mat$Productos),paste0("",2017:2021)]

        data <- data.frame(Date = y,RP = as.numeric(x1), BIP = as.numeric(x2), ASSS = as.numeric(x3))

        Total.Sales <- data.frame(Sales = rowSums(data[,2:4]),
                                  Date = y)


        top_labels <- c('RP', 'BIP', 'ASSS')

        if(input$Segment == "All") {
          plot_ly(Total.Sales, x = ~Sales, y = ~Date, type = 'bar', orientation = 'h', name = 'TotalSales',
                  marker = list(color = 'rgba(30, 45, 23, 0.4)',
                                line = list(color = 'rgb(245, 130, 129)', width = 1))) %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')  %>%
            layout(yaxis = list(title = "Dates"), xaxis = list(title = "Total Sales (All segments)"))

        } else  {

          plot_ly(data, x = ~RP, y = ~Date, type = 'bar', orientation = 'h', name = 'Retail',
                  marker = list(color = 'rgba(38, 24, 74, 0.8)',
                                line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
            add_trace(x = ~BIP, marker = list(color = 'rgba(71, 58, 131, 0.8)'), name = 'BIP') %>%
            add_trace(x = ~ASSS, marker = list(color = 'rgba(122, 120, 168, 0.8)'), name = 'ASSS') %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')  %>%
            layout(yaxis = list(title = "Dates"), xaxis = list(title = "Total Sales (By segment)"))
        }
      })

      observeEvent(input$col, {
        js$pageCol(DF1$mat[which(DF1$mat == "ARS/USD (EOP)"), input$month])
      })

      output$Ebitdatime <- renderPlotly({
        Adjuted.Ebitda <- as.numeric(DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2017:2021)])/
          as.numeric(DF.IS$mat[match("Total Sales",DF.IS$mat$Productos),paste0("",2017:2021)])
        Adjuted.Ebitda <- data.frame(EBITDA = Adjuted.Ebitda,
                                     Dates = paste0("",2017:2021))

        y <- paste0("", 2017:2021)
        x1 <- DF.Retail$mat[match("EBITDA Margin (%)",DF.Retail$mat$Productos),paste0("",2017:2021)]
        x2 <- DF.BIP$mat[match("EBITDA Margin (%)",DF.BIP$mat$Productos),paste0("",2017:2021)]
        x3 <- DF.AGRO$mat[match("EBITDA Margin (%)",DF.AGRO$mat$Productos),paste0("",2017:2021)]
        data1 <- data.frame(Dates = y,  ASSS = as.numeric(x3), BIP = as.numeric(x2), RP = as.numeric(x1))



        if(input$Segment == "All") {
          plot_ly(Adjuted.Ebitda, x = ~percent(round(EBITDA,2)), y = ~Dates, type = 'bar', orientation = 'h', name = 'Total',
                  marker = list(color = 'rgba(30, 45, 23, 0.4)',
                                line = list(color = 'rgb(245, 130, 129)', width = 1))) %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')  %>%
            layout(yaxis = list(title = "Dates"), xaxis = list(title = "EBITDA (All segments)"))

        } else  {
          plot_ly(data1, x = ~percent(round(ASSS,2)), y = ~Dates, type = 'bar', orientation = 'h', name = 'ASSS',
                  marker = list(color = 'rgba(38, 24, 74, 0.8)',
                                line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
            add_trace(x = ~percent(round(BIP,2)), marker = list(color = 'rgba(71, 58, 131, 0.8)'), name = 'BIP') %>%
            add_trace(x = ~percent(round(RP,2)), marker = list(color = 'rgba(122, 120, 168, 0.8)'), name = 'RP') %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')  %>%
            layout(yaxis = list(title = "Dates"), xaxis = list(title = "EBITDA (By segment)"))

        }
      })


      output$WACC <- renderPlotly({

        RF  <- input$RFreeR
        CRP <- input$CRiskP
        ERP <- Value$mat[which(Value$mat$Variables == "Equity Risk Premium"),2]
        Beta <- input$Beta

        Value$mat[which(Value$mat$Variables == "Cost of Capital"),2] <- RF + CRP + ERP*Beta

        Cost_Capital <- RF+CRP+ERP*Beta
        Cost_Debt <- Value$mat[which(Value$mat$Variables == "Cost of Debt"),2]

        Deb.EVRatio <- Value$mat[max(which(Value$mat$Variables == "Debt / EV Ratio")),2]

        Value$mat[max(which(Value$mat$Variables == "WACC")),2] <- Cost_Debt*Deb.EVRatio*(1-0.35) + Cost_Capital*(1-Deb.EVRatio)
        WACC <- Value$mat[max(which(Value$mat$Variables == "WACC")),2]
        WACC_Summary <- data.frame(Rates = (c(RF, CRP, ERP, Cost_Capital, Cost_Debt, WACC)),
                                   Names = c("Risk Free" , "Country Premium", "Equity Premium",
                                             "Cost of Capital", "Cost of Debt", "WACC"))
        Perpetual <- Value$mat[which(Value$mat$Variables == "Perpetual Growth Rate"),2]
        Factor_Descuento <- (1/(1+WACC))^(1:4)
        FCF_Descontado <- sum(Factor_Descuento*
                                Value$mat[max(which(Value$mat$Variables == "FCF")),paste0("",2018:2021)])
        Final_Value <- Value$mat[max(which(Value$mat$Variables == "FCF")),
                                 paste0("",2022)]/(WACC - Perpetual)
        Final_Value <- Final_Value*last(Factor_Descuento)
        Enter_Value <- Final_Value + FCF_Descontado
        EBITDA_USD <- DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2018)]/
          DF1$mat[match("ARS/USD (Avg)",DF1$mat$Variables),paste0("",2018)]


        clarity <- data.frame(ggplot2::diamonds)[21:26,4]

        f <- list(
          family = "Arial Black, monospace",
          size = 20,
          color = "#7f7f7f")

        plot_ly(WACC_Summary, x = ~Names, y = ~Rates, type = 'bar', color = clarity) %>%
          layout(xaxis = list(title = "WACC Components", titlefont = f, categoryarray = ~Names, categoryorder = "array"), showlegend = FALSE,
                 yaxis = list(tickformat = "%"))   %>%
          layout(plot_bgcolor='transparent') %>%
          layout(paper_bgcolor='transparent')

      })

      output$EVEBITDA <- renderValueBox({
        RF  <- input$RFreeR
        CRP <- input$CRiskP
        ERP <- Value$mat[which(Value$mat$Variables == "Equity Risk Premium"),2]
        Beta <- input$Beta

        Value$mat[which(Value$mat$Variables == "Cost of Capital"),2] <- RF + CRP + ERP*Beta

        Cost_Capital <- RF+CRP+ERP*Beta
        Cost_Debt <- Value$mat[which(Value$mat$Variables == "Cost of Debt"),2]

        Deb.EVRatio <- Value$mat[max(which(Value$mat$Variables == "Debt / EV Ratio")),2]

        Value$mat[max(which(Value$mat$Variables == "WACC")),2] <- Cost_Debt*Deb.EVRatio*(1-0.35) + Cost_Capital*(1-Deb.EVRatio)
        WACC <- Value$mat[max(which(Value$mat$Variables == "WACC")),2]
        WACC_Summary <- data.frame(Rates = (c(RF, CRP, ERP, Cost_Capital, Cost_Debt, WACC)),
                                   Names = c("Risk Free" , "Country Premium", "Equity Premium",
                                             "Cost of Capital", "Cost of Debt", "WACC"))
        Perpetual <- Value$mat[which(Value$mat$Variables == "Perpetual Growth Rate"),2]
        Factor_Descuento <- (1/(1+WACC))^(1:4)
        FCF_Descontado <- sum(Factor_Descuento*
                                Value$mat[max(which(Value$mat$Variables == "FCF")),paste0("",2018:2021)])
        Final_Value <- Value$mat[max(which(Value$mat$Variables == "FCF")),
                                 paste0("",2022)]/(WACC - Perpetual)
        Final_Value <- Final_Value*last(Factor_Descuento)
        Enter_Value <- Final_Value + FCF_Descontado
        EBITDA_USD <- DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2018)]/
          DF1$mat[match("ARS/USD (Avg)",DF1$mat$Variables),paste0("",2018)]
        Target <- round(Enter_Value/EBITDA_USD,2)
        valueBox(value = tags$p(paste0(Target, "x"), style = "font-size: 70%;"),
                 subtitle = "EV/EBITDA Multiple",
                 color = "blue",
                 icon = icon(list(name = "university", width="10px")))
      })
      output$DEBEV <- renderValueBox({
        RF  <- input$RFreeR
        CRP <- input$CRiskP
        ERP <- Value$mat[which(Value$mat$Variables == "Equity Risk Premium"),2]
        Beta <- input$Beta

        Value$mat[which(Value$mat$Variables == "Cost of Capital"),2] <- RF + CRP + ERP*Beta

        Cost_Capital <- RF+CRP+ERP*Beta
        Cost_Debt <- Value$mat[which(Value$mat$Variables == "Cost of Debt"),2]

        Deb.EVRatio <- Value$mat[max(which(Value$mat$Variables == "Debt / EV Ratio")),2]

        Value$mat[max(which(Value$mat$Variables == "WACC")),2] <- Cost_Debt*Deb.EVRatio*(1-0.35) + Cost_Capital*(1-Deb.EVRatio)
        WACC <- Value$mat[max(which(Value$mat$Variables == "WACC")),2]
        WACC_Summary <- data.frame(Rates = (c(RF, CRP, ERP, Cost_Capital, Cost_Debt, WACC)),
                                   Names = c("Risk Free" , "Country Premium", "Equity Premium",
                                             "Cost of Capital", "Cost of Debt", "WACC"))
        Perpetual <- Value$mat[which(Value$mat$Variables == "Perpetual Growth Rate"),2]
        Factor_Descuento <- (1/(1+WACC))^(1:4)
        FCF_Descontado <- sum(Factor_Descuento*
                                Value$mat[max(which(Value$mat$Variables == "FCF")),paste0("",2018:2021)])
        Final_Value <- Value$mat[max(which(Value$mat$Variables == "FCF")),
                                 paste0("",2022)]/(WACC - Perpetual)
        Final_Value <- Final_Value*last(Factor_Descuento)
        Enter_Value <- Final_Value + FCF_Descontado
        EBITDA_USD <- DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2018)]/
          DF1$mat[match("ARS/USD (Avg)",DF1$mat$Variables),paste0("",2018)]

        Target <-  EBITDA_USD*Value$mat[which(Value$mat$Variables == "Target Leverage Ratio (Debt to EBITDA)"),
                                        paste0("",2018)]/Enter_Value
        Target <- round(Target,2)
        valueBox(value = tags$p(percent(Target), style = "font-size: 70%;"),
                 subtitle = "Debt / EV Ratio",
                 color = "green",
                 icon = icon(list(name = "percent", width="10px")))
      })

      output$Beta <- renderValueBox({
        RF  <- input$RFreeR
        CRP <- input$CRiskP
        ERP <- Value$mat[which(Value$mat$Variables == "Equity Risk Premium"),2]
        Beta <- input$Beta

        Value$mat[which(Value$mat$Variables == "Cost of Capital"),2] <- RF + CRP + ERP*Beta

        Cost_Capital <- RF+CRP+ERP*Beta
        Cost_Debt <- Value$mat[which(Value$mat$Variables == "Cost of Debt"),2]

        Deb.EVRatio <- Value$mat[max(which(Value$mat$Variables == "Debt / EV Ratio")),2]

        Value$mat[max(which(Value$mat$Variables == "WACC")),2] <- Cost_Debt*Deb.EVRatio*(1-0.35) + Cost_Capital*(1-Deb.EVRatio)
        WACC <- Value$mat[max(which(Value$mat$Variables == "WACC")),2]
        WACC_Summary <- data.frame(Rates = (c(RF, CRP, ERP, Cost_Capital, Cost_Debt, WACC)),
                                   Names = c("Risk Free" , "Country Premium", "Equity Premium",
                                             "Cost of Capital", "Cost of Debt", "WACC"))
        Perpetual <- Value$mat[which(Value$mat$Variables == "Perpetual Growth Rate"),2]
        Factor_Descuento <- (1/(1+WACC))^(1:4)
        FCF_Descontado <- sum(Factor_Descuento*
                                Value$mat[max(which(Value$mat$Variables == "FCF")),paste0("",2018:2021)])
        Final_Value <- Value$mat[max(which(Value$mat$Variables == "FCF")),
                                 paste0("",2022)]/(WACC - Perpetual)
        Final_Value <- Final_Value*last(Factor_Descuento)
        Enter_Value <- Final_Value + FCF_Descontado
        EBITDA_USD <- DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2018)]/
          DF1$mat[match("ARS/USD (Avg)",DF1$mat$Variables),paste0("",2018)]
        Beta <- input$Beta
        valueBox(value = tags$p(round(Beta,2), style = "font-size: 70%;"),
                 subtitle = "Company Beta",
                 color = "blue",
                 icon = icon(list(name = "dollar-sign", width="10px")))
      })



      output$PRICEPER <- renderValueBox({
        RF  <- input$RFreeR
        CRP <- input$CRiskP
        ERP <- Value$mat[which(Value$mat$Variables == "Equity Risk Premium"),2]
        Beta <- input$Beta

        Value$mat[which(Value$mat$Variables == "Cost of Capital"),2] <- RF + CRP + ERP*Beta

        Cost_Capital <- RF+CRP+ERP*Beta
        Cost_Debt <- Value$mat[which(Value$mat$Variables == "Cost of Debt"),2]

        Deb.EVRatio <- Value$mat[max(which(Value$mat$Variables == "Debt / EV Ratio")),2]

        Value$mat[max(which(Value$mat$Variables == "WACC")),2] <- Cost_Debt*Deb.EVRatio*(1-0.35) + Cost_Capital*(1-Deb.EVRatio)
        WACC <- Value$mat[max(which(Value$mat$Variables == "WACC")),2]
        WACC_Summary <- data.frame(Rates = (c(RF, CRP, ERP, Cost_Capital, Cost_Debt, WACC)),
                                   Names = c("Risk Free" , "Country Premium", "Equity Premium",
                                             "Cost of Capital", "Cost of Debt", "WACC"))
        Perpetual <- Value$mat[which(Value$mat$Variables == "Perpetual Growth Rate"),2]
        Factor_Descuento <- (1/(1+WACC))^(1:4)
        FCF_Descontado <- sum(Factor_Descuento*
                                Value$mat[max(which(Value$mat$Variables == "FCF")),paste0("",2018:2021)])
        Final_Value <- Value$mat[max(which(Value$mat$Variables == "FCF")),
                                 paste0("",2022)]/(WACC - Perpetual)
        Final_Value <- Final_Value*last(Factor_Descuento)
        Enter_Value <- Final_Value + FCF_Descontado
        EBITDA_USD <- DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2018)]/
          DF1$mat[match("ARS/USD (Avg)",DF1$mat$Variables),paste0("",2018)]
        Debt <- (-DF.BS$mat[match("Cash and cash equivalents",DF.BS$mat$Productos),paste0("2Q",2018)] +
                   DF.BS$mat[match("Borrowings",DF.BS$mat$Productos),paste0("2Q",2018)] +
                   DF.BS$mat[match("Borrowings1",DF.BS$mat$Productos),paste0("2Q",2018)])/
        DF1$mat[match("ARS/USD (EOP)",DF1$mat$Variables),paste0("2Q",2018)]

        Target <- (Enter_Value - Debt)/
          Value$mat[which(Value$mat$Variables == "New Shares"),paste0("",2018)]
        Target <- round(Target*(1-0.20),2)
        valueBox(value = tags$p(paste(Target, "USD"), style = "font-size: 70%;"),
                 subtitle = "Price per Shares",
                 color = "blue",
                 icon = icon(list(name = "dollar", width="10px")))
      })
      output$WACC1 <- renderValueBox({
        RF  <- input$RFreeR
        CRP <- input$CRiskP
        ERP <- Value$mat[which(Value$mat$Variables == "Equity Risk Premium"),2]
        Beta <- input$Beta

        Value$mat[which(Value$mat$Variables == "Cost of Capital"),2] <- RF + CRP + ERP*Beta

        Cost_Capital <- RF+CRP+ERP*Beta
        Cost_Debt <- Value$mat[which(Value$mat$Variables == "Cost of Debt"),2]

        Deb.EVRatio <- Value$mat[max(which(Value$mat$Variables == "Debt / EV Ratio")),2]

        Value$mat[max(which(Value$mat$Variables == "WACC")),2] <- Cost_Debt*Deb.EVRatio*(1-0.35) + Cost_Capital*(1-Deb.EVRatio)
        WACC <- Value$mat[max(which(Value$mat$Variables == "WACC")),2]
        WACC_Summary <- data.frame(Rates = (c(RF, CRP, ERP, Cost_Capital, Cost_Debt, WACC)),
                                   Names = c("Risk Free" , "Country Premium", "Equity Premium",
                                             "Cost of Capital", "Cost of Debt", "WACC"))
        Perpetual <- Value$mat[which(Value$mat$Variables == "Perpetual Growth Rate"),2]
        Factor_Descuento <- (1/(1+WACC))^(1:4)
        FCF_Descontado <- sum(Factor_Descuento*
                                Value$mat[max(which(Value$mat$Variables == "FCF")),paste0("",2018:2021)])
        Final_Value <- Value$mat[max(which(Value$mat$Variables == "FCF")),
                                 paste0("",2022)]/(WACC - Perpetual)
        Final_Value <- Final_Value*last(Factor_Descuento)
        Enter_Value <- Final_Value + FCF_Descontado
        EBITDA_USD <- DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2018)]/
          DF1$mat[match("ARS/USD (Avg)",DF1$mat$Variables),paste0("",2018)]
        WACC <-  Value$mat[which(Value$mat$Variables == "WACC"), paste0("",2018)]
        WACC <- WACC[-1]
        valueBox(value = tags$p(percent(WACC), style = "font-size: 70%;"),
                 subtitle = "Weighted Average Cost of Capital",
                 color = "blue",
                 icon = icon(list(name = "percent", width="10px")))
      })

      output$EV1 <- renderValueBox({
        RF  <- input$RFreeR
        CRP <- input$CRiskP
        ERP <- Value$mat[which(Value$mat$Variables == "Equity Risk Premium"),2]
        Beta <- input$Beta

        Value$mat[which(Value$mat$Variables == "Cost of Capital"),2] <- RF + CRP + ERP*Beta

        Cost_Capital <- RF+CRP+ERP*Beta
        Cost_Debt <- Value$mat[which(Value$mat$Variables == "Cost of Debt"),2]

        Deb.EVRatio <- Value$mat[max(which(Value$mat$Variables == "Debt / EV Ratio")),2]

        Value$mat[max(which(Value$mat$Variables == "WACC")),2] <- Cost_Debt*Deb.EVRatio*(1-0.35) + Cost_Capital*(1-Deb.EVRatio)
        WACC <- Value$mat[max(which(Value$mat$Variables == "WACC")),2]
        WACC_Summary <- data.frame(Rates = (c(RF, CRP, ERP, Cost_Capital, Cost_Debt, WACC)),
                                   Names = c("Risk Free" , "Country Premium", "Equity Premium",
                                             "Cost of Capital", "Cost of Debt", "WACC"))
        Perpetual <- Value$mat[which(Value$mat$Variables == "Perpetual Growth Rate"),2]
        Factor_Descuento <- (1/(1+WACC))^(1:4)
        FCF_Descontado <- sum(Factor_Descuento*
                                Value$mat[max(which(Value$mat$Variables == "FCF")),paste0("",2018:2021)])
        Final_Value <- Value$mat[max(which(Value$mat$Variables == "FCF")),
                                 paste0("",2022)]/(WACC - Perpetual)
        Final_Value <- Final_Value*last(Factor_Descuento)
        Enter_Value <- Final_Value + FCF_Descontado
        EBITDA_USD <- DF.IS$mat[match("Adjusted EBITDA",DF.IS$mat$Productos),paste0("",2018)]/
          DF1$mat[match("ARS/USD (Avg)",DF1$mat$Variables),paste0("",2018)]
        EV <- Enter_Value
        valueBox(value = tags$p(round(EV,2), style = "font-size: 70%;"),
                 subtitle = "Enterprise Value",
                 color = "green",
                 icon = icon(list(name = "dollar-sign", width="10px")))
      })
      observeEvent(input$saveBtn, {
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Thank you for clicking')

        DF.Retail <- as.data.frame(DF.Retail$mat)
        DF.AGRO <- as.data.frame(DF.AGRO$mat)
        DF.BIP <- as.data.frame(DF.BIP$mat)
        DF.BS <- as.data.frame(DF.BS$mat)
        DF.CF  <- as.data.frame(DF.CF$mat)
        DF.IS  <- as.data.frame(DF.IS$mat)
        DF1 <- as.data.frame(DF1$mat)
        DF2 <- as.data.frame(DF2$mat)

        write.xlsx(DF.Retail, paste0(input$caption,".xlsx"), sheetName="Retail Products",row.names = FALSE)
        write.xlsx(DF.AGRO, paste0(input$caption,".xlsx"), sheetName="Agro Services", append=TRUE, row.names = FALSE)
        write.xlsx(DF.BIP, paste0(input$caption,".xlsx"), sheetName="Branded Industrial Products", append=TRUE, row.names = FALSE)
        write.xlsx(DF.BS, paste0(input$caption,".xlsx"), sheetName="Balance Sheet", append=TRUE,row.names = FALSE)
        write.xlsx(DF.CF, paste0(input$caption,".xlsx"), sheetName="CashFlows", append=TRUE, row.names = FALSE)
        write.xlsx(DF.IS, paste0(input$caption,".xlsx"), sheetName="Income Statement", append=TRUE, row.names = FALSE)
        write.xlsx(DF1, paste0(input$caption,".xlsx"), sheetName="Business Inputs", append=TRUE, row.names = FALSE)
        write.xlsx(DF2, paste0(input$caption,".xlsx"), sheetName="Production Inputs", append=TRUE, row.names = FALSE)
      })
    }

    if (USER$Logged == FALSE) {
      box(title = "Login",textInput("userName", "Username"),
          passwordInput("passwd", "Password"),
          br(),
          actionButton("Login", "Log in"))
    }
  })
}
shinyApp(ui, server)
