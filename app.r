# Dean Attali
# November 21 2014

# This is the ui portion of a shiny app shows cancer data in the United States

library(shiny)
library(shinyjs)
library(shiny)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyjs)

source("helpers.R")  # have the helper functions avaiable


# Get the raw data
cDatRaw <- getData()

# Get the list of colours to use for plotting
plotCols <- getPlotCols()


share <- list(
  title = "Cancer data in the United States",
  url = "http://daattali.com/shiny/cancer-data/",
  image = "http://daattali.com/shiny/img/cancer.png",
  description = "Explore trends in cancer incidence over the years and compare different cancer types.",
  twitter_user = "daattali"
)

ui <- function(request) {
      fluidPage(
        useShinyjs(),
        title = "Cancer data in the United States",
        
        # add custom JS and CSS
        singleton(
          tags$head(
            includeScript(file.path('www', 'message-handler.js')),
            includeScript(file.path('www', 'helper-script.js')),
            includeCSS(file.path('www', 'style.css')),
            tags$link(rel = "shortcut icon", type="image/x-icon", href="http://daattali.com/shiny/img/favicon.ico"),
            # Facebook OpenGraph tags
            tags$meta(property = "og:title", content = share$title),
            tags$meta(property = "og:type", content = "website"),
            tags$meta(property = "og:url", content = share$url),
            tags$meta(property = "og:image", content = share$image),
            tags$meta(property = "og:description", content = share$description),
          
            # Twitter summary cards
            tags$meta(name = "twitter:card", content = "summary"),
            tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
            tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
            tags$meta(name = "twitter:title", content = share$title),
            tags$meta(name = "twitter:description", content = share$description),
            tags$meta(name = "twitter:image", content = share$image)
          )
        ),
        tags$a(
          href="https://github.com/daattali/shiny-server/tree/master/cancer-data",
          tags$img(style="position: absolute; top: 0; right: 0; border: 0;",
                   src="github-green-right.png",
                   alt="Fork me on GitHub")
        ),
      	
      	# enclose the header in its own section to style it nicer
      	div(id = "headerSection",
      		h1("Cancer data in the United States"),
      	
      		# author info
      		span(
                        style = "font-size: 1.2em",
      			span("Created by "),
      			a("Dean Attali", href = "http://deanattali.com"),
      			HTML("&bull;"),
      			span("Code"),
      			a("on GitHub", href = "https://github.com/daattali/shiny-server/tree/master/cancer-data"),
                              HTML("&bull;"),
                              a("More apps", href = "http://daattali.com/shiny/"), "by Dean",
      			br(),
      			
      			span("November 21, 2014")
      		)
      	),
      	
      	# show a loading message initially
      	div(
      		id = "loadingContent",
      		h2("Loading...")
      	),	
      	
      	# all content goes here, and is hidden initially until the page fully loads
      	hidden(div(id = "allContent",
      		# sidebar - filters for the data
      		sidebarLayout(
      			sidebarPanel(
      				h3("Filter data", style = "margin-top: 0;"),
      
      				# show all the cancers or just specific types?
      				selectInput(
      					"subsetType", "",
      					c("Show all cancer types" = "all",
      						"Select specific types" = "specific"),
      					selected = "all"),
      				
      				# which cancer types to show
      				conditionalPanel(
      					"input.subsetType == 'specific'",
      					uiOutput("cancerTypeUi")
      				), br(),
      				
      				# whether to combine all data in a given year or not
      				checkboxInput("showGrouped",
      											strong("Group all data in each year"),
      											FALSE), br(),
      				
      				# what years to show
      				# Note: yearText should use "inline = TRUE" in newer shiny versions,
      				# but since the stats server has an old version I'm doing this in css
      				strong(span("Years:")),
      				textOutput("yearText"), br(),  
      				uiOutput("yearUi"), br(),
      
      				# what variables to show
      				uiOutput("variablesUi"),
      
      				# button to update the data
      				shiny::hr(),
      				actionButton("updateBtn", "Update Data"),
      				
      				# footer - where the data was obtained
      				br(), br(),
      				p("Data was obtained from ",
      					a("the United States CDC",
      						href = "http://wonder.cdc.gov/cancer.html",
      						target = "_blank")),
      				a(img(src = "us-cdc.png", alt = "US CDC"),
      					href = "http://wonder.cdc.gov/cancer.html",
      					target = "_blank"),
                                      br(), br(), bookmarkButton()
      			),
      			
      			# main panel has two tabs - one to show the data, one to plot it
      			mainPanel(wellPanel(
      				tabsetPanel(
      					id = "resultsTab", type = "tabs",
      					
      					# tab showing the data in table format
      					tabPanel(
      						title = "Show data", id = "tableTab",
      						
      						br(),
      						downloadButton("downloadData", "Download table"),
      						br(), br(),
      						
      						span("Table format:"),
      						radioButtons(inputId = "tableViewForm",
      												 label = "",
      												 choices = c("Wide" = "wide", "Long" = "long"),
      												 inline = TRUE),
      						br(),
      						
      						tableOutput("dataTable")
      					),
      					
      					# tab showing the data as plots
      					tabPanel(
      						title = "Plot data", id = "plotTab",
      						br(),
      						downloadButton("downloadPlot", "Save figure"),
      						br(), br(),
      						plotOutput("dataPlot")
      					)
      				)
      			))
      		)
      	))
      )
}

server <- shinyServer(function(input, output, session) {
  # =========== BUILDING THE INPUTS ===========
  
  # Create select box input for choosing cancer types
  output$cancerTypeUi <- renderUI({
    selectizeInput("cancerType", "",
                   levels(cDatRaw$cancerType),
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Select cancer types"))
  })	
  
  # Create select box input to choose variables to show
  output$variablesUi <- renderUI({
    selectizeInput("variablesSelect", "Variables to show:",
                   unique(as.character(cDatRaw$stat)),
                   selected = unique(cDatRaw$stat), multiple = TRUE,
                   options = list(placeholder = "Select variables to show"))
  })	
  
  # Show the years selected (because of the bugs in the slider mentioned below)
  output$yearText <- renderText({
    if (is.null(input$years)) {
      return(formatYearsText(range(cDatRaw$year)))
    }
    
    formatYearsText(input$years)
  })	
  
  # Create slider for selecting year range
  # NOTE: there are some minor bugs with sliderInput rendered in renderUI
  # https://github.com/rstudio/shiny/issues/587
  output$yearUi <- renderUI({
    sliderInput("years", 
                label = "",
                min = min(cDatRaw$year), max = max(cDatRaw$year),
                value = range(cDatRaw$year),
                step = 1)
  })
  
  
  # ============== MANIPULATE THE DATA ================
  
  # The dataset to show/plot, which is the raw data after filtering based on
  # the user inputs
  cDat <- reactive({
    # Add dependency on the update button (only update when button is clicked)
    input$updateBtn	
    
    # If the app isn't fully loaded yet, just return the raw data 
    if (!dataValues$appLoaded) {
      return(cDatRaw)
    }
    
    data <- cDatRaw
    
    # Add all the filters to the data based on the user inputs
    # wrap in an isolate() so that the data won't update every time an input
    # is changed
    isolate({
      
      # Filter years
      data %<>%
        filter(year >= input$years[1] & year <= input$years[2])
      
      # Filter what variables to show
      if (!is.null(input$variablesSelect)) {
        data %<>%
          filter(stat %in% input$variablesSelect)
      }
      
      # Filter cancer types
      if (input$subsetType == "specific" & !is.null(input$cancerType)) {
        data %<>%
          filter(cancerType %in% input$cancerType)
      }
      
      # See if the user wants to show data per cancer type or all combined
      if (input$showGrouped) {
        data %<>%
          group_by(year, stat) %>%
          summarise(value =
                      ifelse(stat[1] != "mortalityRate",
                             sum(value),
                             mean(value))) %>%
          ungroup %>%
          data.frame
      }
    })
    
    data
  })
  
  # The data to show in a table, which is essentially the same data as above
  # with all the filters, but formatted differently:
  # - Format the numbers to look better in a table
  # - Change the data to wide/long format (the filtered data above is long)
  cDatTable <- reactive({
    data <- cDat()
    
    # In numeric columns show 2 digits past the decimal and don't show
    # decimal if the number is a whole integer
    data %<>%
      mutate(value = formatC(data$value, format = "fg", digits = 2))		
    
    # Change the data to wide format if the user wants it
    if (input$tableViewForm == "wide") {
      data %<>%
        spread(stat, value)
    }
    
    data
  })
  
  
  # ============= TAB TO SHOW DATA IN TABLE ===========
  
  # Show the data in a table
  output$dataTable <- renderTable(
    {
      cDatTable()
    },
    include.rownames = FALSE
  )
  
  # Allow user to download the data, simply save as csv
  output$downloadData <- downloadHandler(
    filename = function() { 
      "cancerData.csv"
    },
    
    content = function(file) {
      write.table(x = cDatTable(),
                  file = file,
                  quote = FALSE, sep = ",", row.names = FALSE)
    }
  )	
  
  
  # ============= TAB TO PLOT DATA ===========
  
  # Function to build the plot object
  buildPlot <- reactive({
    
    # Basic ggplot object
    p <-
      ggplot(cDat()) +
      aes(x = as.factor(year), y = value)
    
    # If showing individual cancer types, group each type together, otherwise
    # just connect all the dots as one group
    isolate(
      if (input$showGrouped) {
        p <- p + aes(group = 1)
      } else {
        p <- p + aes(group = cancerType, col = cancerType)
      }
    )
    
    # Facet per variable, add points and lines, and make the graph pretty
    p <- p +
      facet_wrap(~stat, scales = "free_y", ncol = 2) +
      geom_point() +
      geom_line(show.legend = FALSE) +
      theme_bw(16) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_color_manual(values = plotCols) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(title = "",
                                  ncol = 4,
                                  override.aes = list(size = 4))) +
      xlab("Year") + ylab("") +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    p
  })	
  
  # Show the plot, use the width/height that javascript calculated
  output$dataPlot <-
    renderPlot(
      {
        buildPlot()
      },
      height = function(){ input$plotDim },
      width = function(){ input$plotDim },
      units = "px",
      res = 100
    )
  
  # Allow user to download the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "cancerDataPlot.pdf"
    },
    
    content = function(file) {
      pdf(file = file,
          width = 12,
          height = 12)
      print(buildPlot())
      dev.off()
    }
  )		
  
  
  # ========== LOADING THE APP ==========
  
  # We need to have a quasi-variable flag to indicate when the app is loaded
  dataValues <- reactiveValues(
    appLoaded = FALSE
  )
  
  # Wait for the years input to be rendered as a proxy to determine when the app
  # is loaded. Once loaded, call the javascript funtion to fix the plot area
  # (see www/helper-script.js for more information)
  observe({
    if (dataValues$appLoaded) {
      return(NULL)
    }
    if(!is.null(input$years)) {
      dataValues$appLoaded <- TRUE
      
      session$sendCustomMessage(type = "equalizePlotHeight",
                                message = list(target = "dataPlot",
                                               by = "resultsTab"))
    }
  })
  
  # Show form content and hide loading message
  hide("loadingContent")
  show("allContent")
})


shinyApp(ui = ui, server = server)

