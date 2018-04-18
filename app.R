library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(leaflet)
library(plyr)
library(dplyr)


datos <- readRDS(file.path(file.path("data"), data.rds))


#datos <- data.frame(Provincia = c(rep("Buenos Aires",100),rep("Cordoba",100),rep("Corrientes",100)),
 #                   Tipo_Alojamiento = rep("Departamentos", 300),
  #                  Direcc = paste0("Direccion",1:300),
   #                 Longitud = -runif(300,57,60), 
    #                Latitud  = -runif(300,37,39),
     #               Proveedor = c(rep("ZonProp",100),rep("Airbnb",100),rep("Booking",100)))


vars <- c("Todas", sort(c("Jujuy","CABA","Entre Rios","Mendoza","Rio Negro","Salta",
                          "San Juan","San Luis","Santiago del Estero","Tierra del Fuego",
                          "Cordoba","Misiones","Santa Cruz","Buenos Aires",
                          "Catamarca","Chaco","Chubut","Corrientes","La Pampa","La Rioja",
                          "Neuquen","Santa Fe","Tucuman","Formosa")))

vars1 <- c("Todos","Booking","TripAdvisor","Expedia", "ZonaProp")


datos$Latitud <- jitter(datos$Latitud)
datos$Longitud <- jitter(datos$Longitud)
datos$Proveedor <- as.character(datos$Proveedor)

counts <- ddply(datos,.(datos$Proveedor,datos$Provincia),nrow)
counts <- na.omit(counts)
names(counts) <- c("Proveedor","Provincia","Cantidad")

cleantable <- datos %>%
                    select(
                          Provincia = Provincia,
                          Lat = Latitud,
                          Long = Longitud,
                          Proveedor = Proveedor )

ui <- navbarPage("Alojamientos Argentina", id="nav",

  tabPanel("Mapa Interactivo",
    div(class="outer",

      tags$head(
        # Include our custom CSS
       includeScript(file.path('www', 'gomap.js')),
    	        includeScript(file.path('www', 'styles.css'))
      
      ),
      
      leafletOutput("map", width="100%", height="100%"),
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("Filtro"),
                    
                    selectInput("Proveedores", "Proveedores", vars1),
                    selectInput("Provincia", "Provincia", vars)
                    
      )
      
    )
  ),
  tabPanel("Datos Generales", dataTableOutput("summary1")),
  tabPanel("Alojamientos por Proveedor / Provincia", dataTableOutput("summary2"))
)
server <-  function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = first(datos$Longitud), lat = first(datos$Latitud), zoom = 1)
  })
  
  
  
  
  observe({
    if(input$Proveedores == "Todos") {
      data = datos
    } else {
      data = datos[datos$Proveedor == input$Proveedores,]
    } 
    if(input$Provincia == "Todas") {
      data = data
    } else {
      data = data[data$Provincia == input$Provincia,]
    }
    if(sum(lengths(data)) != 0){
      POP =  paste("<strong>","Provincia:","</strong>",data$Provincia,"<br>",
                   "<strong>","Tipo de alojamiento:","</strong>",data$Tipo_Alojamiento,"<br>",
                   "<strong>","Proveedor:","</strong>",data$Proveedor,"<br>",
                   "<strong>","Website:","</strong>",paste0("<a href='",data$Website,"'>",data$Website,"</a>"))  
      Prov = data$Provincia} else {
        POP = NULL       
        Prov = NULL }
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addTiles() %>% 
      addCircles(~Longitud, ~Latitud,
                 stroke=FALSE, fillOpacity=0.5, fillColor="blue",fill = "polygons",radius = 5000,
                 popup = POP ,
                 label = Prov)# %>%
  })
  
  output$summary1 <- renderDataTable({
    if(input$Proveedores == "Todos") {
      data = datos
    } else {
      data = datos[datos$Proveedor == input$Proveedores,]
    }
    
    if(input$Provincia == "Todas") {
      data = data
    } else {
      data = data[data$Provincia == input$Provincia,]
    }
    data  })
  
  
  output$summary2 <- renderDataTable({
    counts  })
}

shinyApp(ui = ui, server = server)

