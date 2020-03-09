#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(pacman)
pacman::p_load(tidyverse,shinyhttr,stringr,R.utils, shiny, shinyFiles, shinyWidgets, shinydashboard, shinyBS, leaflet, leaflet.extras)

####### Functions #######

withBusyIndicatorCSS <- "

.btn-loading-container {

margin-left: 10px;

font-size: 1.2em;

}

.btn-done-indicator {

color: green;

}

.btn-err {

margin-top: 10px;

color: red;

}

"

withBusyIndicatorUI <- function(button) {
  
  id <- button[['attribs']][['id']]
  
  div(
    
    shinyjs::useShinyjs(),
    
    singleton(tags$head(
      
      tags$style(withBusyIndicatorCSS)
      
    )),
    
    `data-for-btn` = id,
    
    button,
    
    span(
      
      class = "btn-loading-container",
      
      shinyjs::hidden(
        
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        
        icon("check", class = "btn-done-indicator")
        
      )
      
    ),
    
    shinyjs::hidden(
      
      div(class = "btn-err",
          
          div(icon("exclamation-circle"),
              
              tags$b("Error: "),
              
              span(class = "btn-err-msg")
              
          )
          
      )
      
    )
    
  )
  
}

# Define UI for application that draws a histogram

header <- dashboardHeader(
  title =   "CPT",
  titleWidth = 300
)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  fluidRow(
    shinyjs::useShinyjs(),
    tags$head(HTML('
<style>
                   .jhr{
                   display: inline;
                   vertical-align: middle;
                   padding-left: 10px;
                   }

                        .card {
                             box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                             padding: 8px;
                             border: 1px solid #ccc;
                             border-radius: 5px; / 5px rounded corners /
                             max-width: 800px;
                             margin: 0 auto;
                             background-color:white;
                             
                        }

                   </style>

                   ')),
        column(width = 4,
           box(width = NULL, solidHeader = TRUE,status = "warning", title = "Crear directorio raiz",
               textInput("text", label = h4("Nombre Carpeta trabajo:"), value = "",width = 300),
               tags$hr(),
               textInput("text1", label = h4("Seleccionar carpeta"), value = "NULL"),
               shinyDirButton("xdir", "seleccionar ubicación","Buscar" ),
               tags$hr(),
               
               splitLayout(cellWidths = c("30%","40%"),
               
               bsButton("action", label = "Crear directorios", style = "primary") ,
               bsTooltip(id = "action", title = "Crea directorio raiz en la ruta seleccionada", placement = "right", trigger = "hover"),
              
               htmlOutput("sliderInputUI"),
               htmlOutput("bsTooltipUI")
               
               #bsButton("run", label = "Generar predicciones", style = "primary") ,
               #bsTooltip(id = "action", title = "Realiza una corrida de CPT", placement = "right", trigger = "hover")
                
               
               )
               
            ),
           box(width = NULL, title = "Descarga predictres TSM" ,solidHeader = TRUE,status = "warning", collapsible = TRUE,
               
               radioButtons(inputId="Check1", label=h4 ("Cantidad de areas predictoras"),
                            choices=c("1 area predictoras" = 1,"2 areas predictoras" = 2),
                            selected = 1),
               
               splitLayout(cellWidths = c("17%","17%", "17%" , "17%" , "32%"),
                          
                           numericInput(inputId = "lon1_1", label = "x1", value = 0),
                           numericInput(inputId = "lon2_1", label = "x2", value = 360),
                           numericInput(inputId = "lat1_1", label = "y1", value = -45),
                           numericInput(inputId = "lat2_1", label = "y2", value = 45),
                           materialSwitch(inputId = "usemap1",label = "Usar mapa",value = FALSE,status = "primary")
               ),
               conditionalPanel("input.Check1 == '2'",
                                materialSwitch(inputId = "usemap2",label = "Usar herramienta de dibujo",value = FALSE,status = "primary") ,
                                splitLayout(cellWidths = c("17%","17%", "17%" , "17%" , "32%"),
                                  numericInput(inputId = "lon1_2", label = "x1", value = NULL),
                                  numericInput(inputId = "lon2_2", label = "x2", value = NULL),
                                  numericInput(inputId = "lat1_2", label = "y1", value = NULL),
                                  numericInput(inputId = "lat2_2", label = "y2", value = NULL)
                                  
                                )
                                
               ), 
               splitLayout(
                 numericInput("syear","Primer año de descarga",value = 1982,min = 1982, max = as.numeric(format(Sys.Date(), "%Y")),width = 80),
                 numericInput("lyear","Ultimo año de descarga",value = as.numeric(format(Sys.Date(), "%Y")),min = 1982, max = as.numeric(format(Sys.Date(), "%Y")),width = 80)
               ),
               fluidRow(
                 column(width = 4,
                        pickerInput("start", label = h4("Primer mes de la temporada"), 
                                    choices = 1:12, 
                                    selected = "", inline = FALSE)
                        ),
                 column(width = 4,
                        pickerInput("length", label = h4("Tamaño de la temporada"), 
                                    choices = 1:3, 
                                    selected = "",   inline = FALSE)
                        ),
                 column(width = 4,
                        pickerInput("ic", label = h4("Condiciones iniciales de la temporada"), 
                                    choices = 1:12, 
                                    selected = "",   inline = FALSE)
                 )
                 
                 
               )
              ,withBusyIndicatorUI( 
              bsButton("download", label = "Descargar TSM", style = "primary")
              ),
              bsTooltip(id = "download", title = "Descarga datos de TSM del modelo CFSv2", placement = "rigth", trigger = "hover")
           ),
           box(width = NULL, solidHeader = TRUE,title = "Cargar estaciones climatologicas",status = "warning", collapsible = TRUE,
               fileInput("upload", "Upload", multiple = FALSE),
               actionButton("copi", label = "Copiar archivo")
           )
      ),
    column(width = 8,
           navbarPage(title = icon("far fa-cogs"),
                      id = "nvpg1",
                      tabPanel(title = "Introducción",
                               tags$div(id = "div1", class = "card",
                                        h3("Introducción"),
                                        tags$p("Esta es una aplicacion para procesar ..."),
                                        tags$p("para mas informacion acceder al siguiente link"),
                                        textOutput("status"),
                                        tags$a(href ="https://www.youtube.com/embed/T1-k7VYwsHg", "link video"),
                                        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/T1-k7VYwsHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                      
                                        )
                                        
                      ),
                               tabPanel(title = "Selector de área",
                                        tags$div(id = "map", class = "card",
                                        #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                           
                                    
                                        leafletOutput("map1",height = "900",width="1026")
                                                 )
                                        ),
                      tabPanel(title = "Resultados")
           )
    )
           
           
    )
    # Shiny versions prior to 0.11 should use class = "modal" instead
    
    
    )
  


dashboardPage(
  header,
  sidebar,
  body
)


