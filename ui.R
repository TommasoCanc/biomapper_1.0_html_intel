#################################
# Shiny App Biomapper v 0.1 -UI #
#################################

# Load libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(leaflet)
library(sf)
library(ggplot2)
library(ggnewscale) # Plot two different scale fill
library(RColorBrewer)
library(gridExtra)
library(grid)
library(plotly)
#library(bs4Dash)
library(shinyjs)
library(png) # Load img


### Colors ----

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

shinyUI(
  navbarPage(
    title = "biomapper 1.0",
    theme = "style/style.css",
    #footer = includeHTML("footer.html"),
    fluid = TRUE,
    collapsible = TRUE,
    
    
    # tab panel 1 - Home ----------------------------------------
    tabPanel(
      "Home",
      includeHTML("home.html"),
      tags$script(src = "plugins/scripts.js"),
      tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
        tags$link(rel = "icon",
                  type = "image/png",
                  href = "images/jon.jpg") # Cambiare questa foto
      ),
      tags$footer(includeHTML("footer.html"))
    ),
    
    
    # tab panel 2 - Neighborhood Browser ------------------------
    tabPanel(
      title = "Map Panel",
      column(
        width = 6,
        HTML(
          "<h2>Insert the taxa richness for each <b>Terrestial</b> geographical units</h2>
          <br>
          <b>Note</b>: F.V.G. = Friuli venezia Giulia; T.A.A. = Trentino Alto Adige, Em-Ro: Emilia Romagna
          <br>
          <br>"
        ),
        
        fluidRow(
          column(2, numericInput("abruzzo", "Abruzzo", 0)),
          column(2, numericInput("basilicata", "Basilicata", 0)),
          column(2, numericInput("calabria", "Calabria", 0)),
          column(2, numericInput("campania", "Campania", 0)),
          column(2, numericInput("emilia", "Em-Ro", 0)),
          column(2, numericInput("friuli", "F.V.G.", 0))
        ),
        fluidRow(
          column(2, numericInput("lazio", "Lazio", 0)),
          column(2, numericInput("liguria", "Liguria", 0)),
          column(2, numericInput("lombardia", "Lombardia", 0)),
          column(2, numericInput("marche", "Marche", 0)),
          column(2, numericInput("molise", "Molise", 0)),
          column(2, numericInput("piemonte", "Piemonte", 0))
        ),
        fluidRow(
          column(2, numericInput("puglia", "Puglia", 0)),
          column(2, numericInput("sardegna", "Sardegna", 0)),
          column(2, numericInput("sicilia", "Sicilia", 0)),
          column(2, numericInput("toscana", "Toscana", 0)),
          column(2, numericInput("trentino", "T.A.A.", 0)),
          column(2, numericInput("umbria", "Umbria", 0))
        ),
        fluidRow(
          column(2, numericInput("aosta", "Valle Aosta", 0)),
          column(2, numericInput("veneto", "Veneto", 0)),
          column(2, numericInput("vaticano", "C. Vaticano", 0)),
          column(2, numericInput("smarino", "San Marino", 0))
        ),
        
        hr(),
        
        fluidRow(
          HTML("<h2>Insert the taxa richness for each <b>Marine</b> geographical units</h2> <br>"),
          column(2, numericInput("m1", "Sector 1", 0)),
          column(2, numericInput("m2", "Sector 2", 0)),
          column(2, numericInput("m3", "Sector 3", 0)),
          column(2, numericInput("m4", "Sector 4", 0)),
          column(2, numericInput("m5", "Sector 5", 0)),
          column(2, numericInput("m6", "Sector 6", 0)),
          column(2, numericInput("m7", "Sector 7", 0)),
          column(2, numericInput("m8", "Sector 8", 0)),
          column(2, numericInput("m9", "Sector 9", 0))
        ),
        
        # hr(),
        # 
        # fluidRow(
        #   HTML(
        #     "<h2>Insert the taxa richness for each <b>Macro</b> geographical units</h2> <br>"
        #   ),
        #   column(2, numericInput("N", "North", 0)),
        #   column(2, numericInput("S", "South", 0)),
        #   column(2, numericInput("R19", "Sicilia", 0)),
        #   column(2, numericInput("R20", "Sardegna", 0)),
        #   column(2, numericInput("R21", "C. Vaticano", 0)),
        #   column(2, numericInput("R22", "San Marino", 0))
        # ), 
        
        hr(),
        
        fluidRow(
          HTML(
            "<h2>Insert the number of bins</h2>
            <br>
            <b>Note</b>:
            T. n. Bins = Terrestral number of bins; M. n. Bins =  Marine number of bins
            <br>
            <br>"
          ),
          
          column(2, numericInput("ter_bins", "T. n. Bins", 2)),
          column(2, numericInput("sea_bins", "M. n. Bins", 2)),
          column(2, numericInput("macro_bins", "MA. n. Bins", 2))
        ),
        
        hr(),
        
        fluidRow(
          HTML("<h2>Plot interactive map</h2>"),
          checkboxInput("plotMap", HTML("<b>Plot</b>"), value = FALSE),
        ),
        
        hr(),
        
        fixedRow(HTML("<h2>Download map</h2>"),
                 column(6,
                     selectInput(inputId = "selectMap", label = "",
                                 choices = c("Terrestrial map" = "map1",
                                             "Marine map" = "map2",
                                             "Macro map" = "map3",
                                             "Terrestrial-Marine map" = "map4",
                                             "Macro-Marine map" = "map5"),
                                 selected = "map1",
                                 multiple = FALSE
                     ),
                     
                     uiOutput("download.Map")
                     )
                 ),
        ),
      
      column(width = 6,
             br(),
             br(),
             leafletOutput("myMap"),
             # br(),
             # uiOutput("download.Map"),
             br(),
             plotlyOutput('plot1'),
             br(),
             plotlyOutput('plot2'),
             br(),
             plotlyOutput('plot3'),
             )
      ),
    
    
    # tab panel 3 - About ---------------------------------------
    tabPanel(
      "About",
      includeHTML("about.html"),
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "plugins/carousel.css"),
        tags$script(src = "plugins/holder.js")
      ),
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      )
    )
    
  )
)