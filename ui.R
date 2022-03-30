#################################
# Shiny App Biomapper v 0.1 -UI #
#################################

# Load libraries
if (!require("shiny"))
  install.packages("shiny")
require(shiny)
if (!require("shinydashboard"))
  install.packages("shinydashboard")
require(shinydashboard)
if (!require("shinyWidgets"))
  install.packages("shinyWidgets")
require(shinyWidgets)
if (!require("DT"))
  install.packages("DT")
require(DT)
if (!require("leaflet"))
  install.packages("leaflet")
require(leaflet)
if (!require("sf"))
  install.packages("sf")
require(sf)
if (!require("ggplot2"))
  install.packages("ggplot2")
require(ggplot2)
if (!require("ggnewscale"))
  install.packages("ggnewscale")
require(ggnewscale) # Plot two different scale fill
if (!require("RColorBrewer"))
  install.packages("RColorBrewer")
require(RColorBrewer)
if (!require("gridExtra"))
  install.packages("gridExtra")
require(gridExtra)
if (!require("grid"))
  install.packages("grid")
require(grid)
if (!require("plotly"))
  install.packages("plotly")
require(plotly)
if (!require("shinyjs"))
  install.packages("shinyjs")
require(shinyjs)
if (!require("png"))
  install.packages("png")
require(png)
if (!require("dplyr"))
  install.packages("dplyr")
require(dplyr)
if (!require("knitr"))
  install.packages("knitr")
require(knitr)
if (!require("kableExtra"))
  install.packages("kableExtra")
require(kableExtra)


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
                  href = "images/pic1.png") # Cambiare questa foto
      ),
      tags$footer(includeHTML("footer.html"))
    ),
    
    
    # tab panel 2 - Main Panel ------------------------
    tabPanel(
      title = "Map Panel",
      column(
        width = 6,
        
        # Import your data ----      
        HTML(
          "<h2>Load data</h2>
          <b>Note</b>: Data have to be load in a specific format. Please go to the <i><b>Help</b></i> panel for more information.
          <br>
          <br>
          Download <b>model 1</b> template at: <a href='https://github.com/TommasoCanc/biomapper_1.0_html_intel/blob/main/model_1.xlsx' target='_blank'>model 1</a>
          <br>
          Download <b>model 2</b> template at: <a href='https://github.com/TommasoCanc/biomapper_1.0_html_intel/blob/main/model_2.xlsx' target='_blank'>model 2</a>
          <br>
          <br>
          <b>Select your data</b>
            <h5>Data can be loaded in <b>.xlsx</b>, <b>.csv</b>, or <b>.txt</b> formats.</h5>"
        ),
        
        fluidRow(
          column(6,
                 fileInput("file1", label = ""), # Import data box
          ),
          
          column(6,
                 selectInput(inputId = "selectInputData", label = "",
                             choices = c("Model 1" = "model1", # Like example 1
                                         "Model 2" = "model2"), # Like example 2
                             selected = "model1",
                             multiple = FALSE))
        ),
        # If selected the graphs are plot with this data
        checkboxInput(inputId = "importData",
                      label = "Use the imported data",
                      value = FALSE),
        
        hr(),
        
        HTML(
          "<h2><b>Terrestial</b> geographical units</h2>
          <br>
          <b>Note</b>: F.V.G. = Friuli venezia Giulia; T.A.A. = Trentino Alto Adige, Em-Ro: Emilia Romagna
          <br>
          <br>"
        ),
        
        fluidRow( # Nord
          HTML("<h4><b>Continental Italy</b></h4>"),
          column(2, numericInput("emilia", "Em-Ro", 0)),
          column(2, numericInput("friuli", "F.V.G.", 0)),
          column(2, numericInput("liguria", "Liguria", 0)),
          column(2, numericInput("lombardia", "Lombardia", 0)),
          column(2, numericInput("piemonte", "Piemonte", 0)),
          column(2, numericInput("trentino", "T.A.A.", 0)),
          column(2, numericInput("aosta", "Valle Aosta", 0)),
          column(2, numericInput("veneto", "Veneto", 0))
        ),
        
        br(),
        
        fluidRow( # Sud
          HTML("<h4><b>Peninsular Italy</b></h4>"),
          column(2, numericInput("abruzzo", "Abruzzo", 0)),
          column(2, numericInput("basilicata", "Basilicata", 0)),
          column(2, numericInput("calabria", "Calabria", 0)),
          column(2, numericInput("campania", "Campania", 0)),
          column(2, numericInput("lazio", "Lazio", 0)),
          column(2, numericInput("marche", "Marche", 0)),
          column(2, numericInput("molise", "Molise", 0)),
          column(2, numericInput("puglia", "Puglia", 0)),
          column(2, numericInput("toscana", "Toscana", 0)),
          column(2, numericInput("umbria", "Umbria", 0))
        ),
        
        br(),
        
        fluidRow( # Isole
          HTML("<h4><b>Islands</b></h4>"),
          column(2, numericInput("sardegna", "Sardegna", 0)),
          column(2, numericInput("sicilia", "Sicilia", 0))
        ),
        
        br(),
        
        fluidRow( 
          HTML("<h4><b>Geopolitical units biogeographically related to Italy</b></h4>"),
          br(),
          column(2, numericInput("vaticano", "Vatican City", 0)),
          column(2, numericInput("smarino", "San Marino", 0)),
          column(2, numericInput("corsica", "Corsica", 0)),
          column(2, numericInput("ticino", "Canton Ticino", 0))
          ),
        
        hr(),
        
        fluidRow(
          HTML("<h2>Insert the taxa richness for each
               <br>
               <b>Marine</b> geographical units</h2> <br>"),
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
        
        hr(),
        
        fluidRow(
          HTML(
            "<h2>Plot interactive map</h2>
            <br>
            <b>Note</b>:
            T. n. Bins = Terrestral number of bins; M. n. Bins =  Marine number of bins; MA. n. Bins =  Macro number of bins
            <br>
            <br>"
          ),
          
          column(2, numericInput("ter_bins", "Terrestial Bins", 2)),
          column(2, numericInput("sea_bins", "Marine Bins", 2)),
          column(2, numericInput("macro_bins", "Macro Bins", 2))
          ),
          
          br(),
          
          checkboxInput("plotMap", HTML("<b>Plot</b>"), value = FALSE),
        
        
        hr(),
        
        fluidRow(HTML("<h2>Download map</h2>"),
                 column(6,
                        selectInput(inputId = "selectMap", label = "",
                                    choices = c("Terrestrial map" = "map1",
                                                "Marine map" = "map2",
                                                "Macro map" = "map3",
                                                "Terrestrial-Marine map" = "map4",
                                                "Macro-Marine map" = "map5"),
                                    selected = "map1",
                                    multiple = FALSE),
                        
                        uiOutput("download.Map")
                        )
                 )
        ),
      
      
      column(width = 6,
             br(),
             br(),
             #uiOutput("tbl"),
             leafletOutput("myMap"),
             br(),
             plotlyOutput('plot1'),
             br(),
             plotlyOutput('plot2'),
             br(),
             plotlyOutput('plot3'),
      )
    ),
    
    # tab panel 3 - Data Report ----------------------------------
    tabPanel(title = "Data Report",
             fluidRow(
               column(width = 5,
                      HTML("<h2>Taxonomy Report</h2>
                           <br>
                           <b>Note</b>: Data have to be load in the format <i><b>model 2</b></i>.
                           <br>
                           Download <b>model 2</b> template at: <a href='https://github.com/TommasoCanc/biomapper_1.0_html_intel/blob/main/model_2.xlsx' target='_blank'>model 2</a>"),
                      
                      br(),
                      br(),
                      
                      column(width = 6,
                             HTML("<b>Select your data (<b>.xlsx</b>, <b>.csv</b>, or <b>.txt</b>)</b>"),       
                             fileInput("file2", label = ""), # Import data box
                             ),
                      
                      column(width = 6,
                             HTML("<b>Report name</b>"),       
                             textInput("reportName", "", "Text your report name"), # Insert the report name
                             HTML("<b>Report Author</b>"),       
                             textInput("authorName", "", "Text author names")
                      )
                      ),
               
               column(width = 7,
                      uiOutput("tblReport"),
                      uiOutput("download.Report")
                      )
               ),
             
             hr()
             ),
    
    
    # tab panel 4 - About ---------------------------------------
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