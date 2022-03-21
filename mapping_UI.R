column(
  width = 6,
  HTML(
    "<h2>Insert the taxa richness for each Italian region</h2>
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
    HTML("<h2>Insert the taxa richness for each marine region</h2> <br>"),
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
      "<h2>Insert the number of bins</h2>
            <br>
            <b>Note</b>:
            T. n. Bins = Terrestral number of bins; M. n. Bins =  Marine number of bins
            <br>
            <br>"
    ),
    
    column(2, numericInput("ter_bins", "T. n. Bins", 2)),
    column(2, numericInput("sea_bins", "M. n. Bins", 2))
  ),
  
  hr(),
  
  checkboxInput("plotMap", "Plot interactive map", value = FALSE),
  
  
  
)