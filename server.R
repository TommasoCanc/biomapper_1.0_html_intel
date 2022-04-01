######################################
# Shiny App Biomapper v 0.1 - SERVER #
######################################

server <- function(input, output) {
  
  
# Map panel ---- 
  
  # Terrestial geographical units ----
  ter.reactive <- reactive({
    ter.df <-
      as.data.frame(t(
        data.frame(
          R1 = input$piemonte, # Piemonte
          R2 = input$aosta, # Valle d'Aosta
          R3 = input$lombardia, # Lombardia
          R4 = input$trentino, # Trentino-Alto Adige
          R5 = input$veneto, # Veneto
          R6 = input$friuli, # Friuli-Venezia Giulia
          R7 = input$liguria, # Liguria
          R8 = input$emilia, # Emilia-Romagna
          R9 = input$toscana, # Toscana
          R10 = input$umbria, # Umbria
          R11 = input$marche, # Marche
          R12 = input$lazio, # Lazio
          R13 = input$abruzzo, # Abruzzo
          R14 = input$molise, # Molise
          R15 = input$campania, # Campania
          R16 = input$puglia, # Puglia
          R17 = input$basilicata, # Basilicata
          R18 = input$calabria, # Calabria
          R19 = input$sicilia, # Sicilia
          R20 = input$sardegna, # Sardegna
          R21 = input$vaticano, # Vatican City
          R22 = input$smarino, # San Marino
          R23 = input$corsica, # Corsica
          R24 = input$ticino # Canton Ticino
        )
      ))
    
    colnames(ter.df) <- "richness"
    ter.df$ID <- rownames(ter.df)
    return(ter.df)
    
  })
  
  # Marine geographical units ----
  sea.reactive <- reactive({
    mar.df <- as.data.frame(t(
      data.frame(
        M1 = input$m1, # Sector 1
        M2 = input$m2, # Sector 2
        M3 = input$m3, # Sector 3
        M4 = input$m4, # Sector 4
        M5 = input$m5, # Sector 5
        M6 = input$m6, # Sector 6
        M7 = input$m7, # Sector 7
        M8 = input$m8, # Sector 8
        M9 = input$m9  # Sector 9
      )
    ))
    
    colnames(mar.df) <- "richness"
    mar.df$ID <- rownames(mar.df)
    return(mar.df)
  
    })
  
  # Macro geographical units ----
  macro.reactive <- reactive({
    macro.df <- as.data.frame(t(
      data.frame(
        N = sum(input$friuli, input$veneto, input$trentino, input$lombardia, input$aosta, input$piemonte, input$liguria, input$emilia), # North
        S = sum(input$toscana, input$marche, input$umbria, input$lazio, input$abruzzo, input$molise, input$campania, input$puglia, input$basilicata, input$calabria), # South
        R19 = input$sicilia, # Sicilia
        R20 = input$sardegna, # Sardegna
        R21 = input$vaticano, # Vatican City
        R22 = input$smarino, # San Marino
        R23 = input$corsica, # Corsica
        R24 = input$ticino # Canton Ticino
      )
    ))
    
    colnames(macro.df) <- "richness"
    macro.df$ID <- rownames(macro.df)
    return(macro.df)
    
  })

    # Input data for mapping ----
  readInput <- reactive({
    
    # Dataset with taxa and sampling site
    # tools::file_ext(inFile$datapath) <- automatically identify the format
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(tools::file_ext(inFile$datapath) == "xlsx"){
      DF_init <- data.frame(readxl::read_excel(inFile$datapath, sheet = 1))
    }
    if(tools::file_ext(inFile$datapath) == "csv"){
      DF_init <- read.csv(inFile$datapath, header = TRUE, sep = ";")
    }
    if(tools::file_ext(inFile$datapath) == "txt"){
      DF_init <- read.table(inFile$datapath, header = TRUE)
    }
    
    # write some conditions (e.g. check the col names to verify the model)
    
    if(input$selectInputData == "model1"){ # MODEL 1
      DF_init <- DF_init[ ,c(3,2)]
      row.names(DF_init) <- DF_init$ID  
    } else { # MODEL 2
      # Number of species for column
      DF_init <- data.frame(colSums(ifelse(DF_init[ , c(19:29, 31:54)] == "yes", 1, 0)))
      colnames(DF_init) <- "richness"
      DF_init$ID <- c("M1", "M2",	"M3",	"M4",	"M5",	"M6",	"M7",	"M8",	"M9",	"N",	"S",		"R2",	"R1",	"R3",	"R4",	"R5",	"R6",	"R7",	"R8",	"R9",	"R11",	"R10",	"R12",	"R13",	"R14",	"R15",	"R16",	"R17",	"R18",	"R19",	"R20",	"R22",	"R21", "R24", "R23")
    }
    
    list(DF = DF_init)
    
  })
  
  # output$tbl <- renderUI({
  #   if (is.null(input$file1)) {
  #     showNotification(
  #       "Data do not upload",
  #       duration = 5,
  #       type = "warning",
  #       closeButton = TRUE
  #     )
  #   }
  #   if (!is.null(input$file1)) {
  #     datatable(
  #       readInput()$DF,
  #       rownames = FALSE,
  #       options = list(lengthChange = FALSE, scrollX = FALSE),
  #       editable = TRUE,
  #       width = 900
  #     )
  #   }
  # })
  
  # Merge shp and richness (Terrestrial)
  ter.map.reactive <- reactive({
    it <- st_read("./data/Italy_complete/Italy_Complete.shp")
    if(input$importData == 1){
      it <- merge(it, readInput()$DF, by = "ID") # Merge shapefile with richness data
    } else {
      it <- merge(it, ter.reactive(), by = "ID") # Merge shapefile with richness data
    }
  })
  
  # Merge shp and richness (Marine)
  sea.map.reactive <- reactive({
    sea <- st_read("./data/bio_sea/bio_sea_IT_4326.shp")
    if(input$importData == 1){
      sea <- merge(sea, readInput()$DF, by = "ID") # Merge shapefile with richness data
    } else {
      sea <- merge(sea, sea.reactive(), by = "ID") # Merge shapefile with richness data  
    }
  })
  
  # Merge shp and richness (Macro)
  macro.map.reactive <- reactive({
    macro <- st_read("./data/North_South/North_South_4326.shp")
    if(input$importData == 1){
      macro <- merge(macro, readInput()$DF, by = "ID") # Merge shapefile with richness data
    } else {
      macro <- merge(macro, macro.reactive(), by = "ID") # Merge shapefile with richness data 
    }
  })
  
  # Leaflet map ----
  output$myMap <- renderLeaflet({
    if (input$plotMap == 1) {
      
      # Terrestrial geographical units ----
      if (length(which(ter.map.reactive()$richness > 0)) >= 2) {
        mypalette.ter <-
          colorBin(
            palette = "Greys",
            domain = ter.map.reactive()$richness,
            na.color = "transparent",
            bins = input$ter_bins
          )
      } else{
        mypalette.ter <- colorNumeric(
          palette = "white",
          domain = 1,
          na.color = "transparent",
          alpha = FALSE,
          reverse = FALSE
        )
      }
      
      # Sea geographical units ----
      if (length(which(sea.map.reactive()$richness > 0)) >= 2) {
        mypalette.sea <-
          colorBin(
            palette = "Blues",
            domain = sea.map.reactive()$richness,
            na.color = "transparent",
            bins = input$sea_bins
          )
      } else{
        mypalette.sea <- colorNumeric(
          palette = "white",
          domain = 1,
          na.color = "transparent",
          alpha = FALSE,
          reverse = FALSE
        )
      }
      
      # Macro geographical units ----
      if (length(which(macro.map.reactive()$richness > 0)) >= 2) {
        mypalette.macro <-
          colorBin(
            palette = "Oranges",
            domain = macro.map.reactive()$richness,
            na.color = "transparent",
            bins = input$macro_bins
          )
      } else{
        mypalette.macro <- colorNumeric(
          palette = "white",
          domain = 1,
          na.color = "transparent",
          alpha = FALSE,
          reverse = FALSE
        )
      }
      
      # Terrestrial geographical units ----
      text.ter <- paste(
        "ID: ",
        ter.map.reactive()$ID,
        "<br/>",
        "Region: ",
        ter.map.reactive()$NAME,
        "<br/>",
        "Richness: ",
        ter.map.reactive()$richness,
        sep = ""
      ) %>%
        lapply(htmltools::HTML)
      
      # Sea geographical units ----
      text.sea <- paste(
        "ID: ",
        sea.map.reactive()$ID,
        "<br/>",
        "Bioregion: ",
        sea.map.reactive()$NAME,
        "<br/>",
        "Richness: ",
        sea.map.reactive()$richness,
        sep = ""
      ) %>%
        lapply(htmltools::HTML)
      
      # Macro geographical units ----
      text.macro <- paste(
        "ID: ",
        macro.map.reactive()$ID,
        "<br/>",
        "Bioregion: ",
        macro.map.reactive()$NAME,
        "<br/>",
        "Richness: ",
        macro.map.reactive()$richness,
        sep = ""
      ) %>%
        lapply(htmltools::HTML)
      
      
      map <- leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron")  %>%
        
        # Terrestrial geographical units ----
      leaflet::addPolygons(
        data = ter.map.reactive(),
        fillColor = ~ mypalette.ter(richness),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "black",
        weight = 0.3,
        label = text.ter,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        group = "Terrestrial"
      ) %>%
        
        # Sea geographical units ----
      leaflet::addPolygons(
        data = sea.map.reactive(),
        fillColor = ~ mypalette.sea(richness),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "black",
        weight = 0.3,
        label = text.sea,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        group = "Marine"
      ) %>%
       
        # Macro geographical units ----
      leaflet::addPolygons(
        data = macro.map.reactive(),
        fillColor = ~ mypalette.macro(richness),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "black",
        weight = 0.3,
        label = text.macro,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        group = "Macro"
      ) %>%
      
      leaflet::addLegend(
        "bottomright",
        pal = mypalette.ter,
        title = "Terrestrial",
        values = ter.map.reactive()$richness,
        group = "Terrestrial",
        opacity = 0.9
      )  %>%
        
        leaflet::addLegend(
          "bottomright",
          pal = mypalette.sea,
          title = "Marine",
          values = sea.map.reactive()$richness,
          group = "Marine",
          opacity = 0.9
        ) %>%
        
        leaflet::addLegend(
          "bottomright",
          pal = mypalette.macro,
          title = "Macro",
          values = macro.map.reactive()$richness,
          group = "Macro",
          opacity = 0.9
        ) %>%
        
        
        leaflet::addLayersControl(
          overlayGroups = c("Terrestrial", "Marine", "Macro"),
          options = layersControlOptions(collapsed = FALSE),
          position = "bottomleft"
        ) %>%
        hideGroup(c("Terrestrial", "Marine", "Macro"))
      
      
    } else {
      leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 15,
                lat = 42,
                zoom = 5)
      
    }
  })
  
  # Plotly graph ----
  # Reorder x tips plot 1
  xform.plot1 <- list(
    categoryorder = "array",
    categoryarray = c("R8", "R6", "R7", "R3", "R1", "R4", "R2", "R5", "R13", "R17", "R18", "R15", "R12", "R11", "R14", "R16", "R9",                       "R10", "R20", "R19", "R21", "R22", "R23", "R24"),
    tickvals = c("R8", "R6", "R7", "R3", "R1", "R4", "R2", "R5", "R13", "R17", "R18", "R15", "R12", "R11", "R14", "R16","R9", 
                 "R10","R20", "R19", "R21", "R22", "R23", "R24"), 
    ticktext = c("Emilia-Romagna", "Friuli-Venezia Giulia", "Liguria", "Lombardia", "Piemonte", "Trentino-Alto Adige",                              "Valle d’Aosta", "Veneto", "Abruzzo", "Basilicata", "Calabria", "Campania", "Lazio", "Marche", "Molise", 
                 "Puglia", "Toscana", "Umbria",  "Sardegna", "Sicilia", "Vatican City", "San Marino", "Corsica", "Canton Ticino"), 
    tickangle = 315
  )
  
  # Reorder x tips plot 2
  xform.plot2 <- list(
    categoryorder = "array",
    categoryarray = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9"),
    tickvals = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9"), 
    ticktext = c("Sector 1", "Sector 2", "Sector 3", "Sector 4", "Sector 5", "Sector 6", "Sector 7", "Sector 8", "Sector 9"), 
    tickangle = 315
  )
  
  # Reorder x tips plot 3
  xform.plot3 <- list(
    categoryorder = "array",
    categoryarray = c("N", "S", "R20", "R19", "R21", "R22", "R23", "R24"),
    tickvals = c("N", "S", "R20", "R19",  "R21", "R22", "R23", "R24"), 
    ticktext = c("North", "South", "Sardegna", "Sicilia", "Vatican City", "San Marino", "Corsica", "Canton Ticino"),
    tickangle = 315
  )
  
  output$plot1 <- renderPlotly(
    plot <- plot_ly(
      x = ter.map.reactive()$ID,
      y = ter.map.reactive()$richness,
      type = 'bar',
      marker = list(
        color = 'rgb(202,202,202)',
        line = list(color = 'rgb(101,101,101)',
                    width = 1.5)
      )
    ) %>%
      layout(xaxis = xform.plot1)
  )
  
  output$plot2 <- renderPlotly(
    plot <- plot_ly(
      x = sea.map.reactive()$ID,
      y = sea.map.reactive()$richness,
      type = 'bar',
      marker = list(
        color = 'rgb(173,216,230)',
        line = list(color = 'rgb(86,108,115)',
                    width = 1.5)
      )
    ) %>%
      layout(xaxis = xform.plot2)
  )
  
  output$plot3 <- renderPlotly(
    plot <- plot_ly(
      x = macro.map.reactive()$ID,
      y = macro.map.reactive()$richness,
      type = 'bar',
      marker = list(
        color = 'rgb(255,154,36)',
        line = list(color = 'rgb(86,108,115)',
                    width = 1.5)
      )
    ) %>%
      layout(xaxis = xform.plot3)
  )
  
  # Plot small ggplot map ----
  mapPlot.2 <- reactive({
    img <- readPNG("./data/italy_sea.png")
  })
  
  # Create map-plot ----
  mapPlot.1 <- reactive({
    
    # Map 1: Terrestrial
    if(input$selectMap == "map1"){
    p1 <- ggplot() +
      geom_sf(data = ter.map.reactive(), aes(fill = richness), colour = "black") +
      scale_fill_distiller(
        "Terrestrial richness",
        type = "seq",
        direction = 1,
        palette = "Greys"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = "transparent"),
        legend.position = "bottom"
      ) +
      ggtitle("Terrestrial richness")
    
    return(p1)
  }
    
    # Map 2: Marine
    if(input$selectMap == "map2"){
      p1 <- ggplot() +
        geom_sf(data = sea.map.reactive(), aes(fill = richness), colour = "black") +
        scale_fill_distiller(
          "Marine richness",
          type = "seq",
          direction = 1,
          palette = "Blues"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_line(color = "transparent"),
          legend.position = "bottom"
        ) +
        ggtitle("Marine richness")
      
      return(p1)
    }
    
    # Map 3: Macro
    if(input$selectMap == "map3"){
      p1 <- ggplot() +
        geom_sf(data = macro.map.reactive(), aes(fill = richness), colour = "black") +
        scale_fill_distiller(
          "Macro region richness",
          type = "seq",
          direction = 1,
          palette = "Oranges"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_line(color = "transparent"),
          legend.position = "bottom"
        ) +
        ggtitle("Macro region richness")
      
      return(p1)
    }
    
    # Map 4: Terrestrial Marine map
    if(input$selectMap == "map4"){
      p1 <- ggplot() +
        geom_sf(data = ter.map.reactive(), aes(fill = richness), colour = "black") +
        scale_fill_distiller(
          "Regional richness",
          type = "seq",
          direction = 1,
          palette = "Greys"
        ) +
        new_scale("fill") +
        geom_sf(data = sea.map.reactive(), aes(fill = richness), colour = "black") +
        scale_fill_distiller(
          "Marine richness",
          type = "seq",
          direction = 1,
          palette = "Blues"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_line(color = "transparent"),
          legend.position = "bottom"
        ) +
        ggtitle("Terrestrial and Marine species richness")
      
      return(p1)
    }
    
    # Map 5: Macro Marine map 
    if(input$selectMap == "map5"){
      p1 <- ggplot() +
        geom_sf(data = macro.map.reactive(), aes(fill = richness), colour = "black") +
        scale_fill_distiller(
          "Macro richness",
          type = "seq",
          direction = 1,
          palette = "Oranges"
        ) +
        new_scale("fill") +
        geom_sf(data = sea.map.reactive(), aes(fill = richness), colour = "black") +
        scale_fill_distiller(
          "Marine richness",
          type = "seq",
          direction = 1,
          palette = "Blues"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_line(color = "transparent"),
          legend.position = "bottom"
        ) +
        ggtitle("Macro and Marine species richness")
      
      return(p1)
    }
    
    }
  )
  
  # Create text for the maps
  textPlot.1 <- reactive({
    
    # Map 1: Terrestrial richness
    if(input$selectMap == "map1"){
      g1 <- tableGrob(data.frame(Geographical_Units = ter.map.reactive()$NAME[1:11],
                                 Richness = ter.map.reactive()$richness[1:11]),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      g2 <- tableGrob(data.frame(Geographical_Units = ter.map.reactive()$NAME[12:22],
                                 Richness = ter.map.reactive()$richness[12:22]),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      
      haligned <- gtable_combine(g1, g2, along = 1)
      
      title.grob <- textGrob(
        label = "Terrestrial richness",
        x = unit(0, "lines"),
        y = unit(0, "lines"),
        hjust = 0,
        vjust = 0,
        gp = gpar(fontsize = 12)
      )
      
      grid.arrange(
        mapPlot.1(),
        rasterGrob(mapPlot.2()),
        arrangeGrob(haligned, top = title.grob),
        layout_matrix = rbind(c(1, 1, 2),
                              c(1, 1, 3))
      )
    }
    
    # Map 2: Marine richness
    if(input$selectMap == "map2"){
      g1 <- tableGrob(data.frame(Geographical_Units = sea.map.reactive()$NAME, 
                                 Richness= sea.map.reactive()$richness),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      
      title.grob <- textGrob(
        label = "Marine richness",
        x = unit(0, "lines"),
        y = unit(0, "lines"),
        hjust = 0,
        vjust = 0,
        gp = gpar(fontsize = 12)
      )
      
      grid.arrange(
        mapPlot.1(),
        rasterGrob(mapPlot.2()),
        arrangeGrob(g1, top = title.grob),
        layout_matrix = rbind(c(1, 1, 2),
                              c(1, 1, 3))
      )
    }
    
    # Map 3: Macro richness
    if(input$selectMap == "map3"){
      g1 <- tableGrob(data.frame(Geographical_Units = macro.map.reactive()$NAME, 
                                 Richness= macro.map.reactive()$richness),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      
      title.grob <- textGrob(
        label = "Macro region richness",
        x = unit(0, "lines"),
        y = unit(0, "lines"),
        hjust = 0,
        vjust = 0,
        gp = gpar(fontsize = 12)
      )
      
      grid.arrange(
        mapPlot.1(),
        rasterGrob(mapPlot.2()),
        arrangeGrob(g1, top = title.grob),
        layout_matrix = rbind(c(1, 1, 2),
                              c(1, 1, 3))
      )
    }
    
    # Map 4: Terrestrial Marine map
    if(input$selectMap == "map4"){
      g1 <- tableGrob(data.frame(Geographical_Units = ter.map.reactive()$NAME[1:11],
                                 Richness = ter.map.reactive()$richness[1:11]),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      g2 <- tableGrob(data.frame(Geographical_Units = ter.map.reactive()$NAME[12:22],
                                 Richness = ter.map.reactive()$richness[12:22]),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      g3 <- tableGrob(data.frame(Geographical_Units = sea.map.reactive()$NAME, 
                                 Richness= sea.map.reactive()$richness),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      
      haligned <- gtable_combine(g1, g2, g3, along = 1)
      
      title.grob <- textGrob(
        label = "Terrestrial and marine richness",
        x = unit(0, "lines"),
        y = unit(-1.5, "lines"),
        hjust = 0,
        vjust = 0,
        gp = gpar(fontsize = 12)
      )
      
      grid.arrange(
        mapPlot.1(),
        rasterGrob(mapPlot.2()),
        arrangeGrob(haligned, top = title.grob),
        layout_matrix = rbind(c(1, 1, 2),
                              c(1, 1, 3))
      )
    }
    
    # Map 5: Macro Marine map 
    if(input$selectMap == "map5"){
      g1 <- tableGrob(data.frame(Geographical_Units = macro.map.reactive()$NAME, 
                                 Richness= macro.map.reactive()$richness),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      g2 <- tableGrob(data.frame(Geographical_Units = sea.map.reactive()$NAME, 
                                 Richness= sea.map.reactive()$richness),
                      rows = NULL,
                      theme = ttheme_minimal(base_size = 8))
      
      haligned <- gtable_combine(g1, g2, along = 1)
      
      title.grob <- textGrob(
        label = "Macro and Marine richness",
        x = unit(0, "lines"),
        y = unit(-1.5, "lines"),
        hjust = 0,
        vjust = 0,
        gp = gpar(fontsize = 12)
      )
      
      grid.arrange(
        mapPlot.1(),
        rasterGrob(mapPlot.2()),
        arrangeGrob(haligned, top = title.grob),
        layout_matrix = rbind(c(1, 1, 2),
                              c(1, 1, 3))
      )
      
    }
    
  })
  
  
  # Download button Map ----
  output$download.Map <- renderUI({
    downloadButton("download.gadmMap.but", "Download")
  })
  
  #  Save map
  output$download.gadmMap.but <- downloadHandler(  
    filename = function() {
      paste("Map_biomapper", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file,
          paper = "a4r",
          width = 11.69,
          height = 8.27)
      textPlot.1()
      dev.off()
    }
  )
  
  
# Report Panel ----
  
  readInput2 <- reactive({
    
    # Dataset with taxa and sampling site
    # tools::file_ext(inFile$datapath) <- automatically identify the format
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    if(tools::file_ext(inFile2$datapath) == "xlsx"){
      DF_report <- data.frame(readxl::read_excel(inFile2$datapath, sheet = 1))
    }
    if(tools::file_ext(inFile2$datapath) == "csv"){
      DF_report <- read.csv(inFile2$datapath, header = TRUE, sep = ";")
    }
    if(tools::file_ext(inFile2$datapath) == "txt"){
      DF_report <- read.table(inFile2$datapath, header = TRUE)
    }
    
    # write some conditions (e.g. check the col names to verify the model)
    
    DF_report <- DF_report[ ,c("phylum", "class", "order", "family", "scientificName", "scientificNameAuthorship")]
    
    # Sort by vector name [z] then [x]
    taxa.reorder <- DF_report[with(DF_report, order(phylum, class, order, family)), ]
    
    # Phylum column preparation ... Attenzione !is.na(unique(taxa.reorder$class)) prende solo il primo valore
    if(length(unique(taxa.reorder$phylum)) > 0 & !is.na(unique(taxa.reorder$phylum))) {
      toAdd <- data.frame()
      for (i in unique(taxa.reorder$phylum)) {
        toAdd.1 <- data.frame(phylum = c(i, rep(NA, length(which(taxa.reorder$phylum == i)) - 1)))
        toAdd <- rbind(toAdd, toAdd.1)
      }
      taxa.reorder$phylum <- toAdd$phylum
      rm(i, toAdd, toAdd.1)
    }
    
    # Class column preparation ... Attenzione !is.na(unique(taxa.reorder$class)) prende solo il primo valore
    if(length(unique(taxa.reorder$class)) > 0 & !is.na(unique(taxa.reorder$class))) {
      toAdd <- data.frame()
      for (i in unique(taxa.reorder$class)) {
        toAdd.1 <- data.frame(class = c(i, rep(NA, length(which(taxa.reorder$class == i)) - 1)))
        toAdd <- rbind(toAdd, toAdd.1)
      }
      taxa.reorder$class <- toAdd$class
      rm(i, toAdd, toAdd.1)
    }
    
    # Order column preparation
    if(length(unique(taxa.reorder$order)) > 0 & !is.na(unique(taxa.reorder$order))) {
      toAdd <- data.frame()
      for (i in unique(taxa.reorder$order)) {
        toAdd.1 <- data.frame(order = c(i, rep(NA, length(which(taxa.reorder$order == i)) - 1)))
        toAdd <- rbind(toAdd, toAdd.1)
      }
      taxa.reorder$order <- toAdd$order
      rm(i, toAdd, toAdd.1)
    }
    
    # Family column preparation
    if(length(unique(taxa.reorder$family)) > 0 & !is.na(unique(taxa.reorder$family))) {
      toAdd <- data.frame()
      for (i in unique(taxa.reorder$family)) {
        toAdd.1 <- data.frame(family = c(i, rep(NA, length(which(taxa.reorder$family == i)) - 1)))
        toAdd <- rbind(toAdd, toAdd.1)
      }
      taxa.reorder$family <- toAdd$family
      rm(i, toAdd, toAdd.1)
    }
    
    taxa.reorder[is.na(taxa.reorder)] = ""
    
    colnames(taxa.reorder) <- c("Phylum", "Class", "Order", "Family", "Scientific Name", "Authorship")
    
    list(report = taxa.reorder)
    
  })
  
  output$tblReport <- renderUI({
    if (is.null(input$file2)) {
      showNotification(
        "Data do not upload",
        duration = 5,
        type = "warning",
        closeButton = TRUE
      )
    }
    if (!is.null(input$file2)) {
      datatable(
        readInput2()$report,
        rownames = FALSE,
        options = list(lengthChange = FALSE, scrollX = FALSE),
        editable = FALSE,
        width = 1000
      )
    }
  })
  
  
  # Download button Report ----
  output$download.Report <- renderUI({
    if (!is.null(input$file2)) {
    downloadButton("download.Report.but", "Download")
    }
  })
  
  #  Save taxonomy report
  output$download.Report.but <- downloadHandler(  
    filename = function() {
      paste("Report_biomapper", Sys.Date(), ".html")
    },
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "shiny_report.Rmd")
      file.copy("./shiny_report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(reportName = input$reportName,
                     authorName = input$authorName,
                     tableTax = readInput2()$report)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        encoding="UTF-8",
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}
