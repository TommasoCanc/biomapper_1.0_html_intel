######################################
# Shiny App Biomapper v 0.1 - SERVER #
######################################

server <- function(input, output) {
  # Terrestria dataframe ----
  ter.reactive <- reactive({
    ter.df <-
      as.data.frame(t(
        data.frame(
          R1 = input$piemonte,
          # Piemonte
          R2 = input$aosta,
          # Valle d'Aosta
          R3 = input$lombardia,
          # Lombardia
          R4 = input$trentino,
          # Trentino-Alto Adige
          R5 = input$veneto,
          # Veneto
          R6 = input$friuli,
          # Friuli-Venezia Giulia
          R7 = input$liguria,
          # Liguria
          R8 = input$emilia,
          # Emilia-Romagna
          R9 = input$toscana,
          # Toscana
          R10 = input$umbria,
          # Umbria
          R11 = input$marche,
          # Marche
          R12 = input$lazio,
          # Lazio
          R13 = input$abruzzo,
          # Abruzzo
          R14 = input$molise,
          # Molise
          R15 = input$campania,
          # Campania
          R16 = input$puglia,
          # Puglia
          R17 = input$basilicata,
          # Basilicata
          R18 = input$calabria,
          # Calabria
          R19 = input$sicilia,
          # Sicilia
          R20 = input$sardegna,
          # Sardegna
          R21 = input$vaticano,
          # Vatican City
          R22 = input$smarino # San Marino
        )
      ))
    
    colnames(ter.df) <- "richness"
    ter.df$ID <- rownames(ter.df)
    return(ter.df)
  })
  
  # Sea dataframe ----
  sea.reactive <- reactive({
    mar.df <- as.data.frame(t(
      data.frame(
        M1 = input$m1,
        # Sector 1
        M2 = input$m2,
        # Sector 2
        M3 = input$m3,
        # Sector 3
        M4 = input$m4,
        # Sector 4
        M5 = input$m5,
        # Sector 5
        M6 = input$m6,
        # Sector 6
        M7 = input$m7,
        # Sector 7
        M8 = input$m8,
        # Sector 8
        M9 = input$m9 # Sector 9
      )
    ))
    
    colnames(mar.df) <- "richness"
    mar.df$ID <- rownames(mar.df)
    return(mar.df)
  })
  
  # Terrestial biogeographic regions dataframe ----
  # terBio.reactive <- reactive({
  #
  #   terBio.df <- as.data.frame(t(data.frame(ALP = input$alp, # Alpine
  #                                           CON = input$con, # Continental
  #                                           MED = input$med  # Mediterranean
  #   )))
  #
  #   colnames(terBio.df) <- "richness"
  #   terBio.df$ID <- rownames(terBio.df)
  #   return(terBio.df)
  # })
  
  
  # Merge shp and richness (terrestrial)
  ter.map.reactive <- reactive({
    it <- st_read("./data/Italy/Italy_complete.shp")
    it <-
      merge(it, ter.reactive(), by = "ID") # Merge shapefile with richness data
  })
  
  # Merge shp and richness (marine)
  sea.map.reactive <- reactive({
    sea <- st_read("./data/bio_sea/bio_sea_IT_4326.shp")
    sea <- merge(sea, sea.reactive(), by = "ID") # Merge shapefile with richness data
  })
  
  # Merge shp and richness (terrestiab biogegraphic regions)
  # terBio.map.reactive <- reactive({
  #   terBio <- st_read("./Terrestrial_biogeographic/terBiogeographic_4326.shp")
  #   terBio <- merge(terBio, terBio.reactive(), by = "ID") # Merge shapefile with richness data
  # })
  
  
  
  # Leaflet map ----
  output$myMap <- renderLeaflet({
    if (input$plotMap == 1) {
      # Terrestrial regions ----
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
      
      # Sea bioregions ----
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
      
      # # Terrestrial biogeographic regions ----
      # if(length(which(terBio.map.reactive()$richness > 0)) >= 2){
      #   mypalette.terBio <- colorBin(palette="Purples", domain=terBio.map.reactive()$richness,
      #                                na.color="transparent", bins=input$terBio_bins)
      # } else{
      #   mypalette.terBio <- colorNumeric(palette = "white", domain = 1,
      #                                    na.color = "transparent", alpha = FALSE, reverse = FALSE)
      # }
      
      # Terrestrial regions ----
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
      # Sea bioregions ----
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
      # # Terrestrial biogeographic regions ----
      # text.terBio <- paste(
      #   "ID: ", terBio.map.reactive()$ID,"<br/>",
      #   "Bioregion: ", terBio.map.reactive()$NAME, "<br/>",
      #   "Richness: ", terBio.map.reactive()$richness,
      #   sep="") %>%
      #   lapply(htmltools::HTML)
      
      map <- leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron")  %>%
        # Terrestrial regions ----
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
        group = "terrestrial.group"
      ) %>%
        # Sea bioregions ----
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
        group = "marine.group"
      ) %>%
        #   # Terrestrial biogeographic regions ----
      # addPolygons(data = terBio.map.reactive(),
      #             fillColor = ~mypalette.terBio(richness),
      #             stroke=TRUE,
      #             fillOpacity = 0.9,
      #             color="black",
      #             weight=0.3,
      #             label = text.terBio,
      #             labelOptions = labelOptions(
      #               style = list("font-weight" = "normal", padding = "3px 8px"),
      #               textsize = "13px",
      #               direction = "auto"
      #             ), group = "terBioreg.group") %>%
      
      
      leaflet::addLegend(
        "bottomright",
        pal = mypalette.ter,
        title = "Terrestrial",
        values = ter.map.reactive()$richness,
        group = "terrestrial.group"
      )  %>%
        
        leaflet::addLegend(
          "bottomright",
          pal = mypalette.sea,
          title = "Sea",
          values = sea.map.reactive()$richness,
          group = "marine.group"
        ) %>%
        #
        # addLegend("bottomright", pal = mypalette.terBio, title="TerBioreg",
        #           values = terBio.map.reactive()$richness,
        #           group = "terBioreg.group") %>%
        
        leaflet::addLayersControl(
          overlayGroups = c("terrestrial.group", "marine.group"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("terrestrial.group", "marine.group"))
      
      
    } else {
      leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 15,
                lat = 42,
                zoom = 5)
      
    }
  })
  
  # Plotly graph
  
  xform <- list(
    categoryorder = "array",
    categoryarray = c(
      "R1",
      "R2",
      "R3",
      "R4",
      "R5",
      "R6",
      "R7",
      "R8",
      "R9",
      "R10",
      "R11",
      "R12",
      "R13",
      "R14",
      "R15",
      "R16",
      "R17",
      "R18",
      "R19",
      "R20",
      "R21",
      "R22"
    )
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
      layout(xaxis = xform)
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
    )
  )
  
  
  # Plot main ggplot map
  mapPlot.1 <- reactive({
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
      ggtitle("Species richness")
  })
  
  # Plot smal ggplot map
  mapPlot.2 <- reactive({
    sf_use_s2(FALSE)
    it.1 <-
      cbind(ter.map.reactive(), st_coordinates(st_centroid(ter.map.reactive())))
    sea.1 <-
      cbind(sea.map.reactive(), st_coordinates(st_centroid(sea.map.reactive())))
    
    p2 <-  ggplot() +
      geom_sf(data = it.1,
              colour = "black",
              fill = "white") +
      geom_sf(data = sea.1,
              colour = "black",
              fill = "white") +
      geom_text(data = it.1, aes(X, Y, label = ID), size = 2) +
      geom_text(data = sea.1,
                aes(X, Y, label = ID),
                size = 3,
                col = "red") +
      theme_bw()
  })
  
  
  # Download button Map ----
  output$download.Map <- renderUI({
    downloadButton("download.gadmMap.but", "Download FaunaMap")
  })
  
  output$download.gadmMap.but <- downloadHandler(
    # <- Save map
    filename = function() {
      paste("FaunaMap", Sys.Date(), ".pdf")
    },
    content = function(file) {
      g1 <-
        tableGrob(ter.reactive()[1:11, 2:1],
                  rows = NULL,
                  theme = ttheme_minimal(base_size = 8))
      g2 <-
        tableGrob(ter.reactive()[12:22, 2:1],
                  rows = NULL,
                  theme = ttheme_minimal(base_size = 8))
      g3 <-
        tableGrob(sea.reactive()[1:nrow(sea.reactive()), 2:1],
                  rows = NULL,
                  theme = ttheme_minimal(base_size = 8))
      
      haligned <- gtable_combine(g1, g2, g3, along = 1)
      
      title.grob <- textGrob(
        label = "Terrestial and marine richness",
        x = unit(0, "lines"),
        y = unit(-1.5, "lines"),
        hjust = 0,
        vjust = 0,
        gp = gpar(fontsize = 12)
      )
      
      pdf(file,
          paper = "a4r",
          width = 11.69,
          height = 8.27)
      grid.arrange(
        mapPlot.1(),
        mapPlot.2(),
        arrangeGrob(haligned, top = title.grob),
        layout_matrix = rbind(c(1, 1, 2),
                              c(1, 1, 3))
      )
      dev.off()
    }
  )
  
}
