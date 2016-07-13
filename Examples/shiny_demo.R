library(shiny)
library(leaflet)
library(RColorBrewer)


# # get the SB item information
# # this is Roy's fisheries data
# test_item<-sbtools::item_get("57115024e4b0ef3b7ca554f3")
# names(test_item)
# parent<-sbtools::item_get(test_item$parentId)
# sbtools::item_list_children(parent)
# sbtools::item_list_files(test_item)
# 
# 
# # get the WFS
# # In this instance, the attribute fields are part of the shapefile
# # and no join between the wfs and attached files is performed
# layer<-sbtools::item_get_wfs("57115024e4b0ef3b7ca554f3")
# 
# # define an automated color ramp for this example
# colPal<-RColorBrewer::brewer.pal(4,"Set1")
# fixedBreaks=c(min(layer@data$M2p25), quantile(layer@data$M2p25,.25),median(layer@data$M2p25),quantile(layer@data$M2p25,.75),max(layer@data$M2p25))
# symb<-cut(layer@data$M2p25,breaks=fixedBreaks,include.lowest=TRUE,right=TRUE)

summary(quakes)
dim(quakes)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)