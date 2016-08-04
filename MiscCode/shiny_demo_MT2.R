library(shiny)
library(leaflet)
library(RColorBrewer)


#summary(quakes)
#dim(quakes)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")#,
  # absolutePanel(top = 10, right = 10,
  #               ## for slider input need to build dataframe for three time periods
  #               sliderInput("range", "Magnitudes", min(perChange), max(perChange),
  #                           value = range(perChange), step = 20
  #               ),
  #               selectInput("colors", "Color Scheme",
  #                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
  #               ),
  #               checkboxInput("legend", "Show legend", TRUE)
  # )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  # })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, perChange)
  # })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    #leaflet(quakes) %>% addTiles() %>%
    #  fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    leaflet(finalSegs) %>% addTiles()%>%
        fitBounds(~min(Longs), ~min(Lats), ~max(Longs), ~max(Lats))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
  #   pal <- colorpal()

    #leafletProxy("map", data = filteredData()) %>%
    #  clearShapes() %>%
    #  addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
    #             fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
    #  )
    leafletProxy("map",data=finalSegs) %>% 
      clearShapes() %>%
      addPolylines(color="red",weight=3,popup=~popup) 
  })
  
  # Use a separate observer to recreate the legend as needed.
  # observe({
  #   #proxy <- leafletProxy("map", data = quakes)
  #   proxy <- leafletProxy("map", data = perChange)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #   #                      pal = pal, values = ~mag
  #                         pal = pal, values = perChange
  #     )
  #   }
  # })
}

shinyApp(ui, server)