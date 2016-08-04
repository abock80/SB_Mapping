library(shiny)
library(leaflet)
library(RColorBrewer)

# one script - ui.r
# one script - server.r
values<-list(y2030=c('ECHAM5','GENMON','Mean'),
             y2055=c('ECHAM5','GENMON','GFDL','Mean'),
             y2080=c('ECHAM5','GENMON','Mean'))

# radial buttons for GCMs 
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  #selectInput('selection', 'selection', c('GCM1',  'GCM2','GCM3','All'), 'GCM1'),
  absolutePanel(top=10,right=10,
    selectInput('selection', 'selection', c('y2030','y2055','y2080'), 'y2030'),
    uiOutput('selectUI'),
    sliderInput(inputId = "target", label = "Target",
              min = 0, max = length(values$y2030) - 1,
              step = 1,
              value = length(values$y2030) - 1),
    selectInput("colors", "Color Scheme",
                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
    checkboxInput("legend", "Show legend", TRUE))
    #verbatimTextOutput('summary')
  )

server <- function(input, output, session) {
  output$summary <- renderPrint({
    print(input$selection)
    print(input$target)
    print(values[[input$selection]])
    print(values[[input$selection]][input$target + 1])
  })
  
  # joe says to consider ditching this part
  output$selectUI <- renderUI({
    sel_values <- paste(paste0('"', values[[input$selection]], '"'), collapse = ',')
    print(sel_values)
    list(
      (HTML(
        sprintf('
                <script type="text/javascript">
                $(document).ready(function() {
                var vals = [%s];
                $(\'#target\').data(\'ionRangeSlider\').update(
                {values:vals,
                min: 0,
                max: %s,
                from:%s})
                })
                </script>
                ', sel_values, 
                length(values[[input$selection]]) - 1,
                length(values[[input$selection]]) - 1)))
    )}
  )
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
     colorNumeric(input$colors, dep2030$MEAN_2030)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(finalSegs) %>% addTiles()%>%
        fitBounds(~min(Longs), ~min(Lats), ~max(Longs), ~max(Lats))
  })
  
  observe({
     pal <- colorpal()

    popup <- paste0("<strong>Name: </strong>", 
                    finalSegs@data$seg_id)
    leafletProxy("map",data=finalSegs) %>% 
      clearShapes() %>%
      #addPolylines(color="red",weight=3,popup=~popup) 
      addPolylines(color=~pal(dep2030$MEAN_2030),weight=3,popup=~popup) 
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    pal <- colorpal()
    
    popup <- paste0("<strong>Name: </strong>", 
                    finalSegs@data$seg_id)
    
    proxy <- leafletProxy("map",data=finalSegs) %>% 
      clearShapes() %>%
      #addPolylines(color="red",weight=3,popup=~popup) 
      addPolylines(color=~pal(dep2030$MEAN_2030),weight=3,popup=~popup) 

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~dep2030$MEAN_2030
      )
    }
  })
  
}

shinyApp(ui, server)