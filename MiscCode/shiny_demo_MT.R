library(shiny)
library(leaflet)
library(RColorBrewer)


## get the SB item information
## this is Roy's fisheries data
test_item<-sbtools::item_get("57115024e4b0ef3b7ca554f3")
names(test_item)
parent<-sbtools::item_get(test_item$parentId)
sbtools::item_list_children(parent)
sbtools::item_list_files(test_item)

## get the WFS
## In this instance, the attribute fields are part of the shapefile
## and no join between the wfs and attached files is performed
layer<-sbtools::item_get_wfs("57115024e4b0ef3b7ca554f3")

summary(layer@data)
M1p25<-layer@data$M1p25    
summary(quakes)

# ## define an automated color ramp for this example
# colPal<-RColorBrewer::brewer.pal(4,"Set1")
# fixedBreaks=c(min(layer@data$M1p25), quantile(layer@data$M1p25,.25),median(layer@data$M1p25),quantile(layer@data$M1p25,.75),max(layer@data$M1p25))
# symb<-cut(layer@data$M1p25,breaks=fixedBreaks,include.lowest=TRUE,right=TRUE)
# 
# # #***************************************************************
# ui <- bootstrapPage(
#   tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#   leafletOutput("map", width = "100%", height = "100%"),
#   absolutePanel(top = 10, right = 10,
#                 sliderInput("range", "Magnitudes", min(layer$M1p25), max(layer$M1p25),
#                             value = range(layer$M1p25), step = 0.1
#                 ),
#                 selectInput("colors", "Color Scheme",
#                             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
#                 ),
#                 checkboxInput("legend", "Show legend", TRUE)
#   )
# )
# 
# server <- function(input, output, session) {
# 
#   # Reactive expression for the data subsetted to what the user selected
#   filteredData <- reactive({
#     layer$M1p25[layer$M1p25 >= input$range[1] & layer$M1p25 <= input$range[2],]
#   })
# 
#   # This reactive expression represents the palette function,
#   # which changes as the user makes selections in UI.
#   colorpal <- reactive({
#      colorNumeric(input$colors, layer$M1p25)
#   })
# 
#   #leaflet(layer) %>% addCircles(color = colPal[symb]) %>% addTiles()
# 
# 
#   output$map <- renderLeaflet({
#     # Use leaflet() here, and only include aspects of the map that
#     # won't need to change dynamically (at least, not unless the
#     # entire map is being torn down and recreated).
#     leaflet(layer) %>% addTiles() %>%
#       fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
#   })
# 
#   # # Incremental changes to the map (in this case, replacing the
#   # # circles when a new color is chosen) should be performed in
#   # # an observer. Each independent set of things that can change
#   # # should be managed in its own observer.
#    # observe({
#    #   pal <- colorpal()
#    # 
#    #   leafletProxy("map", data = filteredData()) %>%
#    #     addCircles(radius = 2, weight = 1, color = symb,
#    #                fillColor = symb, fillOpacity = 0.7)
#    #     
#    # })
#   #
#   # Use a separate observer to recreate the legend as needed.
#   #  observe({
#   #    proxy <- leafletProxy("map", data = layer)
#   # #
#   # #   # Remove any existing legend, and only if the legend is
#   # #   # enabled, create a new one.
#   #   proxy %>% clearControls()
#   #   if (input$legend) {
#   #     pal <- colorpal()
#   #     proxy %>% addLegend(position = "bottomright",
#   #                         pal = pal, values = ~M1p25
#   #     )
#   #   }
#   # })
# }
# # 