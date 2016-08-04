library(leaflet)
library(shiny)

# this is a demo of the shiny/leaflet application of creating
# an interactive map from a SBitem

# get the SB item information
# this is Roy's fisheries data
test_item<-sbtools::item_get("57115024e4b0ef3b7ca554f3")
names(test_item)
parent<-sbtools::item_get(test_item$parentId)
sbtools::item_list_children(parent)
sbtools::item_list_files(test_item)


# get the WFS
# In this instance, the attribute fields are part of the shapefile
# and no join between the wfs and attached files is performed
layer<-sbtools::item_get_wfs("571559c2e4b0ef3b7ca864c7")

# define an automated color ramp for this example
colPal<-RColorBrewer::brewer.pal(4,"Set1")
fixedBreaks=c(min(layer@data$M2p25), quantile(layer@data$M2p25,.25),median(layer@data$M2p25),quantile(layer@data$M2p25,.75),max(layer@data$M2p25))
symb<-cut(layer@data$M2p25,breaks=fixedBreaks,include.lowest=TRUE,right=TRUE)

# shaded relief - http://basemap.nationalmap.gov/arcgis/services/USGSShadedReliefOnly/MapServer/WMSServer?
# set Max bounds does not appear to be working
leaflet(layer) %>% addCircles(color = colPal[symb]) %>% addTiles() 
# explore setMapBounds, setView

# begin the shiny ui
# I have not played around with the interactivity options yet
# (actionButton)
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)


# This instance utilizes the NHD WGS service.  Unforunately this draws very slowly, and 
# does not properly display at certain scales.  I have not tried any of the other WGS
# services yet as background maps.
# This example is also not properly sized.
server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {layer
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addWMSTiles(
        "http://basemap.nationalmap.gov/arcgis/services/USGSHydroNHD/MapServer/WMSServer?",
        layers="0",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = ""
      ) %>%
      addCircles(data = points(),color = colPal[symb])
  })
}


shinyApp(ui, server)


#*****************************************************************
## NHD server behind points
## shaded relief - http://basemap.nationalmap.gov/arcgis/services/USGSShadedReliefOnly/MapServer/WMSServer?
#leaflet(layer) %>% addCircles(color = colPal[symb]) %>% addWMSTiles(
#  "http://basemap.nationalmap.gov/arcgis/services/USGSHydroNHD/MapServer/WMSServer?",
#  layers="0",
#  options = WMSTileOptions(format = "image/png", transparent = TRUE),
#  attribution = ""
#)
