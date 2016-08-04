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
layer<-sbtools::item_get_wfs("57115024e4b0ef3b7ca554f3")

# define an automated color ramp for this example
colPal<-RColorBrewer::brewer.pal(4,"Set1")
fixedBreaks=c(min(layer@data$M2p25), quantile(layer@data$M2p25,.25),median(layer@data$M2p25),quantile(layer@data$M2p25,.75),max(layer@data$M2p25))
symb<-cut(layer@data$M2p25,breaks=fixedBreaks,include.lowest=TRUE,right=TRUE)
          

# begin the shiny ui
# I have not played around with the interactivity options yet
# (actionButton)
#ui <- fluidPage(
ui<-navbarPage("Yowzers",id="nav",
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)


# shinyUI(navbarPage("Superzip", id="nav",
#                    
#                    tabPanel("Interactive map",
#                             div(class="outer",
#                                 
#                                 tags$head(
#                                   # Include our custom CSS
#                                   includeCSS("styles.css"),
#                                   includeScript("gomap.js")
#                                 ),
#                                 
#                                 leafletOutput("map", width="100%", height="100%"),
#                                 
#                                 # Shiny versions prior to 0.11 should use class="modal" instead.
#                                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#                                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
#                                               width = 330, height = "auto",
#                                               
#                                               h2("ZIP explorer"),
#                                               
#                                               selectInput("color", "Color", vars),
#                                               selectInput("size", "Size", vars, selected = "adultpop"),
#                                               conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
#                                                                # Only prompt for threshold when coloring or sizing by superzip
#                                                                numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
#                                               ),
#                                               
#                                               plotOutput("histCentile", height = 200),
#                                               plotOutput("scatterCollegeIncome", height = 250)
#                                 ),
#                                 
#                                 tags$div(id="cite",
#                                          'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960-2010'), ' by Charles Murray (Crown Forum, 2012).'
#                                 )
#                             )
#                    ),
#                    
#                    tabPanel("Data explorer",
#                             fluidRow(
#                               column(3,
#                                      selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
#                               ),
#                               column(3,
#                                      conditionalPanel("input.states",
#                                                       selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
#                                      )
#                               ),
#                               column(3,
#                                      conditionalPanel("input.states",
#                                                       selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
#                                      )
#                               )
#                             ),
#                             fluidRow(
#                               column(1,
#                                      numericInput("minScore", "Min score", min=0, max=100, value=0)
#                               ),
#                               column(1,
#                                      numericInput("maxScore", "Max score", min=0, max=100, value=100)
#                               )
#                             ),
#                             hr(),
#                             DT::dataTableOutput("ziptable")
#                    ),
#                    
#                    conditionalPanel("false", icon("crosshair"))
# ))

# # This instance utilizes the NHD WGS service.  Unforunately this draws very slowly, and 
# # does not properly display at certain scales.  I have not tried any of the other WGS
# # services yet as background maps.
# # This example is also not properly sized.
# server <- function(input, output, session) {
#   
#   points <- eventReactive(input$recalc, {layer
#   }, ignoreNULL = FALSE)
#   
#   output$mymap <- renderLeaflet({
#     leaflet() %>%
#       addWMSTiles(
#         "http://basemap.nationalmap.gov/arcgis/services/USGSHydroNHD/MapServer/WMSServer?",
#         layers="0",
#         options = WMSTileOptions(format = "image/png", transparent = TRUE),
#         attribution = ""
#       ) %>%
#       addCircles(data = points(),color = colPal[symb])
#   })
# }
# 
# 
# shinyApp(ui, server)
