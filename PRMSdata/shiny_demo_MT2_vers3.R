library(shiny)
library(leaflet)
#http://stackoverflow.com/questions/34496597/leaflet-renderleaflet-not-working-in-shiny


y2030=c('ECHAM5','GENMON','Mean',NA)
y2055=c('ECHAM5','GENMON','GFDL','Mean')
y2080=c('ECHAM5','GENMON','Mean',NA)
dd2<-data.frame(y2030,y2055,y2080)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
              selectInput("select", label = h3("Year"), 
                          choices = list("2030" = 1, "2055" = 2,
                                         "2080" = 3), selected = 1),
      
              actionButton("do", "Select Year"),
      
              radioButtons("GCMnames", label="GCM Names", choices="",selected=""),    
              selectInput("colors", "Color Scheme",
                          rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                          checkboxInput("legend", "Show legend", TRUE)),

  mainPanel(
   tabsetPanel(type="tabs",
               tabPanel("Map",leafletOutput("map",width = "90%", height = "400px")),
               tabPanel("Table",DT::dataTableOutput("ex1"))
   )
  )
  #verbatimTextOutput('summary')
  )
)

server <- function(input, output, session) {
  gcmNames <- eventReactive(input$do,{
    unlist(as.character(levels(dd2[,as.numeric(input$select)])))
  })
  
  observe({ 
    z<-gcmNames()
    updateRadioButtons(session, "GCMnames", choices = c(z), inline=TRUE,selected="")
  })
  # 
  # # This reactive expression represents the palette function,
  # # which changes as the user makes selections in UI.
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
  # display 10 rows initially
  output$ex1 <- DT::renderDataTable(
    DT::datatable(FutMM, options = list(pageLength = 12),colnames=c("Month","GCM1","GCM2","Mean"))
  )
}

shinyApp(ui, server)