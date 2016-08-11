library(shiny)
library(leaflet)
library(RColorBrewer)
#http://stackoverflow.com/questions/34496597/leaflet-renderleaflet-not-working-in-shiny


y2030=c('ECHAM5','GENMON','MEAN',NA)
y2055=c('ECHAM5','GENMON','GFDL','Mean')
y2080=c('ECHAM5','GENMON','MEAN',NA)
dd2<-data.frame(y2030,y2055,y2080)
yrs<-c('2030','2055','2080')


ui <- fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;font-family:\"Arial Narrow\",Arial,Sans-serif}"),
#             ".shiny-output-error { visibility: hidden; }",
#             ".shiny-output-error:before { visibility: hidden; }"),
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
   ),
   textOutput("text1"),
   textOutput("text2"),
   textOutput("text3"),
   textOutput("text4"),
   textOutput("text5")
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
  
  fut_yr<-eventReactive(input$do,{
    yr<-yrs[as.numeric(input$select)]
    return(yr)
  })
  
  datum<-eventReactive(input$GCMnames,{
    data<-depAll[,paste(input$GCMnames,"_",fut_yr(),sep="")]
    return (data)
  })
  
  #This reactive expression represents the palette function,
  #which changes as the user makes selections in UI.
  colorpal <- eventReactive(input$GCMnames,{
   colorNumeric(input$colors, datum())
   #colorNumeric(input$colors,depAll$ECHAM5_2030)
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
      addPolylines(color=~pal(datum()),weight=3,popup=~popup)
  })

  # # Use a separate observer to recreate the legend as needed.
  #observe({
  #  pal <- colorpal()

    # popup <- paste0("<strong>Name: </strong>",
    #                 finalSegs@data$seg_id)
    # 
    # proxy <- leafletProxy("map",data=finalSegs) %>%
    #   clearShapes() %>%
    #   #addPolylines(color="red",weight=3,popup=~popup)
    #   addPolylines(color=~pal(datum()),weight=3,popup=~popup)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    # proxy %>% clearControls()
    # if (input$legend) {
    #   pal <- colorpal()
    #   proxy %>% addLegend(position = "bottomright",
    #                       pal = pal, values = ~datum()
    #   )
    # }
  #})
  # # display 10 rows initially
  # output$ex1 <- DT::renderDataTable(
  #   DT::datatable(FutMM, options = list(pageLength = 12),colnames=c("Month","GCM1","GCM2","Mean"))
  # )
  output$text1 <- renderText({ 
    input$GCMnames
    #input$select
  })
  
  output$text2 <- renderText({ 
    input$select
  })
  
  output$text3 <- renderText({ 
    fut_yr()
  }) 
  
  output$text4 <- renderText({ 
    datum()
  })
  
  #output$text5 <- renderText({ 
    #colorNumeric(input$colors, depAll$MEAN_2030)
  #})
  
}

shinyApp(ui, server)