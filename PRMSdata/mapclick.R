library(shiny)
library(leaflet)
library(RColorBrewer)
#http://stackoverflow.com/questions/34496597/leaflet-renderleaflet-not-working-in-shiny


y2030=c('ECHAM5','GENMON','Mean',NA)
y2055=c('ECHAM5','GENMON','GFDL','Mean')
y2080=c('ECHAM5','GENMON','Mean',NA)
dd2<-data.frame(y2030,y2055,y2080)


ui <- fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;font-family:\"Arial Narrow\",Arial,Sans-serif}"),
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
       textOutput("text1")
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
    
    #POI_ID<-c(as.character(finalSegs@data$seg_id))
    POI_ID<-c(finalSegs@data$ID)
    
    leafletProxy("map",data=finalSegs) %>%
      clearShapes() %>%
      #addPolylines(color="red",weight=3,popup=~popup)
      addPolylines(color=~pal(dep2030$MEAN_2030),weight=3,layerId=POI_ID,popup=~popup)
  })
  
  values<-reactiveValues(df_data=NULL)
  
  observeEvent(input$map_shape_click,{
    event <- input$map_shape_click
    
    print(event)  
    values$df_data<-matrix(depAll[event$id,],2,5)
    #print (values$df_data)
    if (is.null(event))
      return()
    #else
    #  return(event)
  })
  
  observeEvent(input$map_shape_click,{
    print(values$df_data)
    #values$df_data<-matrix(depAll[test$id,],2,5)
    #print(values$df_data)
  })
  
  # display 10 rows initially
  observeEvent(input$map_shape_click,{
    print(dim(values$df_data))
    output$ex1 <- DT::renderDataTable(
      DT::datatable(values$df_data, colnames=c("1","2","3","4","5"))
   )
  })
}

shinyApp(ui, server)