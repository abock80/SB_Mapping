library(shiny)
library(leaflet)
library(RColorBrewer)
#http://stackoverflow.com/questions/34496597/leaflet-renderleaflet-not-working-in-shiny


y2030=c('ECHAM5','GENMON','Mean',NA)
y2055=c('ECHAM5','GENMON','GFDL','Mean')
y2080=c('ECHAM5','GENMON','Mean',NA)
dd2<-data.frame(y2030,y2055,y2080)
yrs<-c('2030','2055','2080')
SF_vars<-c("Qann", "QSpring","QSummer","QFall","QWinter","QJanuary",
           "QFebruary","QMarch",'QApril',"QMay","QJune","Qjuly",
           "QAugust","QSeptember","QOctober","QNovember","QDecember")


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
    
              selectInput("select2", label = h3("Streamflow Variable"), 
                        choices = list("Qann" = "ann", "QWinter"= "win","QSpring" = "spr","QSummer" = "sum",
                        "QFall"="fall","QJanuary"="jan",
                        "QFebruary"="feb","QMarch"="mar","QApril"="apr",
                        "QMay"="may","QJune"="jun","Qjuly"="jul",
                        "QAugust"="aug","QSeptember"="sep","QOctober"="oct",
                        "QNovember"="nov","QDecember"="dec"), selected = 1),
    
              actionButton("do2", "Select Variable"),
    
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
  values<-reactiveValues(df_data=NULL)
  
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
  
   strm_var<-eventReactive(input$do2,{
     #sVar<-SF_vars[as.numeric(input$select2)]
     sVar<-input$select2
     return(sVar)
  })
  
  datum<-eventReactive(input$do2,{
  #datum<-eventReactive(input$GCMnames,{
    #data<-depAll[,paste(input$GCMnames,"_",fut_yr(),sep="")]
    data<-dFrame[,paste(input$GCMnames,input$select2,fut_yr(),sep="_")]
    return (data)
  })
  
  #This reactive expression represents the palette function,
  #which changes as the user makes selections in UI.
  #colorpal <- eventReactive(input$GCMnames,{
  colorpal<-eventReactive(input$colors,{
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

  #observe({
  eventReactive(input$do2,{
    pal <- colorpal()

    popup <- paste0("<strong>Name: </strong>",
                    finalSegs@data$ID)

    seg_ID<-c(finalSegs@data$ID)

    leafletProxy("map",data=finalSegs) %>%
      clearShapes() %>%
      #addPolylines(color="red",weight=3,popup=~popup)
      #addPolylines(color=~pal(datum()),weight=3,layerId=POI_ID,popup=~popup)
      addPolylines(color=~pal(datum()),weight=3,layerId=seg_ID,popup=~popup)
  })

  #Use a separate observer to recreate the legend as needed.
  observe({
    pal <- colorpal()

    popup <- paste0("<strong>Name: </strong>",
                    finalSegs@data$ID)

    seg_ID<-c(finalSegs@data$ID)

    proxy <- leafletProxy("map",data=finalSegs) %>%
      clearShapes() %>%
      #addPolylines(color="red",weight=3,popup=~popup)
      #addPolylines(color=~pal(datum()),weight=3,layerId=POI_ID,popup=~popup)
      addPolylines(color=~pal(datum()),weight=3,layerId=seg_ID,popup=~popup)
    

    #Remove any existing legend, and only if the legend is
    #enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~datum(),
                          title=paste(strm_var(),input$GCMnames,sep=" ")
      )
    }
  })

  observeEvent(input$map_shape_click,{
    event <- input$map_shape_click

    print(event)
    #values$df_data<-matrix(dFrame[event$id,],2,5)
    values$df_data<-dFrame[event$id,]
    
    print (values$df_data)
    if (is.null(event))
      return()
  })

  # observeEvent(input$map_shape_click,{
  #   print(dim(values$df_data))
  #   output$ex1 <- DT::renderDataTable(
  #     DT::datatable(values$df_data, colnames=c("1","2","3","4","5"))
  #   )
  # })
  
  # # display 10 rows initially
  # output$ex1 <- DT::renderDataTable(
  #   DT::datatable(FutMM, options = list(pageLength = 12),colnames=c("Month","GCM1","GCM2","Mean"))
  # )
  
  # output$text1 <- renderText({
  #   input$GCMnames
  # })
  # 
  # output$text2 <- renderText({
  #   strm_var()
  # })
  # 
  # output$text3 <- renderText({
  #   fut_yr()
  # })
  # 
  # output$text4 <- renderText({
  #   paste(input$GCMnames,input$select2,fut_yr(),sep="_")
  # })
  # 
  # output$text5 <- renderText({
  #   datum()
  # })
  
}

shinyApp(ui, server)