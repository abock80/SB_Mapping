library(shiny)
library(leaflet)
library(RColorBrewer)
#http://stackoverflow.com/questions/34496597/leaflet-renderleaflet-not-working-in-shiny


y2030=c('ECHAM5','GENMON','Mean',NA)
y2055=c('ECHAM5','GENMON','GFDL','Mean')
y2080=c('ECHAM5','GENMON','Mean',NA)
dd2<-data.frame(y2030,y2055,y2080)
yrs<-c('2030','2055','2080')
SF_vars<<-c("Qann", "QSpring","QSummer","QFall","QWinter","QJanuary",
           "QFebruary","QMarch",'QApril',"QMay","QJune","Qjuly",
           "QAugust","QSeptember","QOctober","QNovember","QDecember")


ui <- fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;font-family:\"Arial Narrow\",Arial,Sans-serif}"),
#             ".shiny-output-error { visibility: hidden; }",
#             ".shiny-output-error:before { visibility: hidden; }"),
  h1("Potential Changes in Streamflow - Eastern and Central Montana"),
  h3("18-year changes from baseline (Water Years 1982-1999) to Future Conditions"),
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
    tags$style(HTML("
        .tabs-above > .nav > li[class=active] > a {
                    background-color: #000;
                    color: #FFF;
                    }")),
   tabsetPanel(type="tabs",
               tabPanel("Map",leafletOutput("map",width = "90%", height = "400px")),
               tabPanel("Table",DT::dataTableOutput("ex1"))
   ),
   h4("Data from:"),
   h4("Chase, K.J., Haj, A.E., Regan, R.S., and Viger, R.J., 2016a, Potential Effects of Climate Change
      on Streamflow in Eastern and Central Montana (2013-2014 Analyses) - PRMS Input and Outpute Data:
      U.S. Geological Survey data release,",
      a("http://dx.doi.org/doi:10.5066/F7P26W5S",href="http://dx.doi.org/doi:10.5066/F7P26W5S.")),
   h4(""),
   h4("Chase, K.J., Haj, A.E., Regan, R.S., and Viger, R.J., 2016b, Potential effects of climate change 
      on streamflow for seven watersheds in eastern and central Montana, Journal of Hydrology: Regional 
      Studies, Volume 7, September 2016, Pages 69-81, ISSN 2214-5818, ",
      a("http://dx.doi.org/10.1016/j.ejrh.2016.06.001.",href="http://dx.doi.org/10.1016/j.ejrh.2016.06.001."))
  )
  ),
  h3("Map Instructions"),
  h4("1.  Select year from the dropdown and hit 'Select'"),
  h4("2.  Select the GCM by clicking on the radio buttond"),
  h4("3.  Select the streamflow variable and hit 'Select'"),
  h4(""),
  h3("Table Instructions"),
  h4("1.  Click on a segment on the map, this will generate\n a pop-up"),
  h4("2.  Click on the 'Table' tab to view results for that segment "),
  h4(""),
  h3("Notes"),
  h4("see Chase and others (2016b) for limitations"),
  h4("2030 Period:  Water Years 2021 through 2038"),
  h4("2055 Period:  Water Years 2046 through 2063"),
  h4("2080 Period:  Water Years 2071 through 2088")
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
                    finalSegs@data$Nseg)

    seg_ID<-c(finalSegs@data$Nseg)

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
                    finalSegs@data$Nseg)

    seg_ID<-c(finalSegs@data$Nseg)

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
    values$df_data<-dFrame[event$id,]
    
    print (values$df_data)
    print(input$GCMnames)
    #values2<-values[,grep("GFDL",colnames(values))]
    values2<-values$df_data[,grep(input$GCMnames,colnames(values$df_data))]
    print(values2)
    
    v1<-values2[,grep("2030",colnames(values2))]
    v2<-values2[,grep("2055",colnames(values2))]
    v3<-values2[,grep("2080",colnames(values2))]
    if (length(v1)>0){
      values$df_data<-t(mapply(c,v1,v2,v3))
    }else{
      values$df_data<-matrix(v2,nrow=17)
    }
    if (is.null(event))
      return()
  })

  observeEvent(input$map_shape_click,{
    print(dim(values$df_data))
    if (dim(values$df_data)[2]>1){
      datColumns<-c("2030","2055","2080")
    }else{
      datColumns<-c("2055")
    }
    print(datColumns)
    output$ex1 <- DT::renderDataTable(
      DT::datatable(values$df_data, options=list(pageLength=17),
                    colnames=datColumns,rownames = SF_vars)
    )
  })
  
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