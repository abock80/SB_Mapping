#http://stackoverflow.com/questions/35056668/update-two-sets-of-radiobuttons-shiny

y2030=c('ECHAM5','GENMON','Mean',NA)
y2055=c('ECHAM5','GENMON','GFDL','Mean')
y2080=c('ECHAM5','GENMON','Mean',NA)
dd2<-data.frame(y2030,y2055,y2080)
yrs<-c('2030','2055','2080')
SF_vars<-c("Qann", "QSpring","QSummer","QFall","QWinter","QJanuary",
          "QFebruary","QMarch",'QApril',"QMay","QJune","Qjuly",
          "QAugust","QSeptember","QOctober","QNovember","QDecember")

ui<-shinyUI(fluidPage(

  titlePanel("Generic grapher"),
  sidebarLayout(
    mainPanel(
      tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
      ),
      
      textOutput("text1"),
      textOutput("text2"),
      textOutput("text3"),
      textOutput("text4"),
      textOutput("text5")
      
    ),
    sidebarPanel(
      
      selectInput("select", label = h3("Year"), 
                  choices = list("2030" = 1, "2055" = 2,
                                 "2080" = 3), selected = 1),
  
      actionButton("do", "Select Year"),
      
      radioButtons("GCMnames", label="GCM Names", choices="",selected=""),
      
      selectInput("select2", label = h3("Streamflow Variable"), 
                  choices = list("Qann" = 1, "QSpring" = 2,"QSummer" = 3,
                                 "QFall"=4,"QWinter"=5,"QJanuary"=6,
                                 "QFebruary"=7,"QMarch"=8,'QApril'=9,
                                 "QMay"=10,"QJune"=11,"Qjuly"=12,
                                 "QAugust"=13,"QSeptember"=14,"QOctober"=15,
                                 "QNovember"=16,"QDecember"=17), selected = 1),
     
      actionButton("do2", "Select Variable")
     
    )
 ))
)

server<-shinyServer(function(input, output, session){

  gcmNames <- eventReactive(input$do,{
    unlist(as.character(levels(dd2[,as.numeric(input$select)])))
  })

  observe({ 
    z<-gcmNames()
    updateRadioButtons(session, "GCMnames", choices = c(z), inline=TRUE,selected="")
  })
  
  datum<-reactive({
    try(data<-depAll[,paste(input$GCMnames,"_",fut_yr(),sep="")],silent=TRUE)
    return(data)
  })
  
  fut_yr<-eventReactive(input$do,{
    yr<-yrs[as.numeric(input$select)]
    return(yr)
  })
  
  
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
    summary(datum())
  })
  
  output$text5 <- renderText({ 
    input$do2
  })
  
})

shinyApp(ui = ui, server = server)