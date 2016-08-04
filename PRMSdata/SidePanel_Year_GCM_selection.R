#http://stackoverflow.com/questions/35056668/update-two-sets-of-radiobuttons-shiny

y2030=c('ECHAM5','GENMON','Mean',NA)
y2055=c('ECHAM5','GENMON','GFDL','Mean')
y2080=c('ECHAM5','GENMON','Mean',NA)
dd2<-data.frame(y2030,y2055,y2080)

ui<-shinyUI(fluidPage(
  titlePanel("Generic grapher"),
  sidebarLayout(
    mainPanel(
      
      #dataTableOutput(outputId="l"),
      #dataTableOutput(outputId="w"),
      textOutput("text1"),
      textOutput("text2")
      
    ),
    sidebarPanel(
      
      selectInput("select", label = h3("Select box"), 
                  choices = list("2030" = 1, "2055" = 2,
                                 "2080" = 3), selected = 1),
  
      actionButton("do", "Select Year"),
      
      radioButtons("GCMnames", label="GCM Names", choices="",selected="")
     
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
  
  output$text1 <- renderText({ 
    input$GCMnames
  })
  
  #output$l <- renderDataTable({ a() })
  #output$w <- renderDataTable({ b() })  
})

shinyApp(ui = ui, server = server)