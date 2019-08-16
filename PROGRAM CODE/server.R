
#Wordcloud
library(datasets)
library(plotly)
library(plotrix)
library(NLP)
library(RColorBrewer)
library(tm)
library(dplyr)
library(ggplot2)
library(syuzhet)
#library(igraph)
library(quanteda)
library("SnowballC")

#Interface
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(stringr)
library(scatterplot3d)
library(shinyalert)
library(shinyBS)



library(shinyAce)
library(devtools)
library(sqldf)
library(RSQLite)




shinyServer(function(input, output,session) {
 
  db <- dbConnect(SQLite(), dbname="AppAdministrators.db")
  
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$login, {
 if(input$username!="" && input$password!=""){
    
    #c<-sqldf("SELECT * FROM DataManagers ", dbname = "AppAdministrators.db")  
    #nrow(c)
    sta<- 0;

    #for (row in 1:nrow(c)) {
      #username <- c[row, "Username"]
     # passwordd  <- c[row, "Password"]
      username="group02"
      passwordd="gerald"
      t<- grepl(input$username, username)
      f<- grepl(input$password, passwordd)
      if(t == TRUE & f == TRUE) {
        sta<- sta+1
      }
    
    #}
    if(sta>0){
      USER$Logged <- TRUE
     
     
    }
   
    
    else{
      output$message = renderText("Invalid user name or password,please try again!")
      
      show("message")
      
      
      delay(10000, hide("message", anim = TRUE, animType = "fade"))
      
    }
 }
    else{
      output$message = renderText("Enter user name or password,please!!!")
      
      show("message")
      
    }
    
    
  })
  observeEvent(input$logout,{
    USER$Logged<-FALSE
  })
  

  output$app <- renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
                      wellPanel(id = 'panel_login',
                                textInput('username', 'Username:'),
                                passwordInput('password', 'Password:'),
                                div(actionButton('login', 'Log in'),actionButton('reset', 'Reset'),actionButton('exit', 'Exit'), style='text-align: center;')
                      ),
                      textOutput("message")
      ))
    } else {
      
              
      
                   fluidPage(theme="style.css",
     
                             
                                 
                                 fileInput("file","Upload file")
                                 
                                
                                
                                 
                                 
                               )
                                 fluidRow(
                                    useShinyjs(),
                                    
                                   
                                  actionButton("logout","LOGOUT"),  
                                   tabsetPanel(id="navbar2",
                                               tabPanel("HOME",textOutput("home"),
                                                        textOutput("valid"),textOutput("vali"),align='center',style="font-size:17.5px;margin-top:85px;font-family:Times New Roman;color:rgb(5,90,5);"),
                                               tabPanel("UPLOAD FILE",uiOutput("put_file")),
                                               tabPanel("UPLOADED DATA",tableOutput("uploaded_data"),style="margin-left:2.5%"),
                                               tabPanel("VISUALISATIONS",uiOutput("visualisations")),
                                              
                                               tabPanel("PREDICTIONS",uiOutput("predictions")),
                                               
                                               #tabPanel("OTHERS",uiOutput("other")),
                                               tabPanel("DOWNLOAD",uiOutput("downloads"))
                                               
                                               
                                   )
                                 )
                                 
          
    } )
  
    output$home<-renderText("WELCOME")
    output$valid<-renderText(input$username) 
    output$vali<-renderText("To GROUP 2 AIDS Prevalence Analysis App") 
    
  
  
  output$help<-renderUI({
    mainPanel(style="margin-left:10%",
     tags$h4("WELCOME TO  AIDS SYSTEM GUIDE:"),
     
     tags$h5("Guideline 1:"),
     tags$p("Authorized application user should first upload a file to be analyzed.To do this,click 'Upload File' 
              Under Tab Panel and then locate the directory of the file containing data to be analyzed,then finally upload it.
            To view the uploaded file,click 'Uploaded Data' under Tab Panel."),
      tags$h5("Guideline 2:"),
     tags$p("To analyze data using various models,click 'visualisations' under Tab panel and then select
             model of choice to analyze data.Such analysis models include pie chart,bar graph and scatterplot. 
            "),
     
     
     
     tags$h5("Guideline 3:"),
     tags$p("Under 'predictions',the user can explore the expected prevalence of HIV in the years to come
            "),
     
     
     tags$h5("-Guideline 4:"),
     tags$p("Under 'Download', user can download visualisations in pdf or png format depending on the model selected.
            ")
    
    )
    
  })
  
  
  output$put_file<-renderUI({
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput("file","Upload file"),
       
        
        tags$br(),tags$br(),
        tags$br()
        
        
      ),
      mainPanel()
      
    )
  }) 
  output$downloads<-renderUI({
    sidebarLayout(
      sidebarPanel(width = 4,
                   useShinyalert(),
                   fluidRow(
                   
                     selectInput("nameit","Download Model",choices=c("select download","PIE CHART","BAR GRAPH","SCATTER PLOT")),
                     actionButton("down","Download"),style="margin-left:10%;margin-top:10.5%"
                   )
          
                   
      ),
      mainPanel()
    )
  }) 
  #Get data from the file and render it in a table  
  
    
  output$uploaded_data<-renderTable({
    req(data()) 
    
  },height = 400,width = 1000)
  
  data <- reactive({
    file1 <- input$file
    if (is.null(file1)) {
      return()
    }
    info<-read.csv(file=file1$datapath)
    info[is.na(info)]<-0
    return(info)
  })
  
  output$sum <- renderTable({
    if (is.null(data())) {
      return()
    }
    summary(data())[,1:1]
  }) 
  
  #plotting Bar graph using this funtion
  
  output$bargraph<-renderPlot({
    mydata<- req(data())
    barplot(table(mydata$Category),
            main="Number of Infections from different Categories",
            xlab="Category",
            ylab="Number of Infections",
            border="red",
            las = 2,
            col="blue",
            density=50
    )
    
    mainPanel(
      
      plotOutput("bargraph")
      
    )

  })
  
  
  #plotting pie chart using this funtion
  output$piechart<-renderPlot({
    mydata<-req(data())
    mytable <- table(mydata$Region)
    lbls <- paste(names(mytable), "\n", mytable, sep="")
    lbls <- paste(lbls,"%",sep="")
    pie3D(mytable,labels =lbls, radius=0.9,explode=0.1,main="PREVALENCE OF HIV PER REGION",
          col=c("brown","#ddaa00","pink","#dd00dd", "blue")
   
    #percent<-(mytable[[input$x]]/sum(mytable[[input$x]]))*100
    #percent<-round(percent,2)
    
    #labels<-piePlotData[[input$y]]
   
    #pie3D(mytable[[input$x]], main="Prevalence per Region",labels=paste(labels,percent,"%"),explode=0.1,theta=pi/6,mar=c(0,0,0,0)
          
          
          )

    mainPanel(
      
      plotOutput("piechart")
      
    )
    
    
     }
  )
 
  #simple Visualisations
  
  output$visualisations<-renderUI({
    
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput("select_model", "VISUALISATIONS",
                    c("select analysis tool","DATA SUMMARY","PIE CHART","BAR GRAPH"), "stat")
        
        
        
      ),
      
      mainPanel(id="xxxx",
         uiOutput("pp")      
      )
    )
   
    
  })
  
  
  output$pp<-renderUI({
    
    if(input$select_model=="PIE CHART"){
      plotOutput("piechart")
    }
    else if(input$select_model=="BAR GRAPH"){
      plotOutput("bargraph")
    }
   
    else if(input$select_model=="DATA SUMMARY"){
      tableOutput("summary")
    }
    
    
    
  })
  output$summary<-renderTable({
  
    summary(data())
    
  })
  
  
    output$visualisation<-renderUI({
    
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput("category", "Choose the category of the people:",
                    c("Category:","Adults" = "Adults",
                      "Children" = "Children",
                      "Youth" = "Youth",
                      
                    )
        ),
        
        selectInput("year", "Choose Year:",
                    c("Year:","2000" = "2000",
                      "2001" = "2001",
                      "2002" = "2002",
                      "2003" = "2003",
                      "2004" = "2004",
                      "2005" = "2005",
                      "2006" = "2006",
                      "2007" = "2007",
                      "2008" = "2008",
                      "2009" = "2009",
                      "2010" = "2010",
                      "2011" = "2011",
                      "2012" = "2012",
                      "2013" = "2013",
                      "2014" = "2014",
                    )
        ),
        paste("Guideline: Choose any of the category and see how many people are affected in easy year.
              
              
              ")
        
        
        ),
      
      mainPanel(id="xxxx",
                tags$br(),
                box(title="Total Number",uiOutput("result"),collapsible = TRUE,status = "success",height =180,solidHeader = TRUE ),
                box(title="On a certain year",uiOutput("yeardisplay"),collapsible = TRUE,status = "success",height =180,solidHeader = TRUE)
      )
      ) 
    
  })
  
    output$predictions<-renderUI({
      mydata<-req(data())
    
      model <- lm(Category ~ Percentage, data = mydata)
      model
      
      new.Percentages <- data.frame(
        Percentage = c(2, 5, 7)
      )
      
      predict(model, newdata = new.Percentages, interval = "confidence")
      
      
        
        mainPanel("predictions")
      
      
      
    })
  
  #download visualization models
  observeEvent(input$down, {
  
    mydata<-req(data())
    
    if(input$nameit=='BAR GRAPH'){
      pdf("PrintedManuals/bargraph.pdf",width=8,height = 6)
      mydata<- req(data())
      Category<-c('Adults','Youth','Children')
      dv<-c('Nsambya','Jinja','Mulago')
      rt<- 0;
      jo<- 0;
      st<- 0;
      
      
      
      for(i in 1:length(dv)){}
      
      print(jo) 
      print(st)
      print(rt)
      
      x = c(jo,st,rt) 
      
      
      
      
      real<-data.frame(Category,x)
      real
      
      
      
      barplot(real$x, las = 1,
              main ="BAR GRAPH SHOWING NUMBER OF INFECTIONS ",ylab = "Number of Chats",
              legend = Category, 
              col = c("green","yellow","red")) 
      
      shinyalert("Yes","Download complete!",type="success")
      dev.off()

    }
   
})

}
) 
?data.frame


