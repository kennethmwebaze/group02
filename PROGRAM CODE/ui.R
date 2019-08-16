# Ctrl + Shift + S = Run Application
library(shinyjs)
library(shinydashboard)
shinyUI(fluidPage(
  # Add Javascript
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style.css")
    
  ),
  
  
  dashboardPage(skin="red",
                
                dashboardHeader(title = tags$a(href='https://www.unaids.org/en/regionscountries/countries/uganda',
                                               tags$img(id="img",src='taso.png'))),
                dashboardSidebar(
                  tags$br(),
                  tags$label("TASO AIDS "),
                  tags$br(),
                  tags$label("Living positively with HIV")
                  
                ),
                dashboardBody(
                  fluidRow(
                    
                    tags$h2("AIDS PREVALENCE ANALYSIS SYSTEM"),
                    
                    uiOutput("app"))
                  
                  
                )
  )
  
  
  
  
))






