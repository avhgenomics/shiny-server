#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
  dashboardHeader(title = "Buffer Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Recipes",tabName = "buffer_tab"),
      menuItem(text = "Agarose Gels",tabName = "agarose_tab")
    )
                   ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "buffer_tab",
              box(width = 6,collapsible = T,
                  title = "Buffer Options",
                  fileInput(inputId = "buffer_file",label = "load CSV",multiple = F),
                  numericInput("vol_num",label = "Number of mL",value = 1,min = 1,step = 1),
                  uiOutput("buffer_opts")),
          box(width=12,title = "Buffer Recipe / Sample",collapsible = T,
              DT::dataTableOutput(outputId = "buffers_raw"),
              htmlOutput("buffer_text"))
      ),
      tabItem(tabName = "agarose_tab",
              box(width = 3, collapsible = T,
                  title = "Set Gel Options",
                  numericInput(inputId = "gel_percent",label = "Gel Percentage",value = 1.0,min = 0,step = 0.5),
                  numericInput(inputId = "tae_volume",label = "Volume Needed",value = 50,min = 0,step = 10)
                  ),
              box(width = 9, collapsible = T,
                  title = "Gel Recipe",
                  DT::dataTableOutput("gel_dt"))
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
options(DT.options = list(pageLength = 50))
  
  buffer.df <- reactive({
    df <- read.csv(file = input$buffer_file$datapath,header = T,sep = ",",quote = "",stringsAsFactors = T)
    df
  })
  
  output$buffer_opts <- renderUI({
    req(!is.null(input$buffer_file))
    tagList(
      selectInput(inputId = "experiment_select",label = "Select Experiment",choices = levels(buffer.df()[,1]),multiple = F),
      selectInput(inputId = "buffer_select",label = "Select Buffer(s)",choices = levels(buffer.df()[,2]),multiple = F)
    )
  })
  
  buffer_conditions.df <- reactive({
    req(!is.null(input$buffer_file))
   df <- buffer.df() %>%
     filter(Experiment == input$experiment_select) %>%
     filter(Buffer == input$buffer_select) %>%
     mutate(Amount = paste(amount.per.ml*input$vol_num,amount.unit),
            Stock = paste(Stock.Concentration,Stock.unit),
            Final = paste(Final.Concentration,Final.Unit)) %>%
     select(Reagent,Stock,Final,Amount,Notes)
   
   amount.name <- paste("For",input$vol_num,"mL")
   colnames(df) <- c("Reagent","Stock Concentration","Final Concentration",amount.name,"Additional Notes")
   
   df
     })
  
  output$buffer_text <- renderUI({HTML(paste("Experiment:",tags$strong(input$experiment_select),tags$br(),"Buffer:",tags$strong(input$buffer_select)))})
  output$buffers_raw <- DT::renderDataTable(buffer_conditions.df(),extensions = 'Responsive')
  
  
  #Agarose Gels
  agarose.df <- reactive({
    df <- data.frame(Reagent = c("Agarose","TAE","Ethidium Bromide"),
                     Final.Concentration = c(input$gel_percent,input$tae_volume,0.5),
                     Final.Unit = c("%","mL","µg/mL"),
                     mass.unit = c("grams","mL","µl"))
    
    grams <- (input$gel_percent / 100)*input$tae_volume
    
    df <- df %>%
      mutate(final.label = paste(Final.Concentration,Final.Unit),
             calculated = c(grams,input$tae_volume,(0.05*input$tae_volume)),
             calculated.label = paste(calculated,mass.unit)) %>%
      select(Reagent,final.label,calculated.label)
    colnames(df) <- c("Reagent","Final Concentration","Amounts to Add")
    
    df
  })
  
  output$gel_dt <- DT::renderDataTable(agarose.df(),extensions = "Responsive")
}

# Run the application 
shinyApp(ui = ui, server = server)

