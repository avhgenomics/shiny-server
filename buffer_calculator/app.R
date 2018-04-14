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
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Recipes",tabName = "buffer_tab")
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
  output$buffers_raw <- DT::renderDataTable(buffer_conditions.df())
}

# Run the application 
shinyApp(ui = ui, server = server)

