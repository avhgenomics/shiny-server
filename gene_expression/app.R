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
library(tidyverse)
library(ggplot2)
library(lubridate)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fileInput(inputId = "load1",label = "Load Ct file",multiple = F),
    box(title = "raw",
        collapsible = T,
        DT::dataTableOutput("ct_dt")),
    box(title = "Set Options",
        collapsible = T,
        uiOutput("hk_selector"),
        uiOutput("hk_gene")
        )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  ct.df <- reactive({
    ct.df <- read.csv(file = input$load1$datapath,header = T,quote = "")
  })
  
  ct.names <- reactive({
    names(ct.df())
  })
  
  output$ct_dt <- DT::renderDataTable(ct.df())
  
  output$hk_selector <- renderUI({
    selectizeInput("df_names","What column represents gene information?", choices = ct.names())
  })
  
  genes.list <- reactive({
    genes.av <- ct.df()$input$hk_selector
    genes.av
  })
  
  output$hk_gene <- renderUI({
    selectizeInput("av_genes","What genes in this?", choices = genes.list())
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

