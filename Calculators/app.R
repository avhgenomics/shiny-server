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


source(file = "calculator_scripts/pcr_mix.r")
source(file = "calculator_scripts/hemocytometry.r")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Calculators"),
  dashboardSidebar(sidebarMenu(id = "tabs",
                               menuItem("PCR",tabName = "pcr_calc",icon = icon("th")),
                               menuItem("Hemocytometer",tabName = "hemocytometry_tab",icon = icon("th"))),
    withTags({
    a(href = 'https://avh.science/shiny/',"Shiny Home")
    })),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "pcr_calc",
              pcr_master_UI_APP(id = "pcr",table_out_ID = "pcr_mm_table")
              ),
      tabItem(tabName = "hemocytometry_tab",
              hemocytometer_UI_APP("hemocytometer", table_out_ID = "hemocytometry_quickstat_table")
              )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pcr_mm_table <- renderTable({
    
    pcr_mm_table.df <- callModule(pcr_master_mixServer,"pcr")
    
    pcr_mm_table.df()
  })
  
  output$hemocytometry_quickstat_table <- renderTable({
    qc.table <- callModule(hemocytometer_Server_QuickStats,"hemocytometer")
    
    qc.table()
  })
  
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

