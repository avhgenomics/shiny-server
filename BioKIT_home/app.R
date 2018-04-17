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

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "BioKIT Home"),
  dashboardSidebar(menuItem(text = "BioKIT Tools",
                            menuSubItem(text = "Biotools",href = "http://avh.science:3838/biotools/"),
                            menuSubItem(text = "Buffer Calcs",href = "http://avh.science:3838/buffer_calculator/"),
                            menuSubItem(text = "Gel Analysis",href = "http://avh.science:3838/gel_densitometry/"),
                            menuSubItem(text = "Primer Dashboard",href = "http://avh.science:3838/Primer_dash/"),
                            menuSubItem(text = "RNAseq Browser",href = "http://avh.science:3838/rnaseqbrowser/")
  )),
  dashboardBody(
    box(title = tags$link("Biotools"),collapsible = T,width = 4,status = "success",solidHeader = T,
        "Provides basic calculators for general molecular techniques. This includes:",tags$br(),
        "Hemocytometry"),
    
    box(title = "Buffer Calcs",collapsible = T,width = 4,status = "success",solidHeader = T,
        "Calculates the buffer recipes for a given volume after providing a recipe csv file (see the formats section)",tags$br(),tags$br(),
        "Also contains common agarose gel calculations and have started to include common casting / loading volumes as well."),
    
    box(title = "Gel Analysis",collapsible = T,width = 4,status = "success",solidHeader = T,
        "Tools for analyzing signal returned from agarose gels;",tags$br(),tags$br(),
        "Includes:",tags$br(),
        "Basic Densitometry",
        tags$br(),
        "semi-automated peak finding",tags$br(),
        "Band size prediction",tags$br(),
        "Area estimations"),
    
    box(title = "Primer Dashboard",collapsible = T,width = 4,status = "success",solidHeader = T,
        "This is the original tool for managing primers.",tags$br(),
        "Includes:",tags$br(),
        "Quick reference from a csv file",tags$br(),
        "Easy updating of locations",tags$br(),
        "Primer testing",tags$br(),
        "Ability to scan new boxes and import them",tags$br(),
        "mobile friendly"
        ),
    
    box(title = "RNASeq Browser",collapsible = T,width = 4,status = "success",solidHeader = T,
        "Provides basic information from RNASeq results in csv format",tags$br(),
        "differential genes, adjustable cut-offs",tags$br(),
        "Basic Enrichment Statistics")
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

