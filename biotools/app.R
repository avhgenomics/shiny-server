#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(lubridate)
library(DT)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Biotools"),
  dashboardSidebar(sidebarMenu(
    menuItem("Cell Counting",tabName = "cellcalculator",icon = icon("th"))
  )
                   ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "cellcalculator",
        h2("Hemocytometry Calculator"),
        box(width = 4,
          title = "Hemocytometry",
          collapsible = T,
          numericInput("livecellsnumber","Live Cells",min = 0,step = 1,value = 0),
          numericInput("deadcellsnumber","Dead Cells",min = 0,step = 1,value = 0),
          numericInput("squarescounted","Squares counted",min = 0,value = 4)
                   ),
        
        box(width = 4,
            title = "Dilution Information",
            collapsible = T,
            numericInput("dilsample","ul of sample used",min = 0,value = 10),
            numericInput("dilpbs","ul of pbs used",min = 0,value = 40),
            numericInput("diltrypan","ul of Trypan Blue",min = 0,step = 1,value = 0)
        ),
        
        box(width = 4,
            title = "Sample Information",
            collapsible = T,
            numericInput("samplevol","ml of original sample",min = 0,value = 4)
        ),
        
        box(width = 6,
            title = "Total Cells Info",
            collapsible = T,
            htmlOutput("livestats")
        ),
        
        box(width = 6,
            title = "Viability Stats",
            collapsible = T,
            htmlOutput("viabilitystats")
        )
        
        
        )
      )
    )
    
    
    
    
    
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   cell.counts <- reactive({
     dilution <- (input$dilsample + input$dilpbs + input$diltrypan) / input$dilsample
     live.count <- (input$livecellsnumber * dilution * 10000) / input$squarescounted
     dead.count <- (input$deadcellsnumber * dilution * 10000) / input$squarescounted
     total.count <- ((input$livecellsnumber + input$deadcellsnumber) * dilution * 10000) / input$squarescounted
     
     #cell.list <- list(Live = live.count)
     #cell.list
     cell.df <- data.frame(Live = live.count,
                            Dead = dead.count,
                            Dilution = dilution,
                           Total = total.count,
                           sampleml = input$samplevol,
                           row.names = NULL)
     
     cell.df
   })
   output$livestats <- renderUI({
     t.per.ml.text <- paste(tags$b(cell.counts()$Total),"Cells per ml","(",(cell.counts()$Total)/1000000,"Million per ml)")
     total.text <- paste(tags$b(cell.counts()$Total*cell.counts()$sampleml),"cells in",cell.counts()$sampleml,"ml", "(",(cell.counts()$Total*cell.counts()$sampleml)/1000000,"Million)")
     dead.text <- paste(tags$b(cell.counts()$Dead),"cells per ml")
     dead.text <- paste(tags$b(cell.counts()$Dead),"cells per ml")
     dilution.text <- paste("Dilution used:",tags$b(cell.counts()$Dilution[1]))
     HTML(
     paste(t.per.ml.text,total.text,dilution.text,sep = "<br/>")
     )})
   
   output$viabilitystats <- renderUI({
     live.text <- paste(tags$b(cell.counts()$Live),"live cells per ml")
     dead.text <- paste(tags$b(cell.counts()$Dead),"dead cells per ml")
     viability.text <- paste("Estimated Viability:", tags$b(paste0((cell.counts()$Live / cell.counts()$Total)*100,"%")))
     
     HTML(
       paste(live.text,dead.text,viability.text,sep = "<br/>")
     )
     })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

