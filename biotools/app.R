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
    menuItem("Cell Counting",tabName = "cellcalculator",icon = icon("th")),
    menuItem("Dilution Calculator",tabName = "dilutioncalculator",icon = icon("th")),
    menuItem("Nanodrop QC (RNA)",tabName = "nanodroprna",icon = icon("th")),
    menuItem(text = "BioKIT Tools",
             menuSubItem(text = "Biotools",href = "http://avh.science:3838/biotools/"),
             menuSubItem(text = "Buffer Calcs",href = "http://avh.science:3838/buffer_calculator/"),
             menuSubItem(text = "Gel Analysis",href = "http://avh.science:3838/gel_densitometry/"),
             menuSubItem(text = "Primer Dashboard",href = "http://avh.science:3838/Primer_dash/"),
             menuSubItem(text = "RNAseq Browser",href = "http://avh.science:3838/rnaseqbrowser/")
    )
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
        
        
        ),
      tabItem(
        tabName = "dilutioncalculator",
        h2("Dilution Calculator"),
       box(title = "C1 * V1",
           width = 6,
           collapsible = T,
           fluidRow(
           numericInput(inputId = "c1",label = "C1",value = 0),
           selectInput(inputId = "c1unit",label = "C1 unit",choices = list("M" = "M",
                                                                           "uM" = "uM",
                                                                           "nM" = "nM",
                                                                           "pM" = "pM")))
           ) 
      ),
      
      tabItem(tabName = "nanodroprna",
              h2("Nanodrop QC, RNA"),
              fluidRow(column(4,box(title = "Nanodrop File Input",
                  width = 12,
                  collapsible = T,
                  fileInput(inputId = "nanofile",label = "Select Nanodrop txt file",multiple = F,placeholder = ".txt file")
                  ),
                  box(title = "QC Parameters",
                      width = 12,
                      collapsible = T,
                      numericInput("abslower",label = "Lower ratio threshold",value = 1.8,min = 0,step = 0.01),
                      numericInput("absupper",label = "Upper ratio threshold",value = 2.2,min = 0,step = 0.01))
                  ),
              column(8,box(title = "Results",
                  width = 12,
                  collapsible = T,
                  DT::dataTableOutput("nanotable")))),
              box(title = "QC Plot",
                  width = 6,
                  collapsible = T,
                  plotOutput("qcplot")
                  ),
              box(title = "QC Caution / Fail",
                  width = 6,
                  collapsible = T,
                  DT::dataTableOutput("failed_samples"))
              
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
   
   nano.df <- reactive({
     #nano.df <- read.csv(input$nanofile$datapath,quote = "",header = T)
     nano.df <- read.delim(file = input$nanofile$datapath,quote = "",header = T,sep = "\t")
     colnames(nano.df) <- c("SampleID",	"User.ID",	"Date",	"Time",	"ng.ul",	"A260",	"A280",	"X260.280",	"X260.230",	"Constant",	"Cursor Pos.",	"Cursor abs.",	"340 raw")
     nano.df %>%
       mutate(nucleotide_status = ifelse(X260.280 >= input$abslower & X260.280 <= input$absupper,"pass","fail"),
              other_status = ifelse(X260.230 >= input$abslower & X260.230 <= input$absupper,"pass","fail"),
              status_summary = ifelse(nucleotide_status == "pass" & other_status == "pass","pass",
                                      ifelse(nucleotide_status == "pass" & other_status == "fail","caution",
                                             ifelse(nucleotide_status == "fail" & other_status == "pass","caution","fail")))) -> df
     df
   })
   
   
   output$nanotable <- DT::renderDataTable(nano.df(),extensions = 'Responsive')
   
   
   output$qcplot <- renderPlot({
     plt.nano <- ggplot(nano.df(),aes(x = SampleID))
     plt.nano +
       geom_hline(yintercept = input$abslower)+
       geom_hline(yintercept = input$absupper)+
       geom_point(aes(y = X260.280),color = "#05c46b")+
       geom_point(aes(y = X260.230),shape = 18,size = 3,color = "#575fcf")
   })
   
   failed.df <- reactive({
     df <- nano.df() %>%
       filter(status_summary %in% c("fail","caution")) %>%
       select(SampleID,nucleotide_status,other_status,status_summary)
     df
   })
   output$failed_samples <- DT::renderDataTable(failed.df(),extensions = 'Responsive')
}

# Run the application 
shinyApp(ui = ui, server = server)

