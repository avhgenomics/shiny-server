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
library(DT)
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Primer Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
    menuItem("Primer Table",tabName = "primertable",icon = icon("th")),
    menuItem("Update Primers",tabName = "updater",icon = icon("shopping-cart")),
    menuItem("How to help",tabName = "tutorial",icon = icon("shopping-cart"))
  )
                   ),
  dashboardBody(
    tabItems(tabItem(tabName = "dashboard",
    fluidPage(fluidRow(
    h2("PrimerDB"),
    box(
    title = "Load Primer Database",
    status = "primary",
    fileInput("dbinput", "Open Database CSV",
              multiple = F,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
    ),
    box(
      title = "Updates",
      status = "primary",
      "12-19-2017: Fixed issue for multiple filtering; added buttons to the top of the primer table to save the information in several formats.",
      tags$br(),
      "12-17-2017: The app has been initiated.  Currently, the section to update primers is not functioning."
    )),
    
    box(title = "Summary Stats",
      valueBoxOutput(outputId = "primernumber",width = NULL),
      valueBoxOutput(outputId = "primergood",width = NULL),
      valueBoxOutput(outputId = "primerempty",width = NULL),
      valueBoxOutput(outputId = "primererrors",width = NULL)
    ),
    box(
      title = "Contact",
      "This shiny app is developed by Andrew VonHandorf as a personal project.",
      tags$br(),
      "Having problems?",
      tags$br(),
      "Interested in contributing?",
      tags$br(),
      "Contact me."
    )
      )
    ),
    
  tabItem(tabName = "primertable",
          h2("Locate Primers"),
          DT::dataTableOutput("primertable"),
          fluidRow(box(title = "Download Selected Primers",
                       "Download a csv file of your primers for printing, etc",tags$br(),
          downloadButton("primerdl","Download Selected Primers")),
          box(title = "Create IDT Reorder form for selected primers",
              "Settings set to standard options; 25 nM, standard desalting",tags$br(),
              downloadButton("reorderdl","Download bulk input for selected primers")))
          ),
  tabItem(tabName = "tutorial",
          h2("How to help"),
          fluidPage(
          box(width = NULL,
            h3("Resolve Missing Information"),
            "Older primers may be missing the manufacturing ID (the barcode value essentially).  
            To help with this you can find the information required.",
            h4("Step One: Download the missing information list"),
            "Click the button found below to download a copy of the current primer locations with missing information.",
            downloadButton("missinginfo","Download Missing Locations"),
            h4("Step Two: Find the primers"),
            "Using this list, find the primers at the location listed.",
            "Important: The primer manufacturing number should match the box location in the list, double check this!",
            h4("Step Three: Fill in the Reference ID"),
            "Write the corresponding reference ID in the missing list.  The reference number is bold and large (see example label)"
          ),
          tags$img(src='sample_label.png',height=250,width=300))
          
          
  ),
  tabItem(tabName = "updater",
          h2("Update Primer Box"),
          box(width = 12,
            h3("Step One: Load the Scan and Master tables"),
            fileInput("updaterinput", "Upload Scanned CSV",
                      multiple = F,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            checkboxInput("csvheader","Header?",F),
            dataTableOutput("updateroutput")
          ),
          box(width = 12,
            h3("Step Two: Select the box to update"),
            textInput("rack_in","Rack",value = "I"),
            textInput("col_in","Column",value = "1"),
            textInput("box_in","Box",value = "A"),
            dataTableOutput("updaterlocs")
          ),
          box(width = 12,
              h3("Step Three: Check that the merge is correct"),
              dataTableOutput("mergedb")
          ),
          box(width = 12,
              h3("Step Four: Updated DB / Save")
          )
          
          
  )
  
      )))

server <- function(input, output) {
  
  primer_db <- reactive({
    primer.table <- read.csv(file = input$dbinput$datapath,header = T,quote = "",as.is = F,stringsAsFactors = F)
    primer.table
     })

  update_list <- reactive({
    update.df<-read.csv(file = input$updaterinput$datapath,quote = "",header = input$csvheader,as.is=F,stringsAsFactors = F)
    colnames(update.df) <- c("Manufacturing.ID","Code","Scanned")
    update.df$Manufacturing.ID <- as.numeric(as.character(update.df$Manufacturing.ID)) 
    update.df %>% select(-Code) %>% arrange(Scanned) -> update.df
    update.df
  })
  
  add_locations <- reactive({
    req(update_list())
    locations_annotated.df <- cbind(
    update_list(),
    Rack = boxlocs()$Rack,
    Column = boxlocs()$Column,
    Box = boxlocs()$Box,
    X = boxlocs()$X,
    Y = boxlocs()$Y,
    Location = boxlocs()$Location)
    locations_annotated.df
  })
  
  mergedb.action <- reactive({
    mergedb.df<- merge(x = primer_db(),y = add_locations(),by = "Location",all.x = T,suffixes = c(".old",".new"))
    mergedb.df<-mergedb.df %>% 
      mutate(Manufacturing.ID.merged = ifelse(is.na(Manufacturing.ID.new), Manufacturing.ID.old,Manufacturing.ID.new))
  })
  
  output$mergedb <- renderDataTable({
    
    mergedb.action() %>% select(Location,Manufacturing.ID.old,Manufacturing.ID.new,Manufacturing.ID.merged)
    
    })
  
  boxlocs <- reactive({
    if(nrow(update_list()) %% 9 == 0){
      Rack = rep(input$rack_in, times = 81)
      Column = rep(input$col_in, times = 81)
      Box = rep(input$box_in, times = 81)
      X=rep(c("a","b","c","d","e","f","g","h","i"),each = 9)
      Y = rep(seq(1:9),times = 9)
      Location = paste0(Rack,"-",Column,"-",Box,"-",X,Y)
      
    } 
    if (nrow(update_list()) %% 10 == 0){
      Rack = rep(input$rack_in, times = 100)
      Column = rep(input$col_in, times = 100)
      Box = rep(input$box_in, times = 100)
      X=rep(c("a","b","c","d","e","f","g","h","i","j"),each = 10)
      Y = rep(seq(1:10),times = 10)
      Location = paste0(Rack,"-",Column,"-",Box,"-",X,Y)
      
    }
    list(Rack = Rack,
         Column = Column,
         Box = Box,
         X = X,
         Y = Y,
         Location = Location)
  })
  
  primer_stats <- reactive({
    primer_db() %>%
      filter(is.na(Manufacturing.ID) == FALSE) %>%
      filter(is.na(Reference..) == FALSE) -> p.good
    p.good.n <- nrow(p.good)
    
    primer_db() %>%
      filter(is.na(Manufacturing.ID) == FALSE) %>%
      filter(Manufacturing.ID != "EMPTY") %>%
      filter(is.na(Reference..)) -> p.errors
    p.errors.simple <- p.errors %>%
      select(Location,Manufacturing.ID) %>%
      mutate(Write.RefID.Here = "     ")
    p.errors.n <- nrow(p.errors)
    
    p.empty <- primer_db() %>%
      filter(Manufacturing.ID == "EMPTY")
    
    p.empty.n <- nrow(p.empty)
    
    primer.n <- nrow(primer_db())
    
    list(p.good.n = p.good.n,
         p.errors.n = p.errors.n,
         primer.n = primer.n,
         p.errors = p.errors,
         p.errors.simple = p.errors.simple,
         p.empty.n = p.empty.n)
  })
  
  output$primertable <- DT::renderDataTable(
    primer_db(), filter = c('top'),extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
    
    )
  
  output$primerdl <- downloadHandler(filename = function(){
    paste("selected_primers",Sys.Date(),".csv",sep = "")
    },
                                      content = function(file) {
    s = input$primertable_rows_all
    write.csv(primer_db()[s, , drop = FALSE], file,quote = F,row.names = F,col.names = T)
  },contentType = "text/csv")
  
  output$reorderdl <- downloadHandler(filename = function(){
    paste("IDT_reorder_form_",Sys.Date(),".csv",sep = "")
  },
  content = function(file) {
    s = input$primertable_rows_all
    reorder.df <- primer_db()[s, , drop = FALSE] %>% select(Sequence.Name,Sequence) %>%
      mutate(scale = "25nm",Purification = "STD")
    write.csv(reorder.df, file,quote = F,row.names = F,col.names = F)
  },contentType = "text/csv")
  
  
  output$missinginfo <- downloadHandler(filename = function(){
    paste("missing_primer_info_",Sys.Date(),".csv",sep = "")
  },
  content = function(file) {
    write.csv(primer_stats()$p.errors.simple, file,quote = F,row.names = F,col.names = T)
  },contentType = "text/csv")
  
  output$primernumber <- renderValueBox({
    valueBox(value = primer_stats()$primer.n,subtitle = "Total Primers Served",icon = icon("database"),color = "purple")
  })
  
  output$primergood <- renderValueBox({
    valueBox(value = primer_stats()$p.good.n,subtitle = "with complete information",icon = icon("thumbs-up"),color = "green")
  })
  
  output$primerempty <- renderValueBox({
    valueBox(value = primer_stats()$p.empty.n,subtitle = "free primer spaces",icon = icon("thumbs-up"),color = "blue")
  })
  
  output$primererrors <- renderValueBox({
    valueBox(value = primer_stats()$p.errors.n,subtitle = "Locations missing Ref Number",icon = icon("warning"),color = "red")
  })

  output$updateroutput <- renderDataTable({
    req(input$updaterinput)
    update_list() 
    })
  
  output$updaterlocs <- renderDataTable({
    req(input$updaterinput)
    add_locations()
  })

  }

shinyApp(ui, server)