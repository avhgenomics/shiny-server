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
          fluidRow(
          box(title = "Create IDT Reorder form for selected primers",
              h3("Set bulk options:"),tags$br(),
              fluidPage(
                selectInput("idt_scale",label = "Set Scale",choices = list("25 nmole" = "25nm",
                                                                           "100 nmole" = "100nm",
                                                                           "250 nmole" = "250nm",
                                                                           "1 µmole" = "1um",
                                                                           "5 µmole" = "5um",
                                                                           "10 µmole" = "10um",
                                                                           "4 nmole Ultramer" = "4nmU",
                                                                           "20 nmole Ultramer" = "20nmU",
                                                                           "PAGE Ultramer" = "PU",
                                                                           "25 nmole Sameday" = "25nmS"
                                                                           ),multiple = F,selected = "25nm"),
                selectInput("idt_purification", label = "Set Purification",choices = list("Standard Desalting" = "STD",
                                                                                          "PAGE (+$50)" = "PAGE",
                                                                                          "HPLC (+$42)" = "HPLC",
                                                                                          "IE HPLC (+$45)" = "IEHPLC",
                                                                                          "RNase Free HPLC (+$75)" = "RNASE",
                                                                                          "Dual HPLC (+$80)" = "DUALHPLC",
                                                                                          "Dual PAGE & HPLC (+$130)" = "PAGEHPLC"),selected = "STD",multiple = F)
              ),
              downloadButton("reorderdl","Download bulk input for selected primers")))
          ),
  tabItem(tabName = "tutorial",
          h2("Getting Started Guides"),
          fluidPage(
            h3("Resolve Missing Information"),
            fluidRow(
          box(width = 10,
            
            "Older primers may be missing the manufacturing ID (the barcode value essentially).  
            To help with this you can find the information required.",
            h4("Step One: Download the missing information list"),
            "Click the button found below to download a copy of the current primer locations with missing information.",
            downloadButton("missinginfo","Download Missing Locations")),
            fluidRow(
              box(
              h4("Step Two: Find the primers"),
            "Using this list, find the primers at the location listed.",
            "Important: The primer manufacturing number should match the box location in the list, double check this!")),
          box(
            h4("Step Three: Fill in the Reference ID"),
            "Write the corresponding reference ID in the missing list.  The reference number is bold and large (see example label)",
            tags$img(src='sample_label.png',height=(580/3),width=(1020/3))
          )
          ))
          
          
  ),
  tabItem(tabName = "updater",
          h2("Update Primer Box"),
          box(width = NULL,
            h3("Step One: Upload scanned csv file"),
            fileInput("updaterinput", "Choose CSV File",
                      multiple = F,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            checkboxInput("csvheader","Header?",F),
            dataTableOutput("updateroutput")
          ),
          box(width = NULL,
            h3("Step Two: Select the box to update"),
            textInput("rack_in","Rack",value = "I"),
            textInput("col_in","Column",value = "1"),
            textInput("box_in","Box",value = "A"),
            dataTableOutput("updaterlocs")
          ),
          box(width = NULL,
              h3("Step Three: Save Merged Database CSV"),
              dataTableOutput("mergedb")
          )
          
  )
  
      )))

server <- function(input, output) {
  library(tidyverse)
  library(DT)
  
  primer.Update <- function(current_db,scanned_box,total_db){
    backup_db.name <- paste(Sys.Date(),current_db,"backup.csv",sep = '')
    write.csv(paste0("Primer_dash/backups/",backup_db.name),x = current_db,quote = F,row.names = F,col.names = T)
    current_db %>%
      transform(Manufacturing.ID = scanned_box$Manufacturing.ID[scanned_box$Location == current_db$Location],
                code = scanned_box$code[scanned_box$Location == current_db$Location],
                scanned = scanned_box$scanned[scanned_box$Location == current_db$Location]
      ) -> current_db
    updated_db<-merge(x = current_db,y = total_db,by.x = "Manufacturing.ID",all.x = T)
    write.csv(updated_db,file = "Primer_dash/csv/updated_db.csv",quote = F,row.names = F,col.names = T)
  }
  
  updatecsv <- reactive({read.csv(file = input$updaterinput$datapath,quote = "",header = input$csvheader)})
  

  
  primer_db <- reactive({
    primer.table <- read.csv(file = input$dbinput$datapath,header = T,quote = "",as.is = F)
     })
  
  update_list <- reactive({
    update.df<-read.csv(file = input$updaterinput$datapath,quote = "",header = input$csvheader,as.is=F)
    colnames(update.df) <- c("Manufacturing.ID","Code","Scanned")
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
    mergedb.df<- merge(x = primer_db(),y = add_locations(),by = "Location",all.x = T)
      
    mergedb.df
  })
  
  output$mergedb <- renderDataTable({
    mergedb.action()
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
  
  output$reorderdl <- downloadHandler(filename = function(){
    paste("IDT_reorder_form_",Sys.Date(),".csv",sep = "")
  },
  content = function(file) {
    s = input$primertable_rows_all
    reorder.df <- primer_db()[s, , drop = FALSE] %>% select(Sequence.Name,Sequence) %>%
      mutate(scale = input$idt_scale,
             Purification = input$idt_purification)
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