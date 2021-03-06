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
library(data.table)
library(tidyverse)
library(DT)
library(rvest)


ui <- dashboardPage(
  dashboardHeader(title = "Primer Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
    menuItem("Primer Table",tabName = "primertable",icon = icon("th")),
    menuItem("Update Primers",tabName = "updater",icon = icon("shopping-cart")),
    menuItem("Update Master",tabName = "masterupdate",icon = icon("shopping-cart")),
    menuItem("Update RefIDs",tabName = "refupdate",icon = icon("shopping-cart")),
    menuItem("How to help",tabName = "tutorial",icon = icon("shopping-cart")),
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
      title = "Updating Files?",
      status = "primary",
      "If you are updating files or locations, load your master file here.",
      tags$br(),
      fileInput("masterinput", "Open Master CSV",
                multiple = F,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    )),

    box(title = "Summary Stats",
      valueBoxOutput(outputId = "primernumber",width = 8),
      valueBoxOutput(outputId = "primergood",width = 8),
      valueBoxOutput(outputId = "primermanual",width = 8),
      valueBoxOutput(outputId = "primerempty",width = 8),
      valueBoxOutput(outputId = "primererrors",width = 8)
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
              downloadButton("reorderdl","Download bulk input for selected primers")),
          box(width = 5,collapsible = T,collapsed = T,title = "Amplicon Check Settings",
              textInput("fwprimer","Forward Primer",placeholder = "ATGC"),
              textInput("rvprimer","Reverse Primer",placeholder = "CGTA"),
              selectInput("amporg","Select Species / Build",choices = list("Mouse, mm10" = "https://genome.ucsc.edu/cgi-bin/hgPcr?org=Mouse&db=mm10",
                                                                           "Mouse, mm9" = "https://genome.ucsc.edu/cgi-bin/hgPcr?org=Mouse&db=mm9",
                                                                           "Human, hg38" = "https://genome.ucsc.edu/cgi-bin/hgPcr?org=Human&db=hg38",
                                                                           "Human, hg19" = "https://genome.ucsc.edu/cgi-bin/hgPcr?org=Human&db=hg19"),multiple = F,selected = "https://genome.ucsc.edu/cgi-bin/hgPcr?org=Mouse&db=mm10"),
              numericInput("ampprod","Set max product size",value = 4000),
              numericInput("ampminperf","Min Perfect Match",value = 15),
              numericInput("ampmingood","Min Good Match",value = 15)

              ),
          box(collapsible = T,collapsed = T,title = "Amplicon Results",
              h3("Genomic Amplicons"),
              htmlOutput("gampcheck"),
              h3("mRNA Amplicons"),
              htmlOutput("mampcheck"))
          )),
  tabItem(tabName = "tutorial",
          h2("How to help"),
          fluidPage(
          box(width = NULL,collapsible = T,collapsed = T,title = "Resolve Missing Information",
            "Older primers may be missing the manufacturing ID (the barcode value essentially).
            To help with this you can find the information required.",
            h4("Step One: Download the missing information list"),
            "Click the button found below to download a copy of the current primer locations with missing information.",
            downloadButton("missinginfo","Download Missing Locations"),
            h4("Step Two: Find the primers"),
            "Using this list, find the primers at the location listed.",
            "Important: The primer manufacturing number should match the box location in the list, double check this!",
            h4("Step Three: Fill in the Reference ID"),
            "Write the corresponding reference ID in the missing list.  The reference number is bold and large (see example label)",tags$br(),
            tags$img(src='sample_label.png',height=250,width=300)
          )
          )


  ),
  tabItem(tabName = "masterupdate",

          h2("Update Master File"),
          box(width = 6,
              title = "Load the New COA File",
              fileInput("coaupdateinput", "coa CSV",
                        multiple = F,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"))),

          box(width = 12,
              title = "Updated Master Table",
              DT::dataTableOutput("umout")),
              downloadButton("saveupdatedmaster","Download Updated Master")

          ),
  tabItem(tabName = "refupdate",
          box(width = 6,
              fileInput("refinput", "refID updates",
                        multiple = F,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"))
              ),
          box(width = 12,
              DT::dataTableOutput("refidmerged"),
              downloadButton("refidmerged_dl","Download Updated Master"))
          ),

  tabItem(tabName = "updater",
          h2("Update Primer Box"),
          fluidPage(fluidRow(
            box(width = 6,
            h3("Step One: Load the Scan File"),
            fileInput("updaterinput", "Scanned CSV",
                      multiple = F,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            checkboxInput("csvheader","Header?",F),
            dataTableOutput("updateroutput")
            ),
            box(width = 6,
                h3("Master File Contents"),
            dataTableOutput("masteroutput"))
          ),
          box(width = 6,
            h3("Step Two: Select the box to update"),
            textInput("rack_in","Rack",value = "I"),
            textInput("col_in","Column",value = "1"),
            textInput("box_in","Box",value = "A"),
            dataTableOutput("updaterlocs")
          ),
          box(width = 12,
              h3("Step Three: Updated DB / Save"),
              DT::dataTableOutput("mergedb"),
              downloadButton("saveupdateddb","Download Updated DB")
          )


  ))

      )))

server <- function(input, output) {

  primer_db <- reactive({
    primer.table <- read.csv(file = input$dbinput$datapath,header = T,quote = "",colClasses = "character",stringsAsFactors = F)
     })

  master_u.db <- reactive({
    master_u.table <- read.csv(file = input$masterinput$datapath,header = T,quote = "",colClasses = "character",stringsAsFactors = F)
  })

  refmast.db <- reactive({
    master_u2.table <- read.csv(file = input$masterinput$datapath,header = T,quote = "",colClasses = "character")
  })
  refID.db <- reactive({
    refid.table <- read.csv(file = input$refinput$datapath,header = T,quote = "",colClasses = "character")
  })

  updaterefid <- reactive({
    upref <- refmast.db()
    ID_list <- list("refs" = refID.db()$Reference,
                      "manufs" = refID.db()$Manufacturing.ID)
    ID_seq_length <- seq(1:length(ID_list$refs))

    refcol <- grep(pattern = "Reference",x=names(upref),value = F,fixed = T)
    manucol <- grep(pattern = "Manufacturing.ID",x=names(upref),value = F,fixed = T)

    for(i in ID_seq_length){
      refid.tmp <- ID_list$refs[i]
      refid.man <- ID_list$manufs[i]
      #print(reorg.master2[reorg.master2$Reference == refid.tmp,2])
      if(length(upref[upref$Reference == refid.tmp,refcol]) > 0 && upref[upref$Reference == refid.tmp,manucol] == "blank"){
        upref[upref$Reference == refid.tmp,manucol] <- refid.man
      }
    }

    upref

  })

  output$refidmerged <- DT::renderDataTable(updaterefid(),extensions = 'Responsive')

  output$refidmerged_dl <- downloadHandler(filename = function(){
    paste(Sys.Date(),"_Master_DB",".csv",sep = "")
  },
  content = function(file) {
    write.csv(updaterefid(), file,quote = F,row.names = F,col.names = T)
  },contentType = "text/csv")

  coa.db <- reactive({
    coa.table <- read.csv(file = input$coaupdateinput$datapath,header = T,quote = "",colClasses = "character")
    coa.table$Sequence <- gsub(pattern = " ",replacement = "",x = coa.table$Sequence)
    coa.table
  })

  updatedmaster <- reactive({
    req(input$masterinput)
    req(input$coaupdateinput)
    updatedmasterset <- rbind(master_u.db(),coa.db())
  })

  output$umout <- DT::renderDataTable(updatedmaster())
  output$masterupdateoutput <- DT::renderDataTable(master_u.db())

  output$coaupdateoutput <- DT::renderDataTable(coa.db())

  master_db <- reactive({
    master.table <- read.csv(file = input$masterinput$datapath,header = T,quote = "",colClasses = "character",stringsAsFactors = F)
  })

  update_list <- reactive({
    update.df<-read.csv(file = input$updaterinput$datapath,quote = "",header = input$csvheader,colClasses = "character")
    colnames(update.df) <- c("Manufacturing.ID","Code","Scanned")
    #update.df$Manufacturing.ID <- as.character(update.df$Manufacturing.ID)
    update.df$Manufacturing.ID <- gsub("^0","",update.df$Manufacturing.ID)
    update.df %>% select(-Code) %>% arrange(Scanned) -> update.df
    update.df
  })

  add_locations <- reactive({
    req(update_list())
    locations_annotated.df <- cbind(
    update_list(),
    Location = boxlocs()$Location)
    locations_annotated.df
  })

  scanned_box_master_annotate <- reactive({
    box_loc_annotated <- as.data.table(add_locations())
    master.df <- as.data.table(master_db())
    merged.df <- merge(x = box_loc_annotated,y=master.df,by = "Manufacturing.ID",all.x = T)
    pna <- names(primer_db())
    merged.df <- merged.df %>%
      select(pna)
    merged.df

  })

  mergedb.action <- reactive({
    primer.db <- as.data.table(primer_db())
    scanned.box <- as.data.table(scanned_box_master_annotate())
    setkey(primer.db,Location)
    primer.db[scanned.box$Location,] <- scanned.box
    primer.db
  })

  output$mergedb <- DT::renderDataTable({
    req(input$updaterinput)
    req(input$masterinput)
    mergedb.action()

    })

  output$saveupdateddb <- downloadHandler(filename = function(){
    paste(Sys.Date(),"_primers_DB.csv",sep = "")
  },
  content = function(file) {
    write.csv(mergedb.action(), file,quote = F,row.names = F,col.names = F)
  },contentType = "text/csv")

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
      filter(is.na(Reference) == FALSE) -> p.good
    p.good.n <- nrow(p.good)

    primer_db() %>%
      filter(is.na(Manufacturing.ID) == FALSE) %>%
      filter(Manufacturing.ID != "EMPTY") %>%
      filter(Manufacturing.ID != "MANUAL") %>%
      filter(is.na(Reference)) -> p.errors
    primer_db() %>%
      filter(Manufacturing.ID == "MANUAL") -> p.man

    p.man.n <- nrow(p.man)

    p.errors.simple <- p.errors %>%
      select(Location,Manufacturing.ID) %>%
      mutate(Reference = "     ")
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
         p.empty.n = p.empty.n,
         p.man.n = p.man.n)
  })

  ptab <- reactive({
    ptab.df <- primer_db()
    ptab.df$Sequence.Name<-as.factor(ptab.df$Sequence.Name)
    ptab.df
  })
  output$primertable <- DT::renderDataTable(
    ptab(), filter = c('top'),extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))


  output$gampcheck <- renderPrint({
    req(input$fwprimer)
    req(input$rvprimer)
    genome.link <- paste0(input$amporg,"&wp_target=genome&wp_f=",input$fwprimer,"&wp_r=",input$rvprimer,"&Submit=submit&wp_size=",input$ampprod,"&wp_perfect=",input$ampminperf,"&wp_good=",input$ampmingood,"&boolshad.wp_flipReverse=0")

    genome <- read_html(genome.link) %>%
      html_nodes("pre") %>%
      html_text

    g.amp<-gsub(pattern = "[>]",x = genome,replacement = "")

    #g.amp<-gsub(pattern = "[\n]",x = genome,replacement = " ")

    tags$p(g.amp)
  })

  output$mampcheck <- renderPrint({
    req(input$fwprimer)
    req(input$rvprimer)
    mrna.link <- paste0(input$amporg,"&wp_target=mm10KgSeq9&wp_f=",input$fwprimer,"&wp_r=",input$rvprimer,"&Submit=submit&wp_size=",input$ampprod,"&wp_perfect=",input$ampminperf,"&wp_good=",input$ampmingood,"&boolshad.wp_flipReverse=0")

    mrna <- read_html(mrna.link) %>%
      html_nodes("pre") %>%
      html_text

    m.amp<-gsub(pattern = ">",x = mrna,replacement = "\n")

    #m.amp<-gsub(pattern = "[\n]",x = mrna,replacement = "\n")

    tags$p(m.amp)
  })

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
    req(input$dbinput)
    valueBox(value = primer_stats()$primer.n,subtitle = "Total Locations in DB",icon = icon("database"),color = "purple")
  })

  output$primergood <- renderValueBox({
    req(input$dbinput)
    valueBox(value = primer_stats()$p.good.n,subtitle = "Primers with complete information",icon = icon("thumbs-up"),color = "green")
  })

  output$primerempty <- renderValueBox({
    req(input$dbinput)
    valueBox(value = primer_stats()$p.empty.n,subtitle = "free spaces",icon = icon("thumbs-up"),color = "blue")
  })

  output$primermanual <- renderValueBox({
    req(input$dbinput)
    valueBox(value = primer_stats()$p.man.n,subtitle = "Primers labeled 'manual' (likely old)",icon = icon("warning"),color = "red")
  })

  output$primererrors <- renderValueBox({
    req(input$dbinput)
    valueBox(value = primer_stats()$p.errors.n,subtitle = "Locations missing Ref Number",icon = icon("warning"),color = "red")
  })

  output$updateroutput <- DT::renderDataTable({
    req(input$updaterinput)
    update_list()
    })

  output$masteroutput <- DT::renderDataTable({
    req(input$masterinput)
    master_db()
  })

  output$updaterlocs <- DT::renderDataTable({
    req(input$updaterinput)
    add_locations()
  })

  output$saveupdatedmaster <- downloadHandler(filename = function(){
    paste(Sys.Date(),"_Master_DB",".csv",sep = "")
  },
  content = function(file) {
    write.csv(updatedmaster(), file,quote = F,row.names = F,col.names = T)
  },contentType = "text/csv")

  }

shinyApp(ui, server)
