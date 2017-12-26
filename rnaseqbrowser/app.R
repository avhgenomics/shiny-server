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
library(enrichR)



ui <- dashboardPage(
  dashboardHeader(title = "RNA-Seq Browser"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Home",tabName = "home",icon = icon("home")),
    menuItem("Visualization",tabName = "visualization",icon = icon("home"))
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidPage(fluidRow(
                h2("Browser Dashboard"),
                box(
                  title = "Load RNA-seq csv",
                  status = "primary",
                  fileInput("dbinput", "Open Database CSV",
                            multiple = F,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")))),
                box(title = "File Summary Statistics",
                    status = "warning",
                    valueBoxOutput("summarystats",width = 10))
                )),
      tabItem(tabName = "visualization",
      fluidPage(fluidRow(
        h2("Plots and viz"),
        box(title = "Volcano Plot",
            status = "primary",
            collapsible = T,
            fluidRow(box(numericInput(inputId = "vp_pvalthresh",label = "P-value significance threshold",value = 0.05,step = 0.001),
                     numericInput(inputId = "vp_fcthresh",label = "log2 Foldchange threshold",value = 0.5,step = 0.01))),
          plotOutput("volcanoplot")
          ),
        
        box(
          title = "Full dataset",
          status = "primary",
          collapsible = T,
          collapsed = T,
          DT::dataTableOutput("fulltable")
        ),
        box(
          title = "Threshold Filtered Genes",
          status = "primary",
          collapsible = T,
          collapsed = T,
          DT::dataTableOutput("threshtable")
        )
        
        
        ),
          box(width = 12,
              title = "Enrichr",
              collapsible = T,
              collapsed = T,
              fluidRow(
                box(width = 8,
                    title = "Enrichr Options",
                    collapsible = T,
                    collapsed = F,
                    selectizeInput(inputId = "enrichrdbs",label = "Select Databases",choices = listEnrichrDbs(),multiple = F)
                  ),
                box(width = 8,
                    title = "Enrichr Results",
                    collapsible = T,
                    collapsed = T,
                    DT::dataTableOutput("enrichrthresh")
                ),
          box(width = 4,
              title = "Enrichr Plot",
              collapsible = T,
              collapsed = F,
              selectizeInput(inputId = "enrichplotx","X axis",choices = "placeholder"),
              selectizeInput(inputId = "enrichploty","y axis",choices = "placeholder"),
              plotOutput("enrichrplot")),
          box(width = 6,
              title = "Filtered Volcano",
              collapsible = T,
              collapsed = F,
              plotOutput("filteredvolcano")
              )
          )
        )))
      
      
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  rnaseq.df <- reactive({
    rnaseq<-read.csv(file = input$dbinput$datapath,header = T,quote = "",as.is = F)
  })
  
  summarystats.df <- reactive({
    genes.n <- nrow(rnaseq.df())
    genes.n
    list("genes.n" = genes.n)
  })
  
  
  output$summarystats <- renderValueBox({
    req(rnaseq.df())
    valueBox(summarystats.df()$genes.n,subtitle = "total gene records",color = "red")})
  
  output$volcano.df <- DT::renderDataTable(rnaseq.df())
  
   output$volcanoplot <- renderPlot({
     req(rnaseq.df())
     plot.volcano <- ggplot(rnaseq.df(),aes(x = log2FoldChange,y = (-log(padj,10))))
     plot.volcano +
       geom_point(data=rnaseq.df(),color = "#000000")+
       geom_hline(yintercept = -log(input$vp_pvalthresh,10))+
       geom_vline(xintercept = input$vp_fcthresh)+
       geom_vline(xintercept = -input$vp_fcthresh)+
       geom_point(data = threshold_genes(),aes(x = threshold_genes()$log2FoldChange,y = (-log(threshold_genes()$padj,10))),color = "#FF0000")
     
   })
   
   threshold_genes <- reactive({
     thresholded_genes.df <- rnaseq.df() %>%
       filter(!between(log2FoldChange,-input$vp_fcthresh,input$vp_fcthresh) & padj <= input$vp_pvalthresh)
   })
   
   output$threshtable <- DT::renderDataTable(
     threshold_genes(),filter = 'top',extensions = 'Responsive'
   )
   
   output$fulltable <- DT::renderDataTable(
     rnaseq.df(),filter = 'top',extensions = 'Responsive'
   )
   
   enrichment <- reactive({
     db <- input$enrichrdbs
     gene.vector <- as.vector(threshold_genes()$symbol)
     enrichthresh.results <- data.frame(enrichr(gene.vector,db))
     enrichthresh.results
   })
   
   output$enrichrthresh <- DT::renderDataTable(
    enrichment(),extensions = 'Responsive'
    )
   
   output$filteredvolcano <- renderPlot({
     
     selected_path_genes <- tolower(unlist(strsplit(enrichment()$Reactome_2016.Genes[input$enrichrplot_rows_selected],";")))
     
    
     plot.fvolc <- ggplot(threshold_genes(),aes(x = log2FoldChange,y = (-log(padj,10))))
     
     plot.fvolc +
       geom_point()
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

