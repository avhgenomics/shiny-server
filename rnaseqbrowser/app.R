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
  
  dashboardSidebar(
    menuItem("Home",tabName = "home",icon = icon("home")),
    menuItem("Visualization",tabName = "visualization",icon = icon("home"))
    ),
  
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
                                       ".csv")))))),
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
        fluidRow(
          box(
            title = "Enrichr Results",
            collapsible = T,
            collapsed = T,
            DT::dataTableOutput("enrichrthresh")
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
     threshold_genes(),filter = 'top'
   )
   
   output$fulltable <- DT::renderDataTable(
     rnaseq.df(),filter = 'top'
   )
   
   enrichment <- reactive({
     db <- "Reactome_2016"
     gene.vector <- threshold_genes$gene.vector
     enrichthresh.results <- enrichr(gene.vector,db)
     enrichthresh.results
   })
   
   output$enrichrthresh <- DT::renderDataTable({
    enrichment()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

