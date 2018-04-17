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
library(random)
library(stringr)
library(ReactomePA)
library(org.Mm.eg.db)
library(igraph)


ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "RNA-Seq Browser"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Home",tabName = "home",icon = icon("home")),
    menuItem("Visualization",tabName = "visualization",icon = icon("home")),
    menuItem(text = "BioKIT Tools",
             menuSubItem(text = "Biotools",href = "http://avh.science:3838/biotools/"),
             menuSubItem(text = "Buffer Calcs",href = "http://avh.science:3838/buffer_calculator/"),
             menuSubItem(text = "Gel Analysis",href = "http://avh.science:3838/gel_densitometry/"),
             menuSubItem(text = "Primer Dashboard",href = "http://avh.science:3838/Primer_dash/"),
             menuSubItem(text = "RNAseq Browser",href = "http://avh.science:3838/rnaseqbrowser/")
    )
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
                                       ".csv")),
                  textInput("sessID","Generate a session ID",value = "12345"))),
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
            fluidRow(box(collapsible = T,title = "options",
              numericInput(inputId = "vp_pvalthresh",label = "P-value significance threshold",value = 0.05,step = 0.001),
                     numericInput(inputId = "vp_fcthresh",label = "log2 Foldchange threshold",value = 0.5,step = 0.01))),
          plotOutput("volcanoplot"),
          downloadButton("volcanodl","Download Volcano Plot")
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
          DT::dataTableOutput("threshtable"),
          downloadButton("threshtable.file","Download Filtered Genes")
        )
        
        
        ),
                fluidRow(
                  box(width = 6,
                    title = "Enrichr Options",
                    collapsible = T,
                    collapsed = F,
                    selectizeInput(inputId = "enrichrdbs",label = "Select Databases",choices = listEnrichrDbs(),multiple = F),
                    actionButton("enrichraction",label = "Load / Update Enrichr Results")
                  ),
                  box(width = 6,
                      title = "Reactome Options",
                      collapsible = T,
                      collapsed = F,
                      selectInput("reactomeorganism","Select Organism",choices = list("Mouse" = "mouse","Human" = "human"),selected = "mouse",multiple = F),
                      selectInput("reactomereadable","GeneIDs or Names?",choices = list("GeneID" = FALSE,"Names" = TRUE),selected = TRUE,multiple = F),
                      selectInput("reactomepadj","p-val Adjustment Method",choices = list("holm" = "holm",
                                                                                          "hochberg" = "hochberg",
                                                                                          "hommel" = "hommel",
                                                                                          "bonferroni" = "bonferroni",
                                                                                          "BH" = "BH",
                                                                                          "BY" = "BY",
                                                                                          "FDR" = "fdr",
                                                                                          "none" = "none"),selected = "fdr",multiple = F),
                      h4("Cutoffs"),
                      fluidRow(numericInput("reactomepval","p-value",value = 0.05,step = 0.01,width = "45%"),
                               numericInput("reactomeqval","q-value",value = 0.2,step = 0.01,width = "45%")),
                      h4("Set size parameters"),
                      fluidRow(numericInput("reactomemingss","Min Genes",value = 10,step = 1,width = "45%"),
                               numericInput("reactomemaxgss","Max Genes",value = 500,step = 1,width = "45%")),
                      actionButton("reactomeaction",label = "Load / Update Reactome Results")
                  )
                  ),
                box(width = 12,
                    title = "Enrichr Results",
                    collapsible = T,
                    collapsed = F,
                    DT::dataTableOutput("enrichrthresh"),
                    downloadButton("enrichr.file",label = "Download Enrichr Results")
                ),
          box(width = 6,
              title = "Filtered Volcano",
              collapsible = T,
              collapsed = F,
              plotOutput("filteredvolcano"),
              downloadButton("downloadfilteredvolc","Download Filtered Volcano Plot"),
              verbatimTextOutput("filtervolctext")
              ),
        box(width=12,
            title = "Pathway Enrichment",
            collapsible = T,
            DT::dataTableOutput("reactomeEPT"),
            downloadButton("reactome.file",label = "Download Reactome Results")
        ),
        fluidRow(
        box(width=6,
            title = "Enrichment Map",
            collapsible = T,
            plotOutput("reactomeEM")
            
        ),
        box(width=6,
            title = "CNET Plot",
            collapsible = T,
            plotOutput("reactomeCNET")
        )))
              )
      
      
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  idn <- reactive({
    input$sessID
  })
  
  rnaseq.df <- reactive({
    rnaseq<-read.csv(file = input$dbinput$datapath,header = T,quote = "",as.is = F)
  })
  
  summarystats.df <- reactive({
    req(input$dbinput)
    genes.n <- nrow(rnaseq.df())
    genes.n
    list("genes.n" = genes.n)
  })
  
  
  output$summarystats <- renderValueBox({
    req(rnaseq.df())
    valueBox(summarystats.df()$genes.n,subtitle = "total gene records",color = "red")})
  
  output$volcano.df <- DT::renderDataTable(rnaseq.df())
  
  firstvolc <- reactive({
    req(rnaseq.df())
    plot.volcano <- ggplot(rnaseq.df(),aes(x = log2FoldChange,y = (-log(padj,10))))
    base_plot<-plot.volcano +
      geom_point(data=rnaseq.df(),color = "#000000")+
      geom_hline(yintercept = -log(input$vp_pvalthresh,10))+
      geom_vline(xintercept = input$vp_fcthresh)+
      geom_vline(xintercept = -input$vp_fcthresh)+
      xlab(label = "Log2 Fold Change")+
      ylab(label = "-log10 Adjusted P-val")+
      geom_point(data = threshold_genes(),aes(x = threshold_genes()$log2FoldChange,y = (-log(threshold_genes()$padj,10))),color = "#FF0000")
    
    base_plot
  })
  
   output$volcanoplot <- renderPlot({
     firstvolc()
   })
   
   output$volcanodl <- downloadHandler(filename = function(){
     paste("ID_",idn(),"_filtered_padj_",input$vp_pvalthresh,"_l2fc_",input$vp_fcthresh,"_",Sys.Date(),".pdf",sep = "")
   },
   content = function(file) {
     ggsave(plot = firstvolc(),filename = file,device = "pdf",dpi = 600)
   })
   
   output$downloadfilteredvolc <- downloadHandler(filename = function(){
     paste("ID_",idn(),"_filtered_padj_",input$vp_pvalthresh,"_l2fc_",input$vp_fcthresh,"_",Sys.Date(),".pdf",sep = "")
   },
   content = function(file) {
     ggsave(plot = volcanocode(),filename = file,device = "pdf",dpi = 600)
   })
   
   threshold_genes <- reactive({
     thresholded_genes.df <- rnaseq.df() %>%
       filter(!between(log2FoldChange,-input$vp_fcthresh,input$vp_fcthresh) & padj <= input$vp_pvalthresh)
   })
   
   output$threshtable <- DT::renderDataTable(
     threshold_genes(),filter = 'top',extensions = 'Responsive'
   )
   output$threshtable.file <- downloadHandler(filename = function(){
     paste("ID_",idn(),"_filtered_padj_",input$vp_pvalthresh,"_l2fc_",input$vp_fcthresh,"_",Sys.Date(),".csv",sep = "")
   },
   content = function(file) {
     write.csv(threshold_genes(), file,quote = F,row.names = F,col.names = F)
   },contentType = "text/csv")
   
   
   output$fulltable <- DT::renderDataTable(
     rnaseq.df(),filter = 'top',extensions = 'Responsive'
   )
   
   enrichment <- eventReactive(input$enrichraction,{
     db <- input$enrichrdbs
     gene.vector <- as.vector(threshold_genes()$symbol)
     enrichthresh.results <- data.frame(enrichr(gene.vector,db))
     enrichthresh.results
   })
   
   output$enrichrthresh <- DT::renderDataTable(
    enrichment(),extensions = 'Responsive'
    )
   output$enrichr.file <- downloadHandler(filename = function(){
     paste("ID_",idn(),"_enrichr_padj_",input$vp_pvalthresh,"_l2fc_",input$vp_fcthresh,"_",Sys.Date(),".csv",sep = "")
   },
   content = function(file) {
     write.csv(enrichment(), file,quote = F,row.names = F,col.names = F)
   },contentType = "text/csv")
   
   output$reactome.file <- downloadHandler(filename = function(){
     paste("ID_",idn(),"_reactome_padj_",input$vp_pvalthresh,"_l2fc_",input$vp_fcthresh,"_",Sys.Date(),".csv",sep = "")
   },
   content = function(file) {
     write.csv(reactomeEPdf(), file,quote = F,row.names = F,col.names = F)
   },contentType = "text/csv")
   
   
   filtered.genes <- reactive({
     filtered.genes <- threshold_genes() %>%
       filter(symbol %in% selectedgenes())
   })
   
   filtered.genesreact <- reactive({
     filtered.genesreact <- threshold_genes() %>%
       filter(symbol %in% selectedreactome())
   })
   
   volcanocode <- reactive({
     req(filtered.genes())
     req(filtered.genesreact())
     plot.fvolc <- ggplot(threshold_genes(),aes(x = log2FoldChange,y = (-log(padj,10))))
     plotted <- plot.fvolc + geom_point()
     plotted<-plotted +
      xlab(label = "Log2 Fold Change")+
      ylab(label = "-log10 Adjusted P-val")+
      geom_point(data = filtered.genes(),color = "red",alpha = 0.6)+
      geom_point(data = filtered.genesreact(),color = "green",alpha = 0.6)
     
     plotted
   })
   
   
   output$filteredvolcano <- renderPlot({
     volcanocode()
     
   })
   
   selectedgenes <- reactive({
     genecol <- grep("*.Genes",names(enrichment()),value = T)
     s = input$enrichrthresh_rows_selected
     geneset<-enrichment()[s,genecol]
     geneset <- str_to_title(geneset)
     geneset.vector <- strsplit(x = geneset,split = ";",fixed = T)
     geneset.vector.ul <- unlist(geneset.vector)
     geneset.vector.ul
     
   })
   
   selectedreactome <- reactive({
     genecol2 <- grep("geneID",names(reactomeEPdf()),value = T,fixed = T)
     r = input$reactomeEPT_rows_selected
     geneset2<-reactomeEPdf()[r,genecol2]
     geneset2 <- str_to_title(geneset2)
     geneset.vector2 <- strsplit(x = geneset2,split = "/",fixed = T)
     geneset.vector.ul2 <- unlist(geneset.vector2)
     geneset.vector.ul2
     
   })
   
   difgenevector <- reactive({
     req(threshold_genes())
    difgenes <- threshold_genes()$geneid
    difgenes <- unlist(difgenes)
   })
   
   reactome_base <- eventReactive(input$reactomeaction,{
    reactomeEP <- enrichPathway(gene = difgenevector(),organism = input$reactomeorganism,readable = input$reactomereadable,pvalueCutoff = input$reactomepval,qvalueCutoff = input$reactomeqval,pAdjustMethod = input$reactomepadj,minGSSize = input$reactomemingss,maxGSSize = input$reactomemaxgss)
    reactomeEP
     
   })
   
   reactomeEPdf <- reactive({
     as.data.frame(reactome_base())
   })
   
   output$reactomeEPT <- DT::renderDataTable(reactomeEPdf(),extensions = 'Responsive')
   
   output$reactomeEM <- renderPlot({
     enrichMap(reactome_base(), layout=igraph::layout.fruchterman.reingold, vertex.label.cex = 1,n = 50)
   })
   output$reactomeCNET <- renderPlot({
     cnetplot(reactome_base(),categorySize = "pvalue",foldChange = difgenevector())
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

