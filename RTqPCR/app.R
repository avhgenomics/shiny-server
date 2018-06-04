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


# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
  dashboardHeader(title = "RT-qPCR Visualizer"),
  dashboardSidebar(),
  dashboardBody(fluidRow(box(title = "Inputs",width = 3,collapsible = T,
                    fileInput(inputId = "ct_csv",label = "Load Data (CSV)",multiple = F,placeholder = "data.csv"),
                    checkboxInput(inputId = "techrepscheck",label = "Data has technical replicates:",value = F),
                    uiOutput("techrepsui"),
                    uiOutput("mCtUI"),
                    uiOutput("dCtUI"),
                    uiOutput("ddCtUI"),
                    uiOutput("ddCtUI2")),
                box(title = "dataworking",width = 9,collapsible = T,
                    selectInput(inputId = "viewtable",label = "Select Table to view",choices = c("Raw","Tech-averaged","Mean Ct","dCt","ddCt"),selected = "Raw",multiple = F),
                    DT::dataTableOutput(outputId = "tableswitch"))),
                fluidRow(box(title = "Plot Options",width = 3,
                    selectInput(inputId = "plot_dataset",label = "Select Dataset",choices = c("Raw","Tech-averaged","Mean Ct","dCt","ddCt"),selected = "ddCt",multiple = F),
                    uiOutput("plot_settings")),
                box(title = "Expression Plots",width = 9,
                    plotOutput("expression_plot")))
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   #Dataframes
   raw.df <- reactive({
     read.csv(file = input$ct_csv$datapath,header = T,sep = ",",quote = "",stringsAsFactors = F)


   })

   work.df <- reactive({
    req(raw.df())
     if(input$techrepscheck == T){
       if(!is.null(input$techrepsgrouping)){
       raw.df() %>%
         group_by_at(.vars = input$techrepsgrouping) %>%
         mutate(Ct = mean(Ct)) %>%
         distinct(Ct,.keep_all = T)} else {
           raw.df()
         }
     } else {
       raw.df()
     }

   })


   mCt.df <- reactive({
     req(!is.null(input$mCtgrouping))
      work.df() %>%
        group_by_at(.vars = input$mCtgrouping) %>%
        mutate(sd.Ct = sd(Ct),
               n = n(),
               Ct = mean(Ct)) %>%
        distinct(Ct,.keep_all = T) %>%
        select(-Replicate)
    })

   dCt.df <- reactive({
     req(!is.null(input$dCtgrouping))
     mCt.df() %>%
       group_by_at(.vars = input$dCtgrouping) %>%
       mutate(dCt = Ct - Ct[Gene == input$housekeeper],
              dSD = sqrt(((sd.Ct^2)+(sd.Ct[Gene == input$housekeeper]^2))/(n - 1)))

   })

   ddCt.df <- reactive({
     req(!is.null(input$ddCtgrouping))
     dCt.df() %>%
       group_by_at(.vars = input$ddCtgrouping) %>%
       mutate(ddCt = dCt - dCt[Condition == input$control_condition])
   })

   #UIs
   output$techrepsui <- renderUI({
     req(raw.df())
     if(input$techrepscheck){
     tagList(selectInput(inputId = "techrepsgrouping",label = "Group tech reps by:",choices = colnames(raw.df()),multiple = T))}
   })

   output$mCtUI <- renderUI({
     req(!is.null(work.df()))
     tagList(selectInput(inputId = "mCtgrouping",label = "Group mean Ct values by:",choices = colnames(work.df()),multiple = T))
   })

   output$dCtUI <- renderUI({
     req(!is.null(mCt.df()))
     tagList(selectInput(inputId = "dCtgrouping",label = "group dCt values by:",choices = colnames(mCt.df()),multiple = T),
             selectInput(inputId = "housekeeper",label = "Select Housekeeping Gene",choices = unique(mCt.df()$Gene),multiple = F))
   })

   output$ddCtUI <- renderUI({
     req(!is.null(dCt.df()))
     tagList(selectInput(inputId = "ddCtgrouping",label = "group ddCt values by:",choices = colnames(dCt.df()),multiple = T),
             selectInput(inputId = "control_col",label = "Select Control Condition using:",choices = colnames(dCt.df()),multiple = F))
   })

   output$ddCtUI2 <- renderUI({
     req(!is.null(input$control_col))
     tagList(selectInput(inputId = "control_condition",label = "Base Condition:",choices = unique(dCt.df()[,input$control_col]),multiple = F))
   })

   output$plot_settings <- renderUI({
     req(ddCt.df())
     data.df <- switch(input$plot_dataset,
                       "Raw" = raw.df(),
                       "Tech-averaged" = work.df(),
                       "Mean Ct" = mCt.df(),
                       "dCt" = dCt.df(),
                       "ddCt" = ddCt.df())
     tagList(selectInput(inputId = "xaxis",label = "Select X axis",choices = colnames(data.df),multiple = F),
             selectInput(inputId = "yaxis",label = "Select Y axis",choices = colnames(data.df),multiple = F),
             selectInput(inputId = "plot_gene",label = "Select Gene",choices = unique(ddCt.df()$Gene),multiple = F))
   })

   #tables

   tableselected <- reactive({
     req(raw.df())
     switch(input$viewtable,
            "Raw" = raw.df(),
            "Tech-averaged" = work.df(),
            "Mean Ct" = mCt.df(),
            "dCt" = dCt.df(),
            "ddCt" = ddCt.df())
   })

   output$tableswitch <- DT::renderDataTable(tableselected(),extensions = 'Responsive')


   #Plotting

   data_plot.df <- reactive({
     req(ddCt.df())
     ddCt.df() %>%
       filter(Gene == input$plot_gene)
   })

   output$expression_plot <- renderPlot({
     req(data_plot.df())
     if(input$plot_dataset == "ddCt"){
       plot.data <- ggplot(data_plot.df(),aes(x = Condition, y = -ddCt))+
         geom_errorbar(aes(ymin = -ddCt-dSD,ymax = -ddCt+dSD),width = 0.5)
     }
     if(input$plot_dataset == "dCt"){
       plot.data <- ggplot(data_plot.df(),aes(x = Condition, y = -dCt))+
         geom_errorbar(aes(ymin = -dCt-dSD,ymax = -dCt+dSD),width = 0.5)
     }
     plot.data +
       geom_col(position = position_dodge())+
       theme(aspect.ratio = 4/3)
   })


}

# Run the application
shinyApp(ui = ui, server = server)
