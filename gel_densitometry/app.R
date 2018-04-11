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
library(zoo)
library(jpeg)
library(reshape2)

# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
  dashboardHeader(title = "Image Analyzer Test"),
  dashboardSidebar(),
  dashboardBody(
    box(width = 12,collapsible = T,
      title = "Gel Information",
    box(width = 12,
        title = "Load Image",
        fileInput(inputId = "imagefile",label = "image",multiple = F,placeholder = "load jpg")),
    box(width = 12,
      title = "Image array",
      DT::dataTableOutput("gel_df")
    )
    ),
    box(width = 4,
        collapsible = T,
      title = "Options",
        textInput(inputId = "lanes",label = "Label Lanes separated by ,"),
        textInput(inputId = "lane_width",label = "Lane Start locations separated by ,"),
        numericInput(inputId = "lane_extend",label = "Extend Lanes by X Pixels",value = 10,min = 1,step = 1),
        numericInput(inputId = "h_start",label = "vertical cutoff (top)",value = 1,min = 1,step = 50),
        numericInput(inputId = "h_end",label = "vertical cutoff (bottom)",value = 100,min = 1,step = 50),
        numericInput(inputId = "bkg",label = "background value",value = 0,min = 0,step = 1),
        actionButton(inputId = "set_options",label = "Click after options are set")),
    box(width = 8,
        collapsible = T,
        title = "Gel Image",
        plotOutput("gel_image",hover = hoverOpts(id ="gel_hover")),
        textOutput("gel_hover_coords")
        ),
    box(title = "Ladder Stats",
        width = 12,
        collapsible = T,
        fluidRow(
          box(width = 4,
          title = "parameters",
          numericInput(inputId = "maxima_ladder_thresh",label = "Ladder Maxima Threshold",value = 3,min = 1,step = 1),
          numericInput(inputId = "signal_ladder_thresh",label = "Ladder Signal Threshold",value = 0.1,min = 0,step = 0.1),
          numericInput(inputId = "signal_ladder_smooth",label = "Ladder Signal Smooth",value = 5,min = 0,step = 1),
          selectInput(inputId = "ladderID",label = "What Ladder?",choices = list("100 bp (G210a)" = "g210a"),selected = "g210a",multiple = F)
        ),
        box(width = 8,
            title = "Ladder Dataframe",
            DT::dataTableOutput("gel_ladder_df")
        )
        ),
        box(width = 12,
            box(width = 6,
            title = "Ladder Signal",
            plotOutput("ladder_plot")
            ),
            box(width = 6,
                title = "BP Size Calcs",
                plotOutput("bp_plot"),
                valueBoxOutput(outputId = "model_stats"),
                valueBoxOutput(outputId = "model_eq")
            )
        )
      )
    ,
    box(width = 12,
        title = "Lane Densitometry",
        collapsible = T,
      box(width = 3,
          title = "Signal Options",
          numericInput(inputId = "gel_ladder_label_y",label = "What Y to place ladder labels?",value = (-0.1),step = 0.1),
          numericInput(inputId = "pred_pixel",label = "Predict Band Size",value = 500,step = 10),
          checkboxInput(inputId = "area_check",label = "Calculate Area Stats?",value = F),
          uiOutput("area_opts"),
          uiOutput("signal_opts")
      ),
      box(width = 9,
          title = "Signal Plot",
          plotOutput("gel_plot"),
          DT::dataTableOutput("area_dt")
    )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #functions
  inflect <- function(x, threshold = 1){
    up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
    down <-  sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
    a    <- cbind(x,up,down)
    list(minima = which(apply(a, 1, min) == a[,1]), maxima = which(apply(a, 1, max) == a[,1]))
  }
  #end functions
  
   gel <- reactive({
     gel <- jpeg::readJPEG(input$imagefile$datapath)
     #gel <- keras::image_to_array(keras::image_load(input$imagefile$datapath,grayscale = T))
     gel
   })
   
   gel_opts <- eventReactive(input$set_options,{
     lane_labels <- str_split(string = input$lanes,pattern = ",")[[1]]
     lane_labels <- tolower(lane_labels)
     lane_width <- as.numeric(str_split(string = input$lane_width,pattern = ",")[[1]])
     lanes_n <- seq(1:length(lane_labels))
     master.list <- list()
     
     for(i in lanes_n){
       w1 = lane_width[i]
       w2 = w1+input$lane_extend
       gel.label = lane_labels[i]
       master.list[[gel.label]] <- rowMeans(gel()[input$h_start:input$h_end,w1:w2])
     }
     #print(master.list)
     df <- as.data.frame(master.list)
     df <- reshape2::melt(df)
     df$normalized_signal <- df$value - input$bkg
     df$pixel_y = seq(from = input$h_start, to = input$h_end)
     df
   })
   
   output$gel_image <- renderPlot({
     t.m <- melt(gel()[input$h_start:input$h_end,])
     plot.tm <- ggplot(t.m,aes(x = Var2,y=-Var1))
     
     plot.tm+geom_raster(aes(fill = value))+
       scale_fill_gradient(low = "#000000",high = "#ffffff")
   })
   
   output$gel_hover_coords <- renderText({
     if(!is.null(input$gel_hover)){
       hover = input$gel_hover$x
       paste("Lane X coord:",round(hover,0),"\n","pixel value:")
     }
   })
   
   samples_processed <- reactive({
     gel_opts() %>%
       filter(variable != "ladder")
   })
   
   gel_ladder <- reactive({
     gel_opts() %>%
       filter(variable == "ladder") %>%
     mutate(normalized_signal_smooth = rollmean(normalized_signal,k = input$signal_ladder_smooth,fill = normalized_signal[1]))
   })
   
   ladder_maxima <- reactive({
     lmaxima <- inflect(gel_ladder()$normalized_signal_smooth,threshold = input$maxima_ladder_thresh)
     ladder.peaks <- gel_ladder()[lmaxima$maxima,]
     ladder.peaks <- ladder.peaks[which(ladder.peaks$normalized_signal_smooth >= input$signal_ladder_thresh),]
     if(input$ladderID == "g210a"){
       ladder.peaks <- ladder.peaks[1:11,]
       #ladder.peaks$ladder_label <- c("1500","1000","900","800","700","600","500","400","300","200","100")
       ladder.peaks$ladder_label <- c(1500,1000,900,800,700,600,500,400,300,200,100)
       
     }
     ladder.peaks %>%
       filter(!is.na(variable))
   })
   
   output$gel_df <- DT::renderDataTable(samples_processed())
   output$gel_ladder_df <- DT::renderDataTable(ladder_maxima())
   output$ladder_plot <- renderPlot({
     plot.ladder <- ggplot(gel_ladder(),aes(x=pixel_y,y = normalized_signal_smooth, group = variable))
     plot.ladder+
       geom_line()+
       geom_vline(data = ladder_maxima(),xintercept = ladder_maxima()$pixel_y,alpha = 0.5)+
       geom_text(data = ladder_maxima(),aes(x = pixel_y,y = input$gel_ladder_label_y,label = ladder_maxima()$ladder_label,angle = 40))
   })
   
   bp_size_model <- reactive({
     model <- lm(ladder_label ~ pixel_y + I(pixel_y^2),data = ladder_maxima())
     model
   })
   
   bp_size_mline <- reactive({
     data.frame(ladder_label = predict(bp_size_model(),x = ladder_maxima()),
                pixel_y = ladder_maxima()$pixel_y)
   })
   
   bp_size_pred <- reactive({
     model <- lm(ladder_label ~ pixel_y + I(pixel_y^2),data = ladder_maxima())
     pred.df <- data.frame(pixel_y = input$pred_pixel)
     data.frame(ladder_label = predict(object = model,newdata = pred.df),
                pixel_y = input$pred_pixel)
   })
   
   output$bp_plot <- renderPlot({
     #ladder.df <- ladder_maxima()
     #ladder.df$ladder_label <- as.numeric(ladder.df$ladder_label)
     plot.ladder <- ggplot(ladder_maxima(),aes(x=pixel_y,y = ladder_label))
     plot.ladder+
       geom_point()+
       geom_line(data = bp_size_mline())+
       annotate("segment",x = min(ladder_maxima()$pixel_y),y = bp_size_pred()$ladder_label,xend = bp_size_pred()$pixel_y,yend = bp_size_pred()$ladder_label,color = "blue")+
       annotate(geom = "text",x = (bp_size_pred()$pixel_y+20),
                y = bp_size_pred()$ladder_label,
                label = paste(round(bp_size_pred()$ladder_label,2),"bp"),color = "blue")
       
     })
   
   output$model_eq <- renderValueBox({
     model <- lm(ladder_label ~ pixel_y + I(pixel_y^2),data = ladder_maxima())
     valueBox(subtitle = "Equation",value = tags$p(paste0("y = ",model$coefficients[1]," + ",model$coefficients[2],"X + ",model$coefficients[3],"X^2"),style = "font-size: 20%;"))
   })
   
   output$model_stats <- renderValueBox({
     model <- lm(ladder_label ~ pixel_y + I(pixel_y^2),data = ladder_maxima())
     valueBox(subtitle = "Adj. R-squared",value = round(summary(model)$adj.r.squared,6))
   })
   
   output$area_opts <- renderUI(
     if(input$area_check == T){
       tagList(
     numericInput(inputId = "area_start",label = "Select Starting Point",value = input$h_start,step = 10,min = input$h_start),
     numericInput(inputId = "area_end",label = "Select End Point",value = input$h_end,step = 10,max = input$h_end)
       )
     }
   )
   
   output$signal_opts <- renderUI(
     selectInput(inputId = "lane_select",label = "Select Lanes to view",choices = levels(samples_processed()$variable),multiple = T,selectize = T)
   )
   
   output$gel_plot <- renderPlot({
     gel_filtered <- samples_processed() %>%
       filter(variable %in% input$lane_select)
     plot.base <- ggplot(gel_filtered,aes(x = pixel_y,y = normalized_signal, group = variable,fill = variable,color = variable))
     plot.gel <- plot.base +
       geom_area(alpha = 0.2,position = "dodge")+
       geom_line()+
       geom_vline(data = ladder_maxima(),xintercept = ladder_maxima()$pixel_y,alpha = 0.1)+
       geom_vline(xintercept = input$pred_pixel,color = "blue",alpha = 0.3)+
       geom_text(data = ladder_maxima(),aes(x = pixel_y,y = input$gel_ladder_label_y,label = ladder_maxima()$ladder_label,angle = 40),color = "black")
     
     if(input$area_check == T){
       plot.gel <- plot.gel +
         geom_vline(xintercept = input$area_start,color = "black")+
         geom_vline(xintercept = input$area_end,color = "black")
     }
     
     plot.gel
     
   })
   
   area_stat_calcs <- reactive({
     if(input$area_check == T){
     samples_processed() %>%
         group_by(variable) %>%
         mutate(total_signal = round(sum(normalized_signal),4)) %>%
         filter(pixel_y >= input$area_start) %>%
         filter(pixel_y <= input$area_end) %>%
         mutate(area_signal = round(sum(normalized_signal),4),
                area_percent = paste0(round((area_signal / total_signal)*100,4),"%")) %>%
         distinct(variable,.keep_all = T) %>%
         select(variable,total_signal,area_signal,area_percent)
     }
   })
   
   output$area_dt <- DT::renderDataTable(area_stat_calcs())
}

# Run the application 
shinyApp(ui = ui, server = server)

