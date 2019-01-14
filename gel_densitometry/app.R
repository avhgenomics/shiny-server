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
library(imager)
library(png)

# Define UI for application that draws a histogram
source("scripts/general_modules.r")

ui <- dashboardPage(
  dashboardHeader(title = "Gel Densitometry"),
  dashboardSidebar(),
  dashboardBody(
    fileInput(inputId = "gel_in",label = "Load gel JPG",multiple = F,buttonLabel = "Load"),
    imageOutput("gel_display"),
    numericInput(inputId = "DF_val",label = "DF to use",value = 10,min = 2,step = 1),
    numericInput(inputId = "lane_ext",label = "lane extension",value = 5,min = 0,step = 1),
    plotOutput("gel_lane_sum"),
    uiOutput("lane_selector"),
    uiOutput("ladder_selector"),
    plotOutput("lane_quantifications.graph"),
    uiOutput("lane_labels")
  )
)


server <- function(input,output){
  
  gel_im <- reactive({
    im <- imager::load.image(file = input$gel_in$datapath) %>%
      imager::grayscale()
    
    opt.height <- 400
    scale_factor <- opt.height / dim(im)[2]
    
    im %>%
      imager::imresize(scale = scale_factor)
    
  })
  
  
  output$gel_display <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, width = 400, height = 300)
    plot(gel_im())
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
gel_base.df <- reactive({
  as.data.frame(gel_im())
})


  gel.df <- reactive({
  
    gel_base.df() %>%
      dplyr::group_by(x) %>%
      dplyr::summarize(total.signal = sum(value))
  
  })
  
  spline.model <- reactive({
    sm <- smooth.spline(gel.df()$x,y = gel.df()$total.signal,df = input$DF_val)
    data.frame(predict(sm))
  })
  
  lane_maxima <- reactive({
    bioKIT::inflect(spline.model()$y)$maxima
  })
  
  
  
  lane_peaks <- reactive({
    
    used_labels <-paste0("lane_label",seq(1,length(lane_maxima())))
    
    
    
    label_IDs <- map(used_labels,function(x) input[[x]]) %>% unlist()
    
    lane_seq <- map(lane_maxima(),.f = function(x) seq(x-input$lane_ext,x+input$lane_ext)) %>% unlist()
    
    #lane_seq
    gel.df() %>%
      dplyr::filter(x %in% lane_seq) %>%
      mutate(`Lane ID` = as.character(rep(label_IDs,each = length(lane_seq)/length(label_IDs))))
  })
  
  
  lane_quantifications.df <- reactive({
    gel_base.df() %>%
      filter(x %in% lane_peaks()$x) -> df
    
    df$'Lane ID' <- lane_peaks()$'Lane ID'[match(df$x,lane_peaks()$x)]
    
    df %>%
      dplyr::group_by(`Lane ID`,y) %>%
      dplyr::summarize(mean.signal = mean(value),
                       sd.signal = sd(value)) %>%
      dplyr::ungroup()
    
  })
  
  output$lane_labels <- renderUI({
  
    output = tagList()
    
    for(i in seq_along(1:length(lane_maxima()))){
      output[[i]] = textInput(inputId = paste0("lane_label",i),label = "ID",value = i)
    }
    
    output
  })

  output$gel_lane_sum <- renderPlot({
    ggplot(gel.df(),aes(x = x,y = total.signal))+
      geom_line()+
      geom_line(data = spline.model(),aes(x = x,y = y),alpha = 0.1)+
      geom_ribbon(data = lane_peaks(),aes(x = x,ymin = min(gel.df()$total.signal),ymax = total.signal,group = `Lane ID`,fill = `Lane ID`,color = `Lane ID`),alpha = 0.3)+
      ggtitle("Lane Identification")+
      xlab("Horiz. Gel Coordinate")+
      ylab("Total Signal")
      
  })
  
  output$ladder_selector <- renderUI({
    tagList(selectInput(inputId = "ladder_select",label = "Select Ladder",choices = unique(lane_quantifications.df()$`Lane ID`),multiple = F,selectize = T),
            numericInput(inputId = "ladder_DF",label = "Ladder DF",value = 2,min = 2,step = 1))
  })
  
  output$lane_selector <- renderUI({
    selectInput(inputId = "lane_select",label = "Select Lanes",choices = unique(lane_quantifications.df()$`Lane ID`),multiple = T,selectize = T)
  })
  
  ladder.maxima <- reactive({
    lane_quantifications.df() %>%
      dplyr::filter(`Lane ID` == input$ladder_select) -> df
    
    ladder.model <- smooth.spline(x = df$y,y = df$mean.signal,df = input$ladder_DF)
    
    pred.ladder <- predict(ladder.model)
    
    bioKIT::inflect(pred.ladder$y)$maxima
    
  })
  
  

  
  
  output$lane_quantifications.graph <- renderPlot({
    
    lane_quantifications.df() %>%
      dplyr::filter(`Lane ID` %in% input$lane_select) -> df
    
    ggplot(df,aes(x = y,y = mean.signal,group = `Lane ID`,color = `Lane ID`))+
      geom_line()+
      geom_ribbon(aes(ymin = mean.signal - sd.signal,ymax = mean.signal+sd.signal,fill = `Lane ID`,color = NULL),alpha = 0.3)+
      geom_vline(xintercept = ladder.maxima())
  })
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

#sapply(1:10, function(x) {paste0("lane ",x)})


#as.data.frame(load.example("parrots")) %>%
#  dplyr::group_by(x) %>%
#  dplyr::summarize(total.signal = sum(value)) -> df

#sm <- smooth.spline(df$x,df$total.signal,df = 40)       

#as.data.frame(predict(sm)) -> df2

#app.t <- c(5,30)

#map(app.t,.f = function(x) seq(x-5,x+5)) %>% unlist()

#unlist(str_split("a,b,c",pattern = ","))


#df <- data.frame(a = 5,b = 6)


