library(shiny)
library(tidyverse)
file_import_UI <- function(id,label_text,title_text = "File Import",width = 4,multiple = F){
  ns <- NS(id)
  
  box(title = title_text,
      width = width,
      collapsible = T,
      collapsed = F,
  tagList(
    fileInput(
      inputId = ns("raw_file"),
      label = label_text,
      multiple = multiple,
      buttonLabel = "Load File"
    ),
    fluidRow(
    column(
      width = 9,
      checkboxInput(
        inputId = ns("header_status"),
        label = "Header?",value = T
      ),
      selectInput(
        inputId = ns("raw_delimiter"),
        label = "Select Delimiter",
        choices = c(
          "Comma",
          "Tab",
          ";",
          "_"
        ),
        selected = "Comma",
        multiple = F
      )
    ))
  ))
}

file_import_Server <- function(input,output,session){
  
  reactive({
    
    validate(need(input$raw_file,message = F))
    
    df <- switch(input$raw_delimiter,
                 "Comma" = read_csv(file = input$raw_file$datapath,col_names = input$header_status,quote = ""),
                 "Tab" = read_delim(file = input$raw_file$datapath,col_names = input$header_status,quote = ""))

    return(df)
    })
  
  
}