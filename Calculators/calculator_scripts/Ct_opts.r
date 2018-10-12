library(shiny)
library(tidyverse)


Ct_opts_UI <- function(id){
  uiOutput()
}

Ct_opts_Server <- function(input,output,session,df) {
  
  validate(need(df,message = F))
  
  renderUI({
    tagList(
      selectInput(inputId = "Ct_col",label = "Ct Column",choices = colnames(df),multiple = F,selectize = T),
      checkboxInput(inputId = "tech_rep_status",label = "Tech reps?",value = F),
      selectInput(inputId = "Ct_summary_groups",label = "Groups to summarize Ct",choices = colnames(df),multiple = T,selectize = T)
      )
  })
}

