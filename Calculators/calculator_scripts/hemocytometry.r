hemocytometer_UI_components <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$em("Counting Info"),
             numericInput(inputId = ns("live_cell_count"),label = "Live Cells",value = 1,min = 0,step = 1),
             numericInput(inputId = ns("dead_cell_count"),label = "Dead Cells",value = 0,min = 0,step = 1),
             numericInput(inputId = ns("squares_count"),label = "# outer squares counted",value = 4,min = 1,step = 1)),
      column(width = 6,
             tags$em("Dilution Components (ul)"),
             numericInput(inputId = ns("aliquot_ul"),label = "Sample",value = 50,min = 1,step = 1),
             numericInput(inputId = ns("pbs_ul"),label = "PBS",value = 50,min = 1,step = 1),
             numericInput(inputId = ns("trypan_ul"),label = "Trypan Blue",value = 100,min = 1,step = 1))
    ),
    fluidRow(
      column(width = 6,
             tags$em("Sample Information"),
             numericInput(inputId = ns("total_sample_vol"),label = "Total ml of sample",value = 1,min = 0,step = 1)
    )
  )
  )
}

hemocytometer_UI_APP <- function(inputs_width = 6,recipe_width = 6, id = "hemocytometer",table_out_ID = "hemocytometry_quickstat_table") {
  tagList(
    box(
      title = "Inputs",
      width = 6,
      collapsible = T,
      collapsed = F,
      hemocytometer_UI_components(id)
    ),
    box(
      title = "Quick Stats",
      width = 6,
      collapsible = T,
      collapsed = F,
      tableOutput("hemocytometry_quickstat_table")
    )
  )
}

hemocytometer_Server_QuickStats <- function(input,output,session){
  
  qc.df <- reactive({
    
    total_cell_count = input$live_cell_count+ input$dead_cell_count
    dil.F <- (input$aliquot_ul+input$pbs_ul+input$trypan_ul) / input$aliquot_ul
    
    total_cells_per_ml = (total_cell_count * dil.F * 10000) / input$squares_count
    
    accuracy_high <- 100
    accuracy_low <- 5
    cell_per_square <- (total_cell_count / input$squares_count)
    
    accuracy_est <- if(cell_per_square >= accuracy_low & cell_per_square  <= accuracy_high){
      "Good"
    } else {"Questionable"}
    
    df <- data.frame(Stat = c("Dilution Factor",
                              "Total Cells / ml",
                              "Total Cells in Sample",
                              "Estimated Viability",
                              "Accuracy Range"),
                     Value = c(dil.F,
                               paste(total_cells_per_ml/1000000,"M"),
                               paste((total_cells_per_ml * input$total_sample_vol)/1000000,"M"),
                               paste0((input$live_cell_count / total_cell_count)*100,"%"),
                               paste0(accuracy_est," (",cell_per_square," cells / square)")
                               )
                     )
    
    return(df)
  })
  
  
  
}
hemocytometer_Server_components <- function(input,output,session){
  
}