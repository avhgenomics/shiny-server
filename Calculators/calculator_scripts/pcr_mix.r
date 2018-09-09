pcr_master_mixUI <- function(id){
  ns <- NS(id)
  
  
  
  tagList(
    fluidRow(
      column(numericInput(inputId = ns("sample_n"),label = "Number of Samples / Wells",value = 1,min = 1,step = 1),width = 6),
      column(numericInput(inputId = ns("techrep_n"),label = "Number of Tech Reps",value = 1,min = 1,step = 1),width = 6)
    ),
    fluidRow(
      column(numericInput(inputId = ns("sample_ul"),label = "Sample ul per well",value = 2,min = 1,step = 1),width = 6),
      column(numericInput(inputId = ns("SYBR_conc"),label = "SYBR Mix Conc",value = 2,min = 1,step = 1),width = 6)
    ),
    fluidRow(
      column(numericInput(inputId = ns("Fw_stock_uM"),label = "Fw Primer Stock uM",value = 100,min = 1,step = 1),width = 6),
      column(numericInput(inputId = ns("Rv_stock_uM"),label = "Rv Primer Stock uM",value = 100,min = 1,step = 1),width = 6)
    ),
    fluidRow(
      column(numericInput(inputId = ns("target_primer_uM"),label = "Target Primer uM",value = 0.2,min = 0,step = 0.05),width = 6)
    ),
    fluidRow(
      column(numericInput(inputId = ns("vol_per_well"),label = "Target ul per well",value = 25,min = 1,step = 1),width = 6),
      column(numericInput(inputId = ns("extra_pct"),label = "Extra vol percentage",value = 10,min = 0,step = 1),width = 6)
    )
  )
}

pcr_master_mixServer <- function(input,output,session){
  pcr_calculations <- reactive({
    
    SYBR_vol_per <- input$vol_per_well / input$SYBR_conc
    
    Fw_primer_ul_per <- (input$target_primer_uM * input$vol_per_well) / input$Fw_stock_uM
    Rv_primer_ul_per <- (input$target_primer_uM * input$vol_per_well) / input$Rv_stock_uM
    H2O_ul_per <- input$vol_per_well - input$sample_ul - SYBR_vol_per - Fw_primer_ul_per - Rv_primer_ul_per
    
    total_mix_vol = SYBR_vol_per + Fw_primer_ul_per + Rv_primer_ul_per + H2O_ul_per
    
    df <- data.frame("Reagent" = c(paste0(input$SYBR_conc,"X SYBR Green"),
                                   paste0("Fw Primer (",input$Fw_stock_uM," uM)"),
                                   paste0("Rv Primer (",input$Rv_stock_uM," uM)"),
                                   "H~2~O",
                                   "Total Volume"),
                     "Vol" = c(SYBR_vol_per,
                               Fw_primer_ul_per,
                               Rv_primer_ul_per,
                               H2O_ul_per,
                               total_mix_vol)
    )
    
    df$Concentration <- c(
      paste0((total_mix_vol+input$sample_ul) / df$Vol[1] / input$SYBR_conc,"X"),
      paste((input$Fw_stock_uM * df$Vol[2]) / (total_mix_vol+input$sample_ul),"uM"),
      paste((input$Rv_stock_uM * df$Vol[3]) / (total_mix_vol+input$sample_ul),"uM"),
      "",""
    )
    
    df$Vol <- df$Vol * (1+(input$extra_pct/100))
    df$Vol <- round(df$Vol * (input$sample_n * input$techrep_n),4)
    
    df$Vol <- paste(df$Vol,"ul")
    
    colnames(df) <- c("Reagent",paste0("For ",(input$sample_n * input$techrep_n)," Wells +",input$extra_pct,"%"),"Concentration")
    
    return(df)
    
  })
}



pcr_master_UI_APP <- function(inputs_width = 6,recipe_width = 6, id = "pcr",table_out_ID = "pcr_mm_table") {
  tagList(
    box(
      title = "Inputs",
      width = 6,
      collapsible = T,
      collapsed = F,
      pcr_master_mixUI(id)
    ),
    box(
      title = "Master Mix Recipe",
      width = 6,
      collapsible = T,
      collapsed = F,
      tableOutput(table_out_ID)
    )
  )
}
