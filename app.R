library(shiny)
library(shinythemes)
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)
library(shinyMatrix)
library(DT)
library(tibble)
library(samplingbook)
library(xtable)
library(gridExtra)

m <- matrix(c(0,0,0,0,0,0,
              5,1,0,0,0,0,
              10,5,1,0,0,0,
              15,13,3,0,0,0,
              20,15,5,0,0,0,
              25,16,7,5,9,0,
              30,17,9,6,12,0,
              35,18,13,8,16,0,
              40,20,15,10,18,0,
              45,21,17,12,22,0,
              60,22,22,22,24,1,
              75,23,23,22,24,2,
              90,23,23,22,24,2,
              105,24,24,22,24,3,
              120,24,24,24,24,24),
            nrow = 15, byrow = TRUE,
            dimnames = list(NULL,
                            c("Time","Bottle1 Dead","Bottle2 Dead",
                              "Bottle3 Dead","Bottle4 Dead","Control Dead")))

diagtimes <- matrix(c(
  20, 45, 45, 90, 45, 60,
  0.75, 30, 30, 45, 60, 0,
  12.5, 15, 30, 15, 30, 60,
  800, 0, 0, 75, 45, 45,
  400, 15, 30, 45, 45, 45,
  2.25, 30, 30, 45, 45, 45,
  43, 10, 10, 30, 30, 30,
  0.05, 0, 0, 60, 60, 0,
  15, 15, 30, 45, 45, 30,
  20, 10, 45, 30, 45, 30
), nrow = 10, byrow = TRUE)
rownames(diagtimes) <- c(
  "Chlorpyrifos", "Deltamethrin", "Etofenprox", "Fenthion",
  "Malathion", "Naled", "Permethrin", "Prallethrin",
  "Pyrethrum", "Sumethrin"
)
colnames(diagtimes) <- c(
  "Insecticide Conc.", "Ae. aegypti", "Ae. albopictus",
  "Cx. pipens", "Cx. quinquefasciatus", "Cx. tarsalis"
)

# <---------- UI ---------->
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$head(
    tags$style(HTML("
      @font-face { font-family: 'DIN Next'; src: url('DINNextW1G-Regular.otf') format('opentype'); }
      @font-face { font-family: 'DIN Next Thin'; src: url('DINNextLight.otf') format('opentype'); }
      .din-next-font { font-family: 'DIN Next', sans-serif; }
      .din-next-thin { font-family: 'DIN Next Thin', sans-serif; }
      #header { background-color: transparent; }
    "))
  ),
  div(id = "header", tags$img(src = "Logo 2 MCEVBD.png", height = "90px")),
  tabsetPanel(
    tabPanel("Insecticide Mortality", fluid = TRUE,
             fluidRow(column(12, htmlOutput("instructions"))),
             fluidRow(
               column(2, selectInput('insecticides','Insecticides',c(None='',rownames(diagtimes)))),
               column(2, selectInput('species','Species',c(None='',colnames(diagtimes)[2:6]))),
               column(8, htmlOutput('datacheck'))
             ),
             fluidRow(
               column(6, htmlOutput('step2')),
               column(6, htmlOutput('recommendation'))
             ),
             fluidRow(
               column(4,
                      span(tags$h4("Data"),style="color:blue;font-style:italic"),
                      selectInput('dropdown_data','Choose a method of uploading data:',
                                  choices=c(''='','Manual Entry','File Upload')),
                      conditionalPanel(
                        condition="input.dropdown_data=='Manual Entry'",
                        matrixInput('sample',value=m,rows=list(names=FALSE),cols=list(names=TRUE),class='numeric')
                      ),
                      conditionalPanel(
                        condition="input.dropdown_data=='File Upload'",
                        fileInput('datafile','Upload CSV File',accept='.csv')
                      )
               ),
               column(8,
                      span(tags$h4("Resistance Plots"),style="color:blue;font-style:italic"),
                      plotOutput('plot')
               )
             ),
             fluidRow(
               column(2, downloadButton('downloadPDF','Download PDF')),
               column(10, htmlOutput('resistantstate'))
             )
    ),
    
    tabPanel("Insecticide Diagnostic Times", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1','Choose CSV File',accept='.csv'),
                 checkboxInput('header','Header',TRUE),
                 actionButton('Splitcolumn','Split Column'),
                 uiOutput('selectUI'),
                 actionButton('deleteRows','Delete Rows'),
                 textInput('textbox','Value(s) to insert (comma-separated):'),
                 actionButton('replacevalues','Insert Values'),
                 actionButton('removecolumn','Remove Column'),
                 actionButton('Undo','Undo')
               ),
               mainPanel(DTOutput('table1'))
             )
    )
  )
)

# <---------- SERVER ---------->
server <- function(input, output, session) {
  library(tidyverse)
  rv <- reactiveValues(data = diagtimes, orig = diagtimes)
  data_in <- reactive({
    req(input$dropdown_data)
    if (input$dropdown_data == 'Manual Entry') {
      df <- as.data.frame(input$sample)
    } else {
      req(input$datafile)
      df <- read.csv(input$datafile$datapath, header = TRUE)
    }
    if ('Control Dead' %in% names(df)) {
      names(df)[names(df)=='Control Dead'] <- 'Control'
    }
    df
  })
  
  
  Datacheck <- reactive({
    req(input$insecticides, input$species, data_in())
    data <- data_in()
    mortality <- data
    for (c in 2:ncol(data)) {
      mortality[,c] <- data[,c] / data[nrow(data),c]
    }
    if (mortality$Control[(nrow(data)-1)] > 0.10) {
      '<br><span style="color:red">Warning: mortality in control bottle is too high. Please repeat bioassay.</span>'
    } else {
      '<br>No inconsistencies detected in mortality data.'
    }
  })
  output$datacheck <- renderText(Datacheck())
  
  AbbottRes <- reactive({
    req(input$insecticides, input$species, data_in())
    data <- data_in()
    mortality <- data
    for (c in 2:ncol(data)) {
      mortality[,c] <- data[,c] / data[nrow(data),c]
    }
    if (mortality$Control[nrow(data)-1] > 0.03) {
      for (c in 2:(ncol(data)-1)) {
        mortality[,c] <- (mortality[,c] - mortality[,ncol(data)]) / (1 - mortality[,ncol(data)])
      }
    }
    diag_time <- diagtimes[input$insecticides, input$species]
    conc      <- diagtimes[input$insecticides, 1]
    mortality$Median <- apply(mortality[,2:(ncol(data)-1)], 1, median, na.rm = TRUE)
    mortality_long <- pivot_longer(mortality[ ,1:(ncol(data)-1)],
                                   cols = 2:(ncol(data)-1),
                                   names_to = 'Replicate',
                                   values_to = 'Mortality')
    
    graph1 <- ggplot() +
      geom_rect(aes(xmin=0, xmax=Inf, ymin=0, ymax=0.9),   alpha=0.5, fill="red") +
      geom_rect(aes(xmin=0, xmax=Inf, ymin=0.9, ymax=0.97),alpha=0.5, fill="orange") +
      geom_rect(aes(xmin=0, xmax=Inf, ymin=0.97, ymax=1),  alpha=0.5, fill="yellow") +
      geom_vline(xintercept = diag_time, color="blue", linetype="dotted") +
      geom_line(data = mortality_long[mortality_long$Time < max(data$Time), ],
                aes(x=Time, y=Mortality, group=Replicate),
                color="white", linetype="dashed") +
      geom_line(data = mortality[1:(nrow(data)-1), ],
                aes(x=Time, y=Median)) +
      labs(y = "Percent Mortality", x = "Time (minutes)") +
      theme_minimal()
    
    list(plot = graph1,
         diag_time = diag_time,
         conc      = conc,
         mortality = mortality)
  })
  
  output$plot <- renderPlot({ AbbottRes()$plot })
  ResistantState <- reactive({
    ab    <- AbbottRes()
    mort  <- ab$mortality
    dt    <- ab$diag_time
    obs   <- mort[mort$Time == dt, 'Median']
    if (obs > 0.97) {
      '<span style="color:#FF0000;"><b>No resistance detected</b>: population susceptible. Continue monitoring.</span>'
    } else if (obs >= 0.90) {
      '<span style="color:#FF9900;"><b>Developing resistance</b>: rotate products and integrate IPM.</span>'
    } else {
      '<span style="color:#FFFF99;"><b>Resistant</b>: avoid this insecticide; consider intensity testing.</span>'
    }
  })
  output$resistantstate <- renderText(ResistantState())
  
  Recommendation <- reactive({
    ab   <- AbbottRes()
    dt   <- ab$diag_time
    conc <- ab$conc
    mort <- ab$mortality
    obs  <- mort[mort$Time == dt, 'Median']
    n_rec <- sample.size.prop(e = 0.1 * obs,
                              P = obs,
                              N = Inf,
                              level = 0.80)[[2]]
    sprintf('<span style="color:#ff69b4;"><b>Recommended:</b> %d mosquitoes, %.1f µg insecticide, track for %d minutes.</span>',
            n_rec, conc, dt)
  })
  output$recommendation <- renderText(Recommendation())
  
  output$downloadPDF <- downloadHandler(
    filename = function() sprintf('My_Report-%s.pdf', Sys.Date()),
    content = function(file) {
      pdf(file)
      grid.table(xtable(data_in()))
      print(AbbottRes()$plot)
      dev.off()
    }
  )
  
  observeEvent(input$file1, {
    file <- input$file1
    req(file)
    validate(need(tools::file_ext(file$datapath) == 'csv', 'CSV required'))
    rv$orig <- read.csv(file$datapath, header = input$header)
    rv$data <- rv$orig
  })
  output$selectUI <- renderUI({ req(rv$data); selectInput('selectcolumn','Select column',choices = names(rv$data)) })
  observeEvent(input$Splitcolumn, { rv$data <- splitColumn(rv$data, input$selectcolumn) })
  observeEvent(input$deleteRows,   { rv$data <- rv$data[-input$table1_rows_selected, ] })
  output$table1 <- renderDT({ datatable(rv$data, editable = TRUE) })
  observeEvent(input$table1_cell_edit, {
    info <- input$table1_cell_edit
    rv$data[info$row, info$col] <- info$value
  })
  observeEvent(input$replacevalues, { rv$data <- fillvalues(rv$data, input$textbox, input$selectcolumn) })
  observeEvent(input$removecolumn, { rv$data <- removecolumn(rv$data, input$selectcolumn) })
  observeEvent(input$Undo,          { rv$data <- rv$orig })
}

shinyApp(ui, server)

