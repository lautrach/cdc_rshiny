library(shiny)
library(shinythemes)
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)
library("shinyMatrix")
library(DT)
library(tibble)
library(samplingbook)
library(xtable)
library(gridExtra)

###function for deleting the rows
splitColumn <- function(data, column_name) {
  newColNames <- c("Unmerged_type1", "Unmerged_type2")
  newCols <- colsplit(data[[column_name]], " ", newColNames)
  after_merge <- cbind(data, newCols)
  after_merge[[column_name]] <- NULL
  after_merge
}

###_______________________________________________
### function for inserting a new column
fillvalues <- function(data, values, columName){
  df_fill <- data
  vec <- strsplit(values, ",")[[1]]
  df_fill <- tibble::add_column(df_fill, newcolumn = vec, .after = columName)
  df_fill
}

##function for removing the column
removecolumn <- function(df, nameofthecolumn){
  df[ , -which(names(df) %in% nameofthecolumn)]
}

## Reads in user input data
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
              120,24,24,24,24,24), 15, 6, byrow=TRUE, dimnames = list(NULL, c("Time",
                                                                              "Bottle1 Dead",
                                                                              "Bottle2 Dead",
                                                                              "Bottle3 Dead",
                                                                              "Bottle4 Dead",
                                                                              "Control Dead")))

## Diagnostic times from CDC Manual
diagtimes = matrix(c(20, 45, 45, 90, 45, 60, 0.75, 30, 30, 45, 60, 0, 12.5, 15, 30, 15, 30, 60, 800, 0, 0, 75, 45,
                     45, 400, 15, 30, 45, 45, 45, 2.25, 30, 30, 45, 45, 45, 43, 10, 10, 30, 30, 30, 0.05, 0, 0, 60, 60,
                     0, 15, 15, 30, 45, 45, 30, 20, 10, 45, 30, 45, 30), 10, 6, byrow=TRUE)
rownames(diagtimes) = c("Chlorpyrifos", "Deltamethrin", "Etofenprox", "Fenthion", "Malathion", "Naled", "Permethrin", "Prallethrin", "Pyrethrum", "Sumethrin")
colnames(diagtimes) = c("Insecticide Conc.", "Ae. aegypti", "Ae. albopictus", "Cx. pipens", "Cx. quinquefasciatus", "Cx. tarsalis")

# ── UI ──────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = shinytheme("yeti"),

  # ── Custom CSS ──
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400;600;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      /* ── Base ── */
      body {
        font-family: 'Source Sans Pro', sans-serif;
        background-color: #F4F6F9;
        color: #2D3748;
      }

      /* ── Header bar ── */
      .app-header {
        background: #FFFFFF;
        border-bottom: 3px solid #1565C0;
        padding: 14px 24px;
        margin: -15px -15px 20px -15px;
        display: flex;
        align-items: center;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
      }
      .app-header img { height: 60px; margin-right: 18px; }
      .app-header h1 {
        font-size: 22px;
        font-weight: 700;
        color: #1565C0;
        margin: 0;
        letter-spacing: -0.3px;
      }

      /* ── Tabs ── */
      .nav-tabs { border-bottom: 2px solid #E2E8F0; }
      .nav-tabs > li > a {
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
        color: #4A5568;
        border: none;
        padding: 10px 20px;
        transition: color 0.2s;
      }
      .nav-tabs > li > a:hover { color: #1565C0; background: transparent; border: none; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: #1565C0;
        border: none;
        border-bottom: 3px solid #1565C0;
        background: transparent;
      }

      /* ── Cards ── */
      .card-panel {
        background: #FFFFFF;
        border-radius: 8px;
        padding: 20px 24px;
        margin-bottom: 18px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.07);
      }
      .card-panel h4 {
        font-weight: 700;
        font-size: 16px;
        color: #1A202C;
        margin-top: 0;
        margin-bottom: 12px;
        padding-bottom: 8px;
        border-bottom: 2px solid #E2E8F0;
      }

      /* ── Info boxes ── */
      .info-box {
        background: #EBF5FB;
        border-left: 4px solid #1565C0;
        border-radius: 4px;
        padding: 14px 18px;
        margin-bottom: 16px;
        font-size: 14px;
        line-height: 1.6;
      }
      .info-box a { color: #1565C0; font-weight: 600; }

      .step-label {
        background: #F7FAFC;
        border-left: 4px solid #2B6CB0;
        border-radius: 4px;
        padding: 12px 16px;
        margin-bottom: 14px;
        font-weight: 600;
        font-size: 14px;
        color: #2D3748;
      }

      /* ── Recommendation box ── */
      .rec-box {
        background: #FFF8E1;
        border-left: 4px solid #F9A825;
        border-radius: 4px;
        padding: 14px 18px;
        font-size: 14px;
        line-height: 1.6;
      }
      .rec-box b { color: #E65100; }

      /* ── Resistance result boxes ── */
      .result-susceptible {
        background: #E8F5E9;
        border-left: 4px solid #43A047;
        border-radius: 4px;
        padding: 14px 18px;
        font-size: 14px;
      }
      .result-developing {
        background: #FFF3E0;
        border-left: 4px solid #FB8C00;
        border-radius: 4px;
        padding: 14px 18px;
        font-size: 14px;
      }
      .result-resistant {
        background: #FFEBEE;
        border-left: 4px solid #E53935;
        border-radius: 4px;
        padding: 14px 18px;
        font-size: 14px;
      }

      /* ── Warning box ── */
      .warn-box {
        background: #FFEBEE;
        border-left: 4px solid #E53935;
        border-radius: 4px;
        padding: 10px 14px;
        font-size: 13px;
      }
      .ok-box {
        background: #E8F5E9;
        border-left: 4px solid #43A047;
        border-radius: 4px;
        padding: 10px 14px;
        font-size: 13px;
      }

      /* ── Select inputs ── */
      .selectize-input {
        border-radius: 6px !important;
        border: 1px solid #CBD5E0 !important;
        font-family: 'Source Sans Pro', sans-serif !important;
      }

      /* ── Download button ── */
      #downloadPDF {
        background: #1565C0;
        color: #fff;
        border: none;
        border-radius: 6px;
        padding: 10px 22px;
        font-weight: 600;
        font-family: 'Source Sans Pro', sans-serif;
        transition: background 0.2s;
      }
      #downloadPDF:hover { background: #0D47A1; }

      /* ── Plot area ── */
      .plot-area {
        background: #FFFFFF;
        border-radius: 8px;
        padding: 16px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.07);
      }
    "))
  ),

  # ── Header ──
  div(class = "app-header",
      tags$img(src = "logo2.jpg"),
      tags$h1("CDC Bottle Bioassay Analysis Tool")
  ),

  tabsetPanel(
    # ════════════════════════════════════════════════════════════════════════════
    # TAB 1: Insecticide Mortality
    # ════════════════════════════════════════════════════════════════════════════
    tabPanel("Insecticide Mortality", fluid = TRUE,

      # Instructions
      div(class = "info-box", uiOutput("instructions")),

      # Step 1
      div(class = "card-panel",
        tags$h4("Step 1 — Select Insecticide & Species"),
        div(class = "step-label",
          "CDC has determined bottle bioassay threshold times and diagnostic doses for several
           species of mosquitoes. Select the mosquito species and insecticide below to set your
           threshold and dose."
        ),
        fluidRow(
          column(3, selectInput('insecticides', "Insecticide",
                                c("Select..." = "", rownames(diagtimes)))),
          column(3, selectInput('species', "Species",
                                c("Select..." = "", colnames(diagtimes)[2:6]))),
          column(6, uiOutput("datacheck"))
        ),
        uiOutput("step1_recommendation")
      ),

      # Step 2
      div(class = "card-panel",
        tags$h4("Step 2 — Enter Mortality Data"),
        div(class = "step-label",
          "Enter the number of dead mosquitoes at each time point in each bottle.
           At the final time point, enter the total number of mosquitoes (dead and alive).
           It may help to freeze the bottle at the end of the experiment to knock down
           all mosquitoes."
        ),

        # Recommendation sits here
        uiOutput("recommendation"),

        fluidRow(
          column(4,
            selectInput("dropdown_data",
                        "Data upload method:",
                        choices = c("Select..." = "",
                                    "Manual Entry" = "Manual Entry",
                                    "File Upload" = "File Upload"),
                        selected = ""),
            conditionalPanel(
              condition = "input.dropdown_data === 'Manual Entry'",
              matrixInput("sample",
                          value = m,
                          rows = list(names = FALSE),
                          cols = list(names = TRUE),
                          class = 'numeric')
            ),
            conditionalPanel(
              condition = "input.dropdown_data === 'File Upload'",
              fileInput("datafile", "Upload CSV File", accept = ".csv")
            )
          ),
          column(8,
            div(class = "plot-area",
              tags$h4("Resistance Plot"),
              plotOutput("plot", height = "420px")
            )
          )
        )
      ),

      # Results row
      div(class = "card-panel",
        fluidRow(
          column(2, downloadButton("downloadPDF", "Download PDF")),
          column(10, uiOutput("resistantstate"))
        )
      )
    ),

    # ════════════════════════════════════════════════════════════════════════════
    # TAB 2: Insecticide Diagnostic Times
    # ════════════════════════════════════════════════════════════════════════════
    tabPanel("Insecticide Diagnostic Times", fluid = TRUE,
      div(class = "card-panel", style = "margin-top:16px;",
        sidebarLayout(
          sidebarPanel(
            fileInput("file1", "Choose CSV File", accept = ".csv"),
            checkboxInput("header", "Header", TRUE),
            actionButton("Splitcolumn", "Split Column"),
            uiOutput("selectUI"),
            actionButton("deleteRows", "Delete Rows"),
            textInput("textbox", label = "Value to replace:"),
            actionButton("replacevalues", label = 'Replace values'),
            actionButton("removecolumn", "Remove Column"),
            actionButton("Undo", 'Undo')
          ),
          mainPanel(
            DTOutput("table1")
          )
        )
      )
    )
  )
)

# ── Server ──────────────────────────────────────────────────────────────────────
server <- function(session, input, output) {
  library(tidyverse)
  library(tibble)
  library(ggplot2)
  library(gridExtra)
  library(samplingbook)

  rv <- reactiveValues(data = diagtimes, orig = diagtimes, state = FALSE)

  # Data source selection
  data_source <- reactive({
    req(input$dropdown_data)
    input$dropdown_data
  })

  # Abbott function — returns list with plot + computed values
  Abbott <- reactive({
    req(input$insecticides)
    req(input$species)
    req(data_source())

    if (data_source() == "Manual Entry") {
      data <- as.data.frame(input$sample)
    } else if (data_source() == "File Upload") {
      req(input$datafile)
      data <- read.csv(input$datafile$datapath)
    } else {
      return(NULL)
    }

    mortality <- data
    for (c in 2:ncol(data)) {
      mortality[, c] <- data[, c] / data[nrow(data), c]
    }

    if (mortality$Control[(nrow(data) - 1)] > 0.03) {
      for (c in 2:(ncol(data) - 1)) {
        mortality[, c] <- (mortality[, c] - mortality[, ncol(data)]) / (1 - mortality[, ncol(data)])
      }
    }

    diag_time <- diagtimes[input$insecticides, input$species]
    conc      <- diagtimes[input$insecticides, 1]

    mortality$Median <- apply(mortality[, 2:(ncol(data) - 1)], 1, median, na.rm = TRUE)
    mortality_long <- pivot_longer(mortality[, 1:(ncol(data) - 1)],
                                   cols = 2:(ncol(data) - 1),
                                   names_to = "Replicate",
                                   values_to = "Mortality")

    graph1 <- ggplot() +
      geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0, ymax = 0.9), alpha = 0.5, fill = "#EF5350") +
      geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0.9, ymax = 0.97), alpha = 0.5, fill = "#FFA726") +
      geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0.97, ymax = 1), alpha = 0.5, fill = "#FFEE58") +
      geom_vline(xintercept = diag_time, color = "#1565C0", linetype = "dotted", linewidth = 1) +
      geom_line(data = mortality_long[which(mortality_long$Time < max(data$Time)), ],
                aes(x = Time, y = Mortality, group = Replicate), color = "grey70", linetype = "dashed") +
      geom_line(data = mortality[1:(nrow(data) - 1), ],
                aes(x = Time, y = Median), linewidth = 1.1, color = "#1A237E") +
      ylab("Percent Mortality") +
      xlab("Time (minutes)") +
      theme_minimal(base_family = "sans", base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
      )

    df <- mortality[1:(nrow(data) - 1), ]
    observed_per <- df[df$Time == diag_time, ]$Median

    rv$state       <- TRUE
    rv$observed_per <- observed_per
    rv$diag_time   <- diag_time
    rv$conc        <- conc

    return(graph1)
  })

  # Plot output
  output$plot <- renderPlot({
    req(data_source())
    Abbott()
  })

  # Data check output — uses uiOutput / renderUI so HTML renders
  output$datacheck <- renderUI({
    req(input$insecticides)
    req(input$species)
    req(data_source())

    if (data_source() == "Manual Entry") {
      data <- as.data.frame(input$sample)
    } else if (data_source() == "File Upload") {
      req(input$datafile)
      data <- read.csv(input$datafile$datapath)
    } else {
      return(div(class = "info-box", "Please select a data upload method."))
    }

    mortality <- data
    for (c in 2:ncol(data)) { mortality[, c] <- data[, c] / data[nrow(data), c] }

    if (mortality$Control[(nrow(data) - 1)] > 0.1) {
      div(class = "warn-box",
          icon("exclamation-triangle"),
          " Warning: mortality in control bottle is too high. Please repeat bioassay.")
    } else {
      div(class = "ok-box",
          icon("check-circle"),
          " No inconsistencies detected in mortality data.")
    }
  })

  # Step 1 recommendation — concentration and diagnostic time
  output$step1_recommendation <- renderUI({
    req(input$insecticides, input$species)
    conc      <- diagtimes[input$insecticides, "Insecticide Conc."]
    diag_time <- diagtimes[input$insecticides, input$species]

    if (diag_time == 0) {
      return(div(class = "warn-box", style = "margin-top:14px;",
        "No diagnostic time available for this insecticide/species combination."
      ))
    }

    div(class = "rec-box", style = "margin-top:14px;",
      HTML(sprintf(
        '<b>Recommendation:</b> Treat each bottle with <b>%.1f \u00b5g</b> of insecticide
         and track mortality for a minimum of <b>%d minutes</b>.',
        conc, diag_time
      ))
    )
  })

  # Recommendation output — uses renderUI so styled HTML renders properly
  output$recommendation <- renderUI({
    req(rv$state)
    req(input$insecticides)
    req(input$species)

    observed_per <- rv$observed_per
    conc         <- rv$conc
    diag_time    <- rv$diag_time

    if (0.1 * observed_per > observed_per || 0.1 * observed_per > (1 - observed_per)) {
      return(div(class = "warn-box", style = "margin-bottom:14px;",
        "No recommended diagnostic time available for this insecticide in this mosquito species.
         Please choose a different insecticide."
      ))
    }

    n_rec <- sample.size.prop(e = 0.1 * observed_per,
                              P = observed_per,
                              N = Inf,
                              level = 0.80)[[2]]

    div(class = "rec-box", style = "margin-bottom:14px;",
      HTML(sprintf(
        '<b>Recommendation:</b> Use <b>%d</b> mosquitoes, treat each bottle with
         <b>%.1f \u00b5g</b> of insecticide, and track mortality for a minimum of
         <b>%d minutes</b>.',
        n_rec, conc, diag_time
      ))
    )
  })

  # Resistant state output — uses renderUI for styled boxes
  output$resistantstate <- renderUI({
    req(rv$state)

    observed_per <- rv$observed_per

    if (observed_per > 0.97) {
      div(class = "result-susceptible",
        HTML('<b>No resistance detected (population susceptible):</b>
              Consider baseline mechanism testing (enzymes, molecular assays, or CDC bottle
              bioassay with inhibitors). <b>Continue monitoring.</b>'))
    } else if (observed_per >= 0.90 ) {
      div(class = "result-developing",
        HTML('<b>Developing resistance:</b>
              Consider mechanism testing (enzymes, molecular assays, or CDC bottle bioassay
              with inhibitors) and field testing. <b>Rotate insecticide products and implement
              integrated pest management best practices.</b>'))
    } else {
      div(class = "result-resistant",
        HTML('<b>Resistant:</b>
              Consider intensity testing (mortality at 120 minutes or CDC bottle bioassay with
              1X, 2X, 5X, and 10X the diagnostic dosage), mechanism testing (enzymes, molecular
              assays, or CDC bottle bioassay with inhibitors) and field testing.
              <b>Avoid this insecticide in this population.</b>'))
    }
  })

  # PDF download handler
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("CDC_Bioassay_Report-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(data_source())

      pdf(file, width = 10, height = 12)

      if (data_source() == "Manual Entry") {
        grid.table(input$sample)
      } else if (data_source() == "File Upload") {
        grid.table(head(read.csv(input$datafile$datapath)))
      }

      grid.newpage()
      print(Abbott())
      dev.off()
    }
  )

  # ── Insecticide Diagnostic Times tab ──
  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    rv$orig <- read.csv(file$datapath, header = input$header)
    rv$data <- rv$orig
  })

  output$selectUI <- renderUI({
    req(rv$data)
    selectInput(inputId = 'selectcolumn', label = 'Select column', choices = names(rv$data))
  })

  observeEvent(input$Splitcolumn, {
    rv$data <- splitColumn(rv$data, input$selectcolumn)
  })

  observeEvent(input$deleteRows, {
    if (!is.null(input$table1_rows_selected)) {
      rv$data <- rv$data[-as.numeric(input$table1_rows_selected), ]
    }
  })

  output$table1 <- renderDT({
    datatable(rv$data, editable = TRUE)
  })

  observeEvent(input$table1_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    rv$data[row, clmn] <- input$table1_cell_edit$value
  })

  observeEvent(input$replacevalues, {
    rv$data <- fillvalues(rv$data, input$textbox, input$selectcolumn)
  })

  observeEvent(input$removecolumn, {
    rv$data <- removecolumn(rv$data, input$selectcolumn)
  })

  observeEvent(input$Undo, {
    rv$data <- rv$orig
  })

  # ── Static text outputs ──
  output$instructions <- renderUI({
    HTML("The steps below will help you to analyze data from the CDC bottle bioassay according to the <u><a href=\"https://www.cdc.gov/mosquitoes/pdfs/CONUS-508.pdf\">CONUS Manual for Evaluating Insecticide Resistance in Mosquitoes Using the CDC Bottle Bioassay Kit[PDF – 19 pages]</a></u><br>
          Programs in the continental United States and its territories can order free Insecticide Resistance Kits by sending an email to USBottleAssayKit@cdc.gov and requesting an order form. Kits include bottles, insecticide, and manual.")
  })
}

shinyApp(ui = ui, server = server)