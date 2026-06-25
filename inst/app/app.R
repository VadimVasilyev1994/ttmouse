# ttmouse Shiny app.
#
# Launched by ttmouse::run_app(), which preloads the assets (both models + the
# gene-length table) and passes them via getOption("ttmouse.assets").
#
# Per-upload pipeline (HANDOFF Section 9):
#   read_counts -> preprocess_counts -> validate_genes (block if genes missing)
#   -> project_test_data -> Results_df -> export_results (CSV) + plots.
#
# Model choice is an explicit selector (intergene | timecourse); it is never
# switched automatically. Metadata is required for timecourse (read with
# require_time = TRUE, needs >= 1 grouping column) and optional for intergene
# (require_time = FALSE, nothing required); the same column names are checked in
# both cases. read_metadata() + metadata_to_projection_args() handle the mapping.

library(shiny)
library(plotly)
library(ttmouse)

# Assets preloaded by run_app(); fail clearly if the app was started directly.
.assets <- getOption("ttmouse.assets")
if (is.null(.assets)) {
  stop("No preloaded assets found. Launch the app via ttmouse::run_app().")
}

# Copy an uploaded file to a temp path carrying its original extension, so
# read_counts()/read_metadata() (which dispatch on file extension) see the right
# type -- Shiny's datapath does not preserve the original extension.
upload_with_ext <- function(upload) {
  ext <- tools::file_ext(upload$name)
  dest <- tempfile(fileext = paste0(".", ext))
  file.copy(upload$datapath, dest, overwrite = TRUE)
  dest
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- fluidPage(
  # Scientific light theme (deep blue / green accents), Bootstrap 5 via bslib.
  # Theming only: layout and all input/output IDs are unchanged.
  theme = bslib::bs_theme(
    version   = 5,
    bg        = "#ffffff",
    fg        = "#1c2b36",
    primary   = "#14506e",   # deep blue  -> Run button, links, active tab
    secondary = "#495d6b",   # slate
    success   = "#2a9d8f",   # teal green
    info      = "#2c7da0"
  ),
  titlePanel("ttmouse - circadian clock projection"),
  sidebarLayout(
    sidebarPanel(
      # Organ selector appears only when more than one organ is available.
      uiOutput("organ_selector"),
      # Model-type selector offers only the types present for the chosen organ.
      uiOutput("model_type_selector"),
      helpText("intergene: per-sample (single sample OK). ",
               "timecourse: needs group structure in the metadata."),
      fileInput("counts", "Raw count matrix (.csv or .xlsx)",
                accept = c(".csv", ".xlsx")),
      # Metadata upload is available for both paths: required for timecourse,
      # optional for intergene.
      fileInput("metadata", "Sample metadata (.csv or .xlsx)",
                accept = c(".csv", ".xlsx")),
      helpText("Recognised columns: time, group_1/2/3, replicate (any order, ",
               "case-insensitive). Required for timecourse (needs time + at ",
               "least one group); optional for intergene. Rows must be in the ",
               "same order as the count columns."),
      actionButton("run", "Run projection", class = "btn-primary"),
      tags$hr(),
      # Selectors for the plots (populated after a run).
      uiOutput("projection_selector"),
      uiOutput("sample_selector"),
      tags$hr(),
      downloadButton("download_csv", "Download results CSV")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      tabsetPanel(
        tabPanel("Results",             tableOutput("results_table")),
        tabPanel("Training projection", plotlyOutput("plot_train", height = "500px")),
        tabPanel("Test projection",     plotlyOutput("plot_test",  height = "500px")),
        tabPanel("Sample likelihoods",  plotOutput("plot_likelis", height = "500px")),
        tabPanel("Sample curve",        plotOutput("plot_curve",   height = "500px")),
        tabPanel("Training expression",
                 fluidRow(
                   column(6, uiOutput("train_gene_selector")),
                   column(6, uiOutput("train_group_selector"))
                 ),
                 plotlyOutput("plot_train_gene", height = "450px"))
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Holds the post-projection object and the model used, after a run.
  state <- reactiveValues(object = NULL, model = NULL, status = "Awaiting input.")

  output$status <- renderText(state$status)

  # The organ currently selected (or the only one available when the organ
  # selector is hidden). Resolves even before the selector renders.
  current_organ <- reactive({
    organs <- names(.assets$organs)
    if (!is.null(input$organ) && input$organ %in% organs) input$organ
    else organs[[1]]
  })

  # The model for the currently selected organ + type. Available immediately (no
  # run needed) so the training-data views can be explored before any upload.
  current_model <- reactive({
    req(input$model_choice)
    .assets$organs[[current_organ()]][[input$model_choice]]
  })

  # Organ selector -- only shown when more than one organ is available.
  output$organ_selector <- renderUI({
    organs <- names(.assets$organs)
    if (length(organs) < 2) return(NULL)
    selectInput("organ", "Organ", choices = organs, selected = organs[[1]])
  })

  # Model-type selector -- only the types present for the selected organ.
  output$model_type_selector <- renderUI({
    types <- names(.assets$organs[[current_organ()]])
    sel   <- if ("intergene" %in% types) "intergene" else types[[1]]
    radioButtons("model_choice", "Model", choices = types, selected = sel)
  })

  # Symbol -> Ensembl map for the current model's training genes.
  train_gene_map <- reactive({
    train_genes(current_model(), .assets$gene_lengths)
  })

  # Run the full pipeline when the button is pressed.
  observeEvent(input$run, {
    state$object <- NULL  # clear any previous result

    req(input$counts)
    is_timecourse <- input$model_choice == "timecourse"
    model <- current_model()

    tryCatch({
      # --- Read + preprocess ---------------------------------------------
      counts <- read_counts(upload_with_ext(input$counts))
      expr   <- preprocess_counts(counts, gene_lengths = .assets$gene_lengths)

      # --- Gene check (block clearly if genes are missing) ----------------
      check <- validate_genes(model, expr)
      if (!check$ok) {
        state$status <- paste("Cannot project:", check$summary)
        return(invisible())
      }

      # --- Metadata -> projection args ------------------------------------
      # Timecourse: metadata required (time + >= 1 group). Intergene: optional;
      # if provided, the same columns are checked but nothing is required.
      meta_args <- list()
      if (!is.null(input$metadata)) {
        md <- read_metadata(upload_with_ext(input$metadata), counts = counts,
                            require_time = is_timecourse)
        meta_args <- metadata_to_projection_args(md, require_groups = is_timecourse)
      } else if (is_timecourse) {
        state$status <- "Timecourse selected: please upload a metadata file."
        return(invisible())
      }

      # --- Project --------------------------------------------------------
      object <- do.call(project_test_data,
                        c(list(model = model, expr_matrix = expr), meta_args))

      state$object <- object
      state$model  <- model
      state$status <- paste(check$summary, "Projection complete.",
                            sprintf("%d sample(s).", ncol(expr)))
    },
    error = function(e) {
      state$status <- paste("Error:", conditionMessage(e))
    })
  })

  # Local-projection selector for the 3D plots, from the chosen model's choices.
  output$projection_selector <- renderUI({
    req(state$model)
    choices <- local_projection_choices(state$model)
    selectInput("local_projection", "Local projection (3D plots)",
                choices = choices, selected = choices[1])
  })

  # Sample selector for the per-sample plots, labelled by sample name.
  output$sample_selector <- renderUI({
    req(state$object)
    res <- export_results(state$object)
    selectInput("sample_num", "Sample (per-sample plots)",
                choices = stats::setNames(seq_len(nrow(res)), res$Sample),
                selected = 1)
  })

  # Results table: the prediction-relevant subset.
  output$results_table <- renderTable({
    req(state$object)
    export_results(state$object)
  })

  # CSV download of the same subset.
  output$download_csv <- downloadHandler(
    filename = function() "ttmouse_results.csv",
    content  = function(file) {
      req(state$object)
      export_results(state$object, path = file)
    }
  )

  # 3D training projection (available once a model has been used).
  output$plot_train <- renderPlotly({
    req(state$model, input$local_projection)
    plot_training_projection(state$model, input$local_projection)
  })

  # 3D training + test overlay (needs the completed projection).
  output$plot_test <- renderPlotly({
    req(state$object, input$local_projection)
    plot_test_projection(state$object, input$local_projection)
  })

  # Per-sample raw likelihood curves (base graphics).
  output$plot_likelis <- renderPlot({
    req(state$object, input$sample_num)
    plot_sample_likelihoods(state$object, as.integer(input$sample_num))
  })

  # Per-sample theta-calculation curves (base graphics).
  output$plot_curve <- renderPlot({
    req(state$object, input$sample_num)
    plot_sample_curve(state$object, as.integer(input$sample_num))
  })

  # --- Training-data gene expression --------------------------------------
  output$train_gene_selector <- renderUI({
    choices <- train_gene_map()
    selectInput("train_gene", "Gene (by symbol)",
                choices = choices, selected = choices[[1]])
  })

  output$train_group_selector <- renderUI({
    gv <- train_group_vars(current_model())
    selectInput("train_group", "Group by", choices = gv, selected = gv[[1]])
  })

  output$plot_train_gene <- renderPlotly({
    req(input$train_gene, input$train_group)
    # Recover the symbol label for the chosen Ensembl ID, for the title.
    m   <- train_gene_map()
    lbl <- names(m)[match(input$train_gene, m)]
    plot_train_gene(current_model(), gene = input$train_gene,
                    group_var = input$train_group, gene_label = lbl,
                    interactive = TRUE)
  })
}

shinyApp(ui, server)
