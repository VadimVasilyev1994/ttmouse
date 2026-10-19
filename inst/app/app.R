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

# Render markdown text to HTML (commonmark, via shiny), with every link opening
# in a new browser tab so following one never navigates away from a running
# session and its results.
md_to_html <- function(text) {
  html <- as.character(shiny::markdown(text))
  HTML(gsub("<a href=", '<a target="_blank" rel="noopener" href=', html, fixed = TRUE))
}

# About / help text for the landing tab. Kept in about.md (next to this file) so
# it can be edited without touching the R code; read once at startup.
.about_file <- "about.md"
if (!file.exists(.about_file)) {
  stop("About text not found (inst/app/about.md). Is ttmouse installed correctly?")
}
.about_html <- md_to_html(paste(readLines(.about_file, encoding = "UTF-8", warn = FALSE),
                                collapse = "\n"))

# Access / licence statement, shown in the footer under every tab.
.footer_html <- md_to_html(paste(
  "ttmouse is free and open to all users, with no login required. Source code:",
  "[github.com/VadimVasilyev1994/ttmouse](https://github.com/VadimVasilyev1994/ttmouse)",
  "(MIT licence). Uploaded files are used only to compute your results and are",
  "not kept after your session ends."))

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
  # Styling for the About tab (readable line length and spacing) and the footer
  # (small and muted, so it is visible without competing with the results).
  tags$head(tags$style(HTML("
    .tt-about { max-width: 780px; line-height: 1.6; padding: 1.25rem 0 0.5rem; }
    .tt-about h2 { font-size: 1.45rem; color: #14506e; margin-bottom: 0.75rem; }
    .tt-about h3 { font-size: 1.1rem; color: #14506e; margin-top: 1.5rem; }
    .tt-about li { margin-bottom: 0.45rem; }
    .tt-about code { color: #1c2b36; background: #f1f4f6; padding: 0 0.25rem;
                     border-radius: 3px; }
    .tt-footer { margin-top: 2rem; padding-top: 0.75rem; border-top: 1px solid #dee2e6;
                 font-size: 0.85rem; color: #495d6b; }
    .tt-footer p { margin-bottom: 0.5rem; }
  "))),
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
      uiOutput("status"),
      # About is the first tab and the landing page; after a successful run the
      # server switches to Results (see the run observer).
      tabsetPanel(
        id = "main_tabs", selected = "About",
        tabPanel("About", div(class = "tt-about", .about_html)),
        tabPanel("Results",             tableOutput("results_table")),
        tabPanel("Results plot",
                 fluidRow(
                   column(4, uiOutput("results_x_selector")),
                   column(4, uiOutput("results_y_selector")),
                   column(4, uiOutput("results_colour_selector"))
                 ),
                 fluidRow(
                   column(4, uiOutput("results_shape_selector")),
                   column(4, uiOutput("results_facet_selector"))
                 ),
                 fluidRow(column(9, plotlyOutput("plot_results", height = "500px")))),
        tabPanel("Training projection",
                 fluidRow(column(9, plotlyOutput("plot_train", height = "500px")))),
        tabPanel("Test projection",
                 fluidRow(column(9, plotlyOutput("plot_test", height = "500px")))),
        tabPanel("Sample likelihoods",
                 fluidRow(column(9, plotOutput("plot_likelis", height = "500px")))),
        tabPanel("Sample curve",
                 fluidRow(column(9, plotOutput("plot_curve", height = "500px")))),
        tabPanel("Training expression",
                 fluidRow(
                   column(6, uiOutput("train_gene_selector")),
                   column(6, uiOutput("train_group_selector"))
                 ),
                 fluidRow(column(9, plotlyOutput("plot_train_gene", height = "450px"))))
      ),
      # Access / licence statement, visible under every tab.
      div(class = "tt-footer", .footer_html)
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Holds the post-projection object and the model used, after a run.
  # status_type is "info" normally and "error" when the message is a blocking
  # error that should stand out (rendered in red below). warnings holds any
  # non-blocking warnings from the last run (rendered in amber below the status).
  state <- reactiveValues(object = NULL, model = NULL,
                          status = "Awaiting input.", status_type = "info",
                          warnings = character(0))

  # Status line. Kept in a <pre> to preserve the original monospace look and any
  # line breaks in the message; coloured red when status_type is "error".
  # Long messages wrap (pre-wrap) rather than scrolling sideways.
  output$status <- renderUI({
    style <- if (identical(state$status_type, "error")) {
      "color:#c0392b; font-weight:600; margin:0; white-space:pre-wrap;"   # red, blocking error
    } else {
      "margin:0;"
    }
    # Warnings: one amber line each, under the status, so they are visible but
    # do not replace the main message.
    warn <- if (length(state$warnings)) {
      tags$pre(style = "color:#9a6700; font-weight:600; margin:0; white-space:pre-wrap;",
               paste("Warning:", state$warnings, collapse = "\n"))
    }
    tagList(tags$pre(style = style, state$status), warn)
  })

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
    state$status_type <- "info"  # clear any prior error styling
    state$warnings <- character(0)  # clear any prior warnings

    req(input$counts)
    is_timecourse <- input$model_choice == "timecourse"
    model <- current_model()

    # Progress notification for the whole run, so users can see the projection
    # is working (the status line only updates once the run has finished).
    # Closed automatically when the run ends, including early stops and errors.
    withProgress(message = "Running projection. Please do not refresh the page.",
                 value = 0, {
      tryCatch({
        # --- Read + preprocess ---------------------------------------------
        setProgress(0.1, detail = "Reading and preprocessing counts")
        counts <- read_counts(upload_with_ext(input$counts))
        expr   <- preprocess_counts(counts, gene_lengths = .assets$gene_lengths)

        # --- Gene check (block clearly if genes are missing) ----------------
        setProgress(0.3, detail = "Checking genes")
        check <- validate_genes(model, expr)
        if (!check$ok) {
          state$status <- paste("Cannot project:", check$summary)
          return(invisible())
        }

        # --- Metadata -> projection args ------------------------------------
        setProgress(0.4, detail = "Reading metadata")
        # Timecourse: metadata required (time + >= 1 group). Intergene: optional;
        # if provided, the same columns are checked but nothing is required.
        meta_args <- list()
        if (!is.null(input$metadata)) {
          # Non-blocking warnings (e.g. unrecognised time formats on intergene,
          # or mixed ZT/CT labels) are collected for display under the status
          # line instead of being lost in the console.
          md <- withCallingHandlers(
            read_metadata(upload_with_ext(input$metadata), counts = counts,
                          require_time = is_timecourse),
            warning = function(w) {
              state$warnings <- c(state$warnings, conditionMessage(w))
              invokeRestart("muffleWarning")
            })
          meta_args <- metadata_to_projection_args(md, require_groups = is_timecourse)
        } else if (is_timecourse) {
          state$status <- "Timecourse selected: please upload a metadata file."
          return(invisible())
        }

        # --- Time-course coverage check (block clearly if too narrow) --------
        # Time-course normalisation scales each group across its own samples, so a
        # group covering too small a time window is unreliable. Flag the offending
        # group(s) and block projection (no result) so the user can amend the data.
        if (is_timecourse) {
          cov <- check_time_coverage(
            test_time      = meta_args$test_time,
            test_group_1   = meta_args$test_group_1,
            test_group_2   = meta_args$test_group_2,
            test_group_3   = meta_args$test_group_3,
            test_replicate = meta_args$test_replicate)
          if (!cov$ok) {
            state$status      <- cov$summary
            state$status_type <- "error"      # render the message in red
            return(invisible())
          }
        }

        # --- Project --------------------------------------------------------
        setProgress(0.5, detail = "Projecting onto the model")
        object <- do.call(project_test_data,
                          c(list(model = model, expr_matrix = expr), meta_args))

        state$object <- object
        state$model  <- model
        setProgress(1)
        state$status <- paste(check$summary, "Projection complete.",
                              sprintf("%d sample(s).", ncol(expr)))
        # Move the user from the About landing page to their results.
        updateTabsetPanel(session, "main_tabs", selected = "Results")
      },
      error = function(e) {
        state$status <- paste("Error:", conditionMessage(e))
        # Time-format problems block the time-course path: show them in red.
        if (inherits(e, "ttmouse_time_error")) state$status_type <- "error"
      })
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

  # --- Results plot (x / y / colour builder over the exported results) -----
  # Column choices for the three selectors, from the current results table.
  results_vars <- reactive({
    req(state$object)
    results_plot_vars(export_results(state$object))
  })

  output$results_x_selector <- renderUI({
    v <- results_vars()$x
    # Default: actual time when the metadata supplied it, otherwise per sample.
    sel <- if ("Actual_Time" %in% v) "Actual_Time" else "Sample"
    selectInput("results_x", "x axis", choices = v, selected = sel)
  })

  output$results_y_selector <- renderUI({
    v <- results_vars()$y
    req(length(v) > 0)
    sel <- if ("weighted_mean_time_pred" %in% v) "weighted_mean_time_pred" else v[[1]]
    selectInput("results_y", "y axis", choices = v, selected = sel)
  })

  output$results_colour_selector <- renderUI({
    v <- results_vars()$colour
    # "" means no colouring; labelled "None" so the choice is explicit.
    sel <- if ("Group_1" %in% v) "Group_1" else ""
    selectInput("results_colour", "Colour by", choices = c("None" = "", v),
                selected = sel)
  })

  # Shape and facet selectors: off ("None") by default.
  output$results_shape_selector <- renderUI({
    selectInput("results_shape", "Shape by",
                choices = c("None" = "", results_vars()$shape), selected = "")
  })

  output$results_facet_selector <- renderUI({
    selectInput("results_facet", "Facet by",
                choices = c("None" = "", results_vars()$facet), selected = "")
  })

  output$plot_results <- renderPlotly({
    v <- results_vars()
    # TRUE when an optional selector is unset, "None", or a valid choice.
    opt_ok <- function(value, choices) is.null(value) || value %in% c("", choices)
    # Wait until the selectors hold choices valid for the current results (they
    # can briefly hold the previous run's values while re-rendering).
    req(input$results_x %in% v$x, input$results_y %in% v$y,
        opt_ok(input$results_colour, v$colour),
        opt_ok(input$results_shape,  v$shape),
        opt_ok(input$results_facet,  v$facet))
    plot_results(export_results(state$object), x = input$results_x,
                 y = input$results_y, colour = input$results_colour,
                 shape = input$results_shape, facet = input$results_facet,
                 interactive = TRUE)
  })

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
