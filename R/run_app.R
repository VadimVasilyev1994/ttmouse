#' Launch the ttmouse Shiny application
#'
#' Discovers and preloads the runtime assets once at startup -- the shared
#' gene-length table and every organ's models -- then launches the bundled Shiny
#' app. Preloaded assets are passed to the app via the `ttmouse.assets` option
#' and shared across sessions, so they are read from disk only once.
#'
#' Assets are located by [discover_assets()]: pass `data_dir` (containing
#' `gene_lengths.rds` and one subdirectory per organ, each with `intergene.rds`
#' and/or `timecourse.rds`) or set the `TTMOUSE_DATA_DIR` environment variable. A
#' single organ is fine. A missing or invalid asset fails loudly here, at
#' startup, rather than mid-session.
#'
#' @param data_dir Directory with `gene_lengths.rds` and the organ
#'   subdirectories; defaults to the `TTMOUSE_DATA_DIR` environment variable.
#' @param gene_lengths Optional explicit path to the gene-length table,
#'   overriding the conventional top-level `gene_lengths.rds`.
#' @param host Host passed to [shiny::runApp()]; defaults to `"0.0.0.0"` so the
#'   app is reachable when containerised.
#' @param port Port passed to [shiny::runApp()]; `NULL` lets Shiny choose.
#' @param launch.browser Passed to [shiny::runApp()]; defaults to `TRUE`
#'   interactively.
#' @return Invisibly `NULL`; the function runs the app until it is stopped.
#' @export
run_app <- function(data_dir = NULL,
                    gene_lengths = NULL,
                    host = "0.0.0.0",
                    port = NULL,
                    launch.browser = interactive()) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required to run the app.")
  }

  # --- Discover + preload assets once (fail loudly at startup) -------------
  found <- discover_assets(data_dir, gene_lengths)

  # Load every organ's models (each organ has intergene and/or timecourse).
  organs <- lapply(found$organs, function(org) {
    lapply(org, load_model)            # structural validation inside load_model
  })
  assets <- list(gene_lengths = readRDS(found$gene_lengths), organs = organs)

  # Hand the preloaded assets to the app and restore the option on exit.
  old <- options(ttmouse.assets = assets)
  on.exit(options(old), add = TRUE)

  # --- Locate and launch the bundled app -----------------------------------
  app_dir <- system.file("app", package = "ttmouse")
  if (!nzchar(app_dir)) {
    stop("Could not find the bundled app directory (inst/app). ",
         "Is ttmouse installed correctly?")
  }

  if (is.null(port)) {
    shiny::runApp(app_dir, host = host, launch.browser = launch.browser)
  } else {
    shiny::runApp(app_dir, host = host, port = port,
                  launch.browser = launch.browser)
  }
  invisible(NULL)
}
