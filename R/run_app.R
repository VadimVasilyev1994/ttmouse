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
#' @param port Port to listen on. When `NULL` (the default), the `PORT`
#'   environment variable is used if set (e.g. Cloud Run sets it); otherwise the
#'   port falls back to 7860 (the Hugging Face Spaces convention). This lets a
#'   single container serve on either platform unchanged. Pass a value to
#'   override.
#' @param launch.browser Passed to [shiny::runApp()]; defaults to `TRUE`
#'   interactively.
#' @param max_upload_mb Maximum size, in megabytes, of an uploaded file; sets the
#'   `shiny.maxRequestSize` option (Shiny's own default is only 5 MB). Defaults
#'   to 50, comfortably covering typical count matrices; raise it for very large
#'   studies.
#' @return Invisibly `NULL`; the function runs the app until it is stopped.
#' @export
run_app <- function(data_dir = NULL,
                    gene_lengths = NULL,
                    host = "0.0.0.0",
                    port = NULL,
                    launch.browser = interactive(),
                    max_upload_mb = 50) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required to run the app.")
  }

  # Validate the upload cap up front (it is set as an option below).
  if (!is.numeric(max_upload_mb) || length(max_upload_mb) != 1L ||
      is.na(max_upload_mb) || max_upload_mb <= 0) {
    stop("`max_upload_mb` must be a single positive number.")
  }

  # --- Discover + preload assets once (fail loudly at startup) -------------
  found <- discover_assets(data_dir, gene_lengths)

  # Load every organ's models (each organ has intergene and/or timecourse).
  organs <- lapply(found$organs, function(org) {
    lapply(org, load_model)            # structural validation inside load_model
  })
  assets <- list(gene_lengths = readRDS(found$gene_lengths), organs = organs)

  # Hand the preloaded assets to the app and cap the upload size; restore both
  # options on exit. shiny.maxRequestSize is in bytes.
  old <- options(ttmouse.assets    = assets,
                 shiny.maxRequestSize = max_upload_mb * 1024^2)
  on.exit(options(old), add = TRUE)

  # --- Locate and launch the bundled app -----------------------------------
  app_dir <- system.file("app", package = "ttmouse")
  if (!nzchar(app_dir)) {
    stop("Could not find the bundled app directory (inst/app). ",
         "Is ttmouse installed correctly?")
  }

  # Resolve the listening port: an explicit `port` wins; otherwise honour the
  # platform's PORT env var (Cloud Run sets it) and fall back to 7860, the
  # Hugging Face Spaces convention, so one image serves on either platform.
  if (is.null(port)) {
    env_port <- Sys.getenv("PORT", unset = "")
    port <- if (nzchar(env_port)) suppressWarnings(as.integer(env_port)) else 7860L
  }
  if (is.na(port)) {
    stop("Could not resolve a numeric port (check the PORT environment variable).")
  }

  shiny::runApp(app_dir, host = host, port = port,
                launch.browser = launch.browser)
  invisible(NULL)
}
