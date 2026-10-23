# Non-Shiny helpers for the app, factored out so they can be unit-tested without
# a running Shiny session. discover_assets() locates the runtime data assets
# (shared gene lengths + one subdirectory of models per organ);
# metadata_to_projection_args() maps a metadata table to project_test_data() args.

#' Discover the runtime data assets (organ models, gene lengths, examples)
#'
#' Resolves the data directory (the `data_dir` argument, else the
#' `TTMOUSE_DATA_DIR` environment variable) and discovers the assets within it: a
#' shared `gene_lengths.rds` at the top level, and one subdirectory per organ
#' containing `intergene.rds` and/or `timecourse.rds`. An organ is included if it
#' has at least one of the two model files; an organ may provide only one type. A
#' single organ is fine (no error).
#'
#' An organ may also hold an `examples/` folder with one subdirectory per example
#' dataset, named after the dataset (e.g. its GEO accession). Each must contain
#' `counts.xlsx`, `metadata.xlsx` and `description.md`; a missing file is an
#' error, so an incomplete example fails at startup rather than when a user
#' selects it. Examples are only collected for organs that have models.
#'
#' Expected layout:
#' \preformatted{
#' data_dir/
#'   gene_lengths.rds        # shared, organ-independent
#'   liver/  intergene.rds  timecourse.rds
#'     examples/
#'       GSE190939/  counts.xlsx  metadata.xlsx  description.md
#'   scn/    intergene.rds  timecourse.rds
#' }
#'
#' @param data_dir Directory containing `gene_lengths.rds` and the organ
#'   subdirectories; defaults to the `TTMOUSE_DATA_DIR` environment variable.
#' @param gene_lengths Optional explicit path to the gene-length table,
#'   overriding the conventional top-level `gene_lengths.rds`.
#' @return A list with `gene_lengths` (a verified file path), `organs` (a
#'   named list; each organ is itself a named list of the model file paths that
#'   exist, among `intergene` and `timecourse`) and `examples` (a named list by
#'   organ, holding only organs that have examples; each is a named list of
#'   examples, each a list of `counts`, `metadata` and `description` paths).
#' @export
discover_assets <- function(data_dir = NULL, gene_lengths = NULL) {

  if (is.null(data_dir)) {
    env <- Sys.getenv("TTMOUSE_DATA_DIR", unset = NA)
    data_dir <- if (is.na(env) || !nzchar(env)) NULL else env
  }
  if (is.null(data_dir) && is.null(gene_lengths)) {
    stop("No data directory: pass `data_dir` or set TTMOUSE_DATA_DIR ",
         "(must contain gene_lengths.rds and one subdirectory per organ).")
  }

  # --- Shared gene-length table --------------------------------------------
  gl <- if (!is.null(gene_lengths)) gene_lengths
        else file.path(data_dir, "gene_lengths.rds")
  if (!file.exists(gl)) {
    stop("Missing gene_lengths.rds: ", gl)
  }

  # --- One subdirectory per organ; keep those with >= 1 model file ----------
  # Example datasets are collected alongside, for the organs that are kept.
  model_files <- c(intergene = "intergene.rds", timecourse = "timecourse.rds")
  organs   <- list()
  examples <- list()
  if (!is.null(data_dir)) {
    subdirs <- list.dirs(data_dir, recursive = FALSE, full.names = TRUE)
    for (d in subdirs) {
      found <- list()
      for (type in names(model_files)) {
        p <- file.path(d, model_files[[type]])
        if (file.exists(p)) found[[type]] <- p
      }
      if (length(found) > 0L) {
        organs[[basename(d)]] <- found
        ex <- discover_examples(file.path(d, "examples"))
        if (length(ex) > 0L) examples[[basename(d)]] <- ex
      }
    }
  }

  if (length(organs) == 0L) {
    stop("No organ models found under '", data_dir,
         "' (expected <organ>/intergene.rds or <organ>/timecourse.rds).")
  }

  list(gene_lengths = gl, organs = organs, examples = examples)
}

#' Find the example datasets in one organ's `examples/` folder
#'
#' Returns a named list (one entry per example subdirectory) of the example's
#' file paths: `counts`, `metadata` and `description`. An absent folder gives an
#' empty list; an example missing any of the three files is an error.
#' @keywords internal
#' @noRd
discover_examples <- function(ex_dir) {
  if (!dir.exists(ex_dir)) return(list())

  files <- c(counts = "counts.xlsx", metadata = "metadata.xlsx",
             description = "description.md")
  examples <- list()
  for (d in list.dirs(ex_dir, recursive = FALSE, full.names = TRUE)) {
    paths <- file.path(d, files)
    if (!all(file.exists(paths))) {
      stop("Example '", basename(d), "' is incomplete: expected ",
           paste(files, collapse = ", "), " in ", d)
    }
    names(paths) <- names(files)
    examples[[basename(d)]] <- as.list(paths)
  }
  examples
}

#' Map a metadata table to project_test_data() arguments
#'
#' Converts a metadata data.frame (as returned by [read_metadata()], with
#' canonical column names) into the named argument list for [project_test_data()]:
#' whichever of `time`, `group_1/2/3`, `replicate` are present, mapped to the
#' corresponding `test_*` arguments.
#'
#' Verified against VadimVasilyev1994/TimeTeller-v2 (commit f3cbd44): the plain
#' `timecourse` normalisation derives its grouping from `test_group_1/2/3` +
#' `test_replicate` and errors ("No groups specified") if none are supplied, so
#' on that path at least one grouping column is required (`require_groups =
#' TRUE`). The intergene normalisation ignores grouping/time entirely (it
#' z-scores per sample), so there nothing is required (`require_groups = FALSE`);
#' any supplied columns are still stored and surface in `Results_df`. Neither
#' path uses `test_grouping_vars` (matched/combined only), so it is not produced.
#'
#' @param metadata A data.frame from [read_metadata()] (canonical columns).
#' @param require_groups If `TRUE` (default, time-course path), at least one
#'   grouping column (`group_1/2/3` or `replicate`) must be present. Set `FALSE`
#'   for the intergene path, where nothing is required.
#' @return A named list of [project_test_data()] arguments (any of `test_time`,
#'   `test_group_1/2/3`, `test_replicate`); possibly empty when nothing is
#'   present and `require_groups = FALSE`.
#' @export
metadata_to_projection_args <- function(metadata, require_groups = TRUE) {
  if (!is.data.frame(metadata)) {
    stop("`metadata` must be a data.frame (use read_metadata()).")
  }

  group_cols <- intersect(c("group_1", "group_2", "group_3", "replicate"),
                          colnames(metadata))
  if (require_groups && length(group_cols) == 0L) {
    stop("Timecourse projection needs at least one grouping column ",
         "(group_1/2/3 or replicate); none found in the metadata.")
  }

  args <- list()
  if ("time"      %in% colnames(metadata)) args$test_time      <- metadata[["time"]]
  if ("group_1"   %in% colnames(metadata)) args$test_group_1   <- metadata[["group_1"]]
  if ("group_2"   %in% colnames(metadata)) args$test_group_2   <- metadata[["group_2"]]
  if ("group_3"   %in% colnames(metadata)) args$test_group_3   <- metadata[["group_3"]]
  if ("replicate" %in% colnames(metadata)) args$test_replicate <- metadata[["replicate"]]
  args
}
