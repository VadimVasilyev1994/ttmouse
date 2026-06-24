#' Check that all model-required genes are present, before projection
#'
#' `test_model()` hard-errors if any required gene is absent
#' (Testing_functions.R:67). `validate_genes()` performs the same check up front
#' and returns a structured report (counts + the missing IDs) so the app can show
#' a clear message and block projection gracefully. It never normalises or
#' projects, and never stops on missing genes (it reports them); it only stops on
#' malformed input.
#'
#' @param model A model loaded by [load_model()].
#' @param expr_matrix The preprocessed matrix (output of [preprocess_counts()]):
#'   genes in rows, samples in columns, rownames in the model's ID space.
#' @param max_show Maximum number of missing IDs to list in the `summary` string.
#'
#' @return A list with `ok`, `n_required`, `n_present`, `n_missing`, the full
#'   `missing` vector, and a display `summary` string.
#' @export
validate_genes <- function(model, expr_matrix, max_show = 20) {
  # --- Guard the inputs (these are programming errors, not missing-gene cases) -
  if (!is.matrix(expr_matrix) && !is.data.frame(expr_matrix)) {
    stop("`expr_matrix` must be a matrix or data.frame (genes x samples).")
  }
  if (is.null(rownames(expr_matrix))) {
    stop("`expr_matrix` must have rownames (gene IDs) to validate against the model.")
  }

  required <- model_genes(model)
  missing  <- required[!(required %in% rownames(expr_matrix))]

  n_required <- length(required)
  n_missing  <- length(missing)
  ok         <- n_missing == 0L

  # One-line, human-readable summary; the missing list is capped for display
  # while the full vector is always returned in `missing`.
  if (ok) {
    summary <- sprintf("All %d required genes present.", n_required)
  } else {
    shown <- paste(utils::head(missing, max_show), collapse = ", ")
    more  <- if (n_missing > max_show) {
      sprintf(" ... and %d more", n_missing - max_show)
    } else {
      ""
    }
    summary <- sprintf("%d of %d required genes missing: %s%s",
                       n_missing, n_required, shown, more)
  }

  list(
    ok         = ok,                       # TRUE only if nothing is missing
    n_required = n_required,               # genes the model needs
    n_present  = n_required - n_missing,   # of those, how many were supplied
    n_missing  = n_missing,
    missing    = missing,                  # full vector of missing gene IDs
    summary    = summary                   # display string (missing list capped)
  )
}
