#' Normalisation methods requiring per-group structure at projection time
#' @keywords internal
#' @noRd
TT_GROUP_METHODS <- c("timecourse", "timecourse_matched", "combined")

#' Internal seam to the external projection call
#'
#' Wraps `TimeTeller::test_model()` so the external dependency can be mocked in
#' tests (via `testthat::local_mocked_bindings(.call_test_model = ...)`) without
#' the TimeTeller package installed. Not part of the public API.
#' @keywords internal
#' @noRd
.call_test_model <- function(...) {
  TimeTeller::test_model(...)
}

#' Project a preprocessed matrix onto a pre-built TimeTeller model
#'
#' Runs the model on a user's preprocessed matrix and returns the updated
#' TimeTeller object (predictions in `object$Test_Data$Results_df`, e.g.
#' `time_1st_peak`, `time_2nd_peak`, `Theta`).
#'
#' Verified behaviour (against VadimVasilyev1994/TimeTeller-v2, master):
#' `mat_normalised_test` is forced `TRUE` because the matrix is already on the
#' model scale (log2(TPM+1)); `log_thresh` defaults to the model's stored
#' training threshold so it is applied automatically; and unsupplied optional
#' arguments are *omitted* (not passed as `NULL`) because `add_test_data()` uses
#' `missing()` to default them to `NA`. `intergene`/`clr` models need no groups;
#' the timecourse family requires grouping information.
#'
#' @param model A model loaded by [load_model()].
#' @param expr_matrix Preprocessed matrix from [preprocess_counts()].
#' @param test_group_1,test_group_2,test_group_3,test_replicate Optional grouping
#'   vectors (required for the timecourse family; omit for intergene/clr).
#' @param test_time Optional known sample times (for validation).
#' @param test_grouping_vars Optional; used by the timecourse_matched/combined
#'   methods.
#' @param log_thresh Log threshold; defaults to `model_log_thresh(model)`.
#' @param verbose If `TRUE`, print a one-line summary.
#'
#' @return The updated TimeTeller object (results in `Test_Data$Results_df`).
#' @export
project_test_data <- function(model, expr_matrix,
                              test_group_1       = NULL,
                              test_group_2       = NULL,
                              test_group_3       = NULL,
                              test_replicate     = NULL,
                              test_time          = NULL,
                              test_grouping_vars = NULL,
                              log_thresh = model_log_thresh(model),
                              verbose    = FALSE) {

  # --- Input guards --------------------------------------------------------
  if (!is.matrix(expr_matrix) && !is.data.frame(expr_matrix)) {
    stop("`expr_matrix` must be a matrix or data.frame (genes x samples).")
  }
  if (is.null(log_thresh) || !is.numeric(log_thresh) || length(log_thresh) != 1L) {
    stop("`log_thresh` must be a single numeric value ",
         "(the default is the model's stored Train_Data$LogThresh_Train).")
  }

  # --- Group-based models need grouping information ------------------------
  # Fail early and clearly rather than deep inside normalise_test_data().
  method <- model_normalisation(model)
  if (method %in% TT_GROUP_METHODS) {
    has_group <- any(!vapply(
      list(test_group_1, test_group_2, test_group_3, test_replicate),
      is.null, logical(1)))
    if (!has_group) {
      stop("Model normalisation '", method, "' requires per-group structure; ",
           "supply at least one of test_group_1/2/3 or test_replicate ",
           "(e.g. a time-series grouping).")
    }
  }

  # --- Assemble the test_model() call, omitting unsupplied optional args ---
  # Passing NULL would defeat add_test_data()'s missing() defaults, so only
  # include the optional arguments the caller actually provided.
  args <- list(object              = model,
               exp_matrix          = expr_matrix,
               mat_normalised_test = TRUE,        # app already produced model scale
               log_thresh          = log_thresh)
  if (!is.null(test_group_1))       args$test_group_1       <- test_group_1
  if (!is.null(test_group_2))       args$test_group_2       <- test_group_2
  if (!is.null(test_group_3))       args$test_group_3       <- test_group_3
  if (!is.null(test_replicate))     args$test_replicate     <- test_replicate
  if (!is.null(test_time))          args$test_time          <- test_time
  if (!is.null(test_grouping_vars)) args$test_grouping_vars <- test_grouping_vars

  if (verbose) {
    cat(sprintf("Projecting %d sample(s) onto '%s' model (log_thresh = %g)\n",
                ncol(expr_matrix), method, log_thresh))
  }

  # Calls TimeTeller::test_model() via the mockable seam. Returns the updated
  # object; predictions are in object$Test_Data$Results_df.
  do.call(.call_test_model, args)
}
