#' Prediction-relevant result columns exported by default
#'
#' The subset of `Test_Data$Results_df` written by [export_results()]. Verified
#' against VadimVasilyev1994/TimeTeller-v2 (commit f3cbd44), `second_peaks_fun()`
#' (mode = 'test'): the projection produces 26 columns; these are the
#' prediction-relevant ones (the rest are internal peak diagnostics).
#' `Actual_Time`, `Pred_Error` and the `Group_*`/`Replicate` columns are only
#' populated when test metadata was supplied.
#' @keywords internal
#' @noRd
RESULT_EXPORT_COLS <- c("time_1st_peak", "weighted_mean_time_pred", "Theta",
                        "Actual_Time", "Pred_Error",
                        "Group_1", "Group_2", "Group_3", "Replicate")

#' Export projection results to a CSV (prediction-relevant subset)
#'
#' Builds the per-sample prediction subset of `Test_Data$Results_df`: a leading
#' `Sample` column (the `Results_df` rownames, i.e. the sample names) followed by
#' whichever of `columns` are present. The 16 internal peak-diagnostic columns
#' are dropped. Columns that are absent on a given path (for example the
#' `Group_*`/`Actual_Time`/`Pred_Error` columns on the single-sample intergene
#' path, where no metadata was supplied) are quietly skipped rather than causing
#' an error, so the same call works for both projection paths.
#'
#' @param object A post-projection object from [project_test_data()].
#' @param path Output CSV path. If `NULL` (default), no file is written and the
#'   data.frame is returned for inspection or display.
#' @param columns Character vector of `Results_df` columns to keep, in order;
#'   defaults to the prediction-relevant subset.
#' @return Invisibly, the exported data.frame (`Sample` plus the present
#'   columns).
#' @export
export_results <- function(object, path = NULL, columns = RESULT_EXPORT_COLS) {
  results <- object[["Test_Data"]][["Results_df"]]
  if (is.null(results)) {
    stop("No Test_Data$Results_df found; run project_test_data() first.")
  }

  # Keep only the requested columns that actually exist on this path.
  present <- intersect(columns, colnames(results))

  out <- data.frame(Sample = rownames(results),
                    results[, present, drop = FALSE],
                    check.names      = FALSE,
                    stringsAsFactors = FALSE)

  if (!is.null(path)) {
    utils::write.csv(out, path, row.names = FALSE)
  }
  invisible(out)
}
