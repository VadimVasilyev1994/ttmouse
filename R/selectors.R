#' Available local-projection choices for a model
#'
#' Lists the local projections a model offers, for populating a selector (e.g. a
#' Shiny dropdown) of valid `selected_local_projection` values.
#'
#' Verified against VadimVasilyev1994/TimeTeller-v2 (commit f3cbd44): the plot
#' functions build `paste0('Time_', selected_local_projection)` and index
#' `object$Projections$All_Projections` with it. Each local projection is stored
#' there under a `Time_<label>` name, so the valid selector values are those
#' names with the `Time_` prefix removed.
#'
#' @param model A model loaded by [load_model()] (or a post-projection object;
#'   both carry `Projections$All_Projections`).
#' @return A character vector of local-projection labels (the `Time_` prefix
#'   stripped), each usable as a `selected_local_projection` value.
#' @export
local_projection_choices <- function(model) {
  nms <- names(model[["Projections"]][["All_Projections"]])
  if (is.null(nms)) {
    stop("Model has no Projections$All_Projections; not a usable TimeTeller model.")
  }
  # Strip the stored `Time_` prefix; the plot functions re-add it internally.
  sub("^Time_", "", nms)
}

#' Number of samples available to the per-sample plots
#'
#' Returns how many samples the per-sample diagnostic plots
#' ([plot_sample_likelihoods()], [plot_sample_curve()]) can index, for
#' populating a sample selector. The valid `sample_num` values are `1:n`.
#'
#' Verified against VadimVasilyev1994/TimeTeller-v2 (commit f3cbd44):
#' `plot_raw_likelis()` indexes `likelis_array[, sample_num, ]`, where the array
#' is `Test_Data$Test_Likelihood_Array` (test) or
#' `Train_Data$Train_Likelihood_Array` (train); the sample axis is the 2nd
#' dimension.
#'
#' @param object A post-projection object (for `"test"`) or a model (for
#'   `"train"`).
#' @param train_or_test Which likelihood array to size; `"test"` (default) or
#'   `"train"`.
#' @return Integer count of samples (valid `sample_num` is `1:n`).
#' @export
n_samples <- function(object, train_or_test = c("test", "train")) {
  train_or_test <- match.arg(train_or_test)

  arr <- if (train_or_test == "test") {
    object[["Test_Data"]][["Test_Likelihood_Array"]]
  } else {
    object[["Train_Data"]][["Train_Likelihood_Array"]]
  }

  if (is.null(arr)) {
    stop("No ", train_or_test, " likelihood array found",
         if (train_or_test == "test") " (has the projection been run?)" else "",
         ".")
  }
  dim(arr)[2]
}
