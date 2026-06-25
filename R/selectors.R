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

#' Build a metadata label for a sample (for plot annotation)
#'
#' Returns a short, human-readable string of the metadata stored for one sample
#' -- whichever of `group_1/2/3`, `time`, `replicate` are present (non-NA) -- for
#' annotating the per-sample plots. Returns `""` when no metadata is available
#' (e.g. the intergene path with no metadata file), so callers can skip the
#' annotation.
#'
#' Verified against VadimVasilyev1994/TimeTeller-v2 (commit f3cbd44),
#' `add_test_data()`: the supplied metadata is stored under
#' `object$Metadata$Test$Group_1`/`Group_2`/`Group_3`/`Time`/`Replicate`, each a
#' vector of length n_samples (NA-filled when not supplied).
#'
#' @param object A post-projection object from [project_test_data()].
#' @param sample_num Sample index (1-based).
#' @param train_or_test Which metadata block to read; `"test"` (default) or
#'   `"train"`.
#' @return A single string like `"group_1: Adrenal | group_2: ALF | time: 0"`,
#'   or `""` if nothing is available.
#' @export
sample_label <- function(object, sample_num, train_or_test = c("test", "train")) {
  train_or_test <- match.arg(train_or_test)
  block <- if (train_or_test == "test") object[["Metadata"]][["Test"]]
           else object[["Metadata"]][["Train"]]
  if (is.null(block)) return("")

  # Canonical label -> stored slot name; kept in a fixed display order.
  fields <- c(group_1 = "Group_1", group_2 = "Group_2", group_3 = "Group_3",
              time = "Time", replicate = "Replicate")

  parts <- character(0)
  for (lab in names(fields)) {
    vec <- block[[fields[[lab]]]]
    if (is.null(vec) || sample_num > length(vec)) next
    val <- vec[sample_num]
    if (is.null(val) || is.na(val) || !nzchar(trimws(as.character(val)))) next
    parts <- c(parts, paste0(lab, ": ", trimws(as.character(val))))
  }
  paste(parts, collapse = " | ")
}
