# Phase D plot wrappers.
#
# Thin wrappers around the four TimeTeller plotting functions. Each wrapper sets
# the agreed defaults (3D plots: density = FALSE so they stay pure-plotly and
# headless-safe; per-sample plots: logthresh defaults to the model's stored
# threshold), fails early on an invalid selector, and then calls the external
# function through a small internal seam so the TimeTeller dependency can be
# mocked in tests (as project.R does for test_model).
#
# Signatures verified against VadimVasilyev1994/TimeTeller-v2 (commit f3cbd44),
# Plotting_and_Diagnostics.R:
#   plot_3d_projection(object, selected_local_projection, density = FALSE, opacity = 0.05, sig_level = 0.90)
#   plot_3d_projection_with_test(object, selected_local_projection, density = FALSE, opacity = 0.05, sig_level = 0.90)
#   plot_raw_likelis(object, sample_num, logthresh, train_or_test = 'test')
#   plot_ind_curve(object, sample_num, logthresh, train_or_test = 'test')

# --- Internal seams to the external plot calls (mockable in tests) ----------

#' @keywords internal
#' @noRd
.call_plot_3d_projection <- function(...) {
  TimeTeller::plot_3d_projection(...)
}

#' @keywords internal
#' @noRd
.call_plot_3d_projection_with_test <- function(...) {
  TimeTeller::plot_3d_projection_with_test(...)
}

#' @keywords internal
#' @noRd
.call_plot_raw_likelis <- function(...) {
  TimeTeller::plot_raw_likelis(...)
}

#' @keywords internal
#' @noRd
.call_plot_ind_curve <- function(...) {
  TimeTeller::plot_ind_curve(...)
}

# --- Shared helpers ---------------------------------------------------------

# Validate a single local-projection selector against the model's choices.
.check_local_projection <- function(model, selected_local_projection) {
  choices <- local_projection_choices(model)
  if (length(selected_local_projection) != 1L ||
      !as.character(selected_local_projection) %in% choices) {
    stop("`selected_local_projection` must be one of: ",
         paste(choices, collapse = ", "), ".")
  }
  invisible(TRUE)
}

# Validate a single sample index against the available sample count.
.check_sample_num <- function(object, sample_num, train_or_test) {
  n <- n_samples(object, train_or_test)
  if (!is.numeric(sample_num) || length(sample_num) != 1L ||
      sample_num < 1 || sample_num > n) {
    stop("`sample_num` must be a single number in 1:", n, ".")
  }
  invisible(TRUE)
}

# --- 3D projection plots (plotly) -------------------------------------------

#' Plot the training projection in 3D
#'
#' Wraps TimeTeller's `plot_3d_projection()`: the model's own training data
#' projected onto a chosen local projection. Returns a plotly object.
#'
#' @param model A model loaded by [load_model()].
#' @param selected_local_projection One of [local_projection_choices()].
#' @param density Draw covariance ellipsoids? Kept `FALSE` by default so the
#'   plot stays pure-plotly (no `rgl`/OpenGL), which is headless-Docker-safe.
#' @param ... Forwarded to `plot_3d_projection()` (e.g. `opacity`, `sig_level`,
#'   used only when `density = TRUE`).
#' @return A plotly object.
#' @export
plot_training_projection <- function(model, selected_local_projection,
                                     density = FALSE, ...) {
  .check_local_projection(model, selected_local_projection)
  .call_plot_3d_projection(model,
                           selected_local_projection = selected_local_projection,
                           density = density, ...)
}

#' Plot the training projection with the test data overlaid, in 3D
#'
#' Wraps TimeTeller's `plot_3d_projection_with_test()`: the test samples
#' projected on top of the training projection. Requires a completed projection
#' (the test data must already be present in `object`). Returns a plotly object.
#'
#' @param object A post-projection object from [project_test_data()].
#' @param selected_local_projection One of [local_projection_choices()].
#' @param density Draw covariance ellipsoids? Kept `FALSE` by default (see
#'   [plot_training_projection()]).
#' @param ... Forwarded to `plot_3d_projection_with_test()`.
#' @return A plotly object.
#' @export
plot_test_projection <- function(object, selected_local_projection,
                                 density = FALSE, ...) {
  if (is.null(object[["Test_Data"]])) {
    stop("No Test_Data in object; run project_test_data() first.")
  }
  .check_local_projection(object, selected_local_projection)
  .call_plot_3d_projection_with_test(
    object,
    selected_local_projection = selected_local_projection,
    density = density, ...)
}

# --- Per-sample diagnostic plots (base graphics) ----------------------------

#' Plot a sample's raw truncated likelihood curves
#'
#' Wraps TimeTeller's `plot_raw_likelis()`. Draws to the active graphics device
#' (base graphics) and returns nothing useful. `logthresh` defaults to the
#' model's stored training threshold via [model_log_thresh()].
#'
#' @param object A post-projection object (for `"test"`) or a model (for
#'   `"train"`).
#' @param sample_num Sample index; one of `1:n_samples(object, train_or_test)`.
#' @param logthresh Log threshold for truncation; defaults to
#'   `model_log_thresh(object)`.
#' @param train_or_test Plot the `"test"` (default) or `"train"` sample.
#' @return Invisibly `NULL` (called for its plot side effect).
#' @export
plot_sample_likelihoods <- function(object, sample_num,
                                    logthresh = model_log_thresh(object),
                                    train_or_test = c("test", "train")) {
  train_or_test <- match.arg(train_or_test)
  if (is.null(logthresh) || !is.numeric(logthresh) || length(logthresh) != 1L) {
    stop("`logthresh` must be a single numeric value ",
         "(default is the model's stored Train_Data$LogThresh_Train).")
  }
  .check_sample_num(object, sample_num, train_or_test)

  # Annotate with the sample's metadata when available. Only touch par()/draw
  # the banner if there is a label, so the no-metadata path is unchanged.
  label <- sample_label(object, sample_num, train_or_test)
  if (nzchar(label)) {
    op <- graphics::par(oma = c(0, 0, 2.5, 0))
    on.exit(graphics::par(op), add = TRUE)
  }

  res <- .call_plot_raw_likelis(object, sample_num = sample_num,
                                logthresh = logthresh,
                                train_or_test = train_or_test)
  if (nzchar(label)) {
    graphics::mtext(paste0("(", label, ")"), outer = TRUE, side = 3,
                    line = 0.8, cex = 0.9)
  }
  invisible(res)
}

#' Plot a sample's theta-calculation curves
#'
#' Wraps TimeTeller's `plot_ind_curve()`: the per-sample theta / flat-likelihood
#' diagnostic. Base graphics; returns the plot as a `recordPlot()` object (as the
#' underlying function does). `logthresh` defaults to the model's stored
#' threshold via [model_log_thresh()].
#'
#' @param object A post-projection object (for `"test"`) or a model (for
#'   `"train"`).
#' @param sample_num Sample index; one of `1:n_samples(object, train_or_test)`.
#' @param logthresh Log threshold; defaults to `model_log_thresh(object)`.
#' @param train_or_test Plot the `"test"` (default) or `"train"` sample.
#' @return A recorded plot (`recordPlot()`), as returned by the underlying
#'   function.
#' @export
plot_sample_curve <- function(object, sample_num,
                              logthresh = model_log_thresh(object),
                              train_or_test = c("test", "train")) {
  train_or_test <- match.arg(train_or_test)
  if (is.null(logthresh) || !is.numeric(logthresh) || length(logthresh) != 1L) {
    stop("`logthresh` must be a single numeric value ",
         "(default is the model's stored Train_Data$LogThresh_Train).")
  }
  .check_sample_num(object, sample_num, train_or_test)

  # Annotate with the sample's metadata when available (see
  # plot_sample_likelihoods). The banner is drawn on the active device after the
  # underlying plot; the returned recordPlot() reflects the underlying function
  # and does not include the banner.
  label <- sample_label(object, sample_num, train_or_test)
  if (nzchar(label)) {
    op <- graphics::par(oma = c(0, 0, 2.5, 0))
    on.exit(graphics::par(op), add = TRUE)
  }

  res <- .call_plot_ind_curve(object, sample_num = sample_num,
                              logthresh = logthresh,
                              train_or_test = train_or_test)
  if (nzchar(label)) {
    graphics::mtext(paste0("(", label, ")"), outer = TRUE, side = 3,
                    line = 0.8, cex = 0.9)
  }
  res
}
