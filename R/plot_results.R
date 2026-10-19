# Results visualisation: a simple x / y / colour plot builder over the exported
# results table from export_results().
#
# results_plot_vars() -- list the columns usable for the x, y and colour selectors.
# plot_results()      -- scatter plot of one result column against another, optionally
#                        coloured by a grouping column, with a dashed y = x reference
#                        line when a predicted time is plotted against Actual_Time.
#
# Column roles follow RESULT_EXPORT_COLS (export_results.R). Columns absent on a
# given path (e.g. Actual_Time / Group_* on intergene without metadata) are simply
# not offered.

#' Numeric result columns (usable on either axis)
#' @keywords internal
#' @noRd
RESULT_NUMERIC_COLS <- c("time_1st_peak", "weighted_mean_time_pred", "Theta",
                         "Actual_Time", "Pred_Error")

#' Grouping result columns (usable for colour)
#' @keywords internal
#' @noRd
RESULT_GROUP_COLS <- c("Group_1", "Group_2", "Group_3", "Replicate")

#' Predicted-time columns that get a y = x line when plotted against Actual_Time
#' @keywords internal
#' @noRd
RESULT_PRED_TIME_COLS <- c("time_1st_peak", "weighted_mean_time_pred")

#' Columns available for the results plot selectors
#'
#' Lists which columns of an [export_results()] table can be offered for each
#' selector of the results plot. Only columns that are present and carry
#' information are returned: numeric columns need at least one finite value,
#' grouping columns at least one non-empty value.
#'
#' @param results A data.frame from [export_results()].
#' @return A list of character vectors: `x` (`"Sample"` followed by the numeric
#'   columns), `y` (the numeric columns) and `colour` (the grouping columns; may
#'   be empty).
#' @export
results_plot_vars <- function(results) {
  if (!is.data.frame(results)) {
    stop("`results` must be a data.frame (use export_results()).")
  }

  # Numeric columns with at least one usable value.
  has_numbers <- function(col) {
    col %in% colnames(results) &&
      any(is.finite(suppressWarnings(as.numeric(results[[col]]))))
  }
  # Grouping columns with at least one non-empty value.
  has_labels <- function(col) {
    if (!col %in% colnames(results)) return(FALSE)
    v <- as.character(results[[col]])
    any(!is.na(v) & nzchar(v))
  }

  numeric_cols <- Filter(has_numbers, RESULT_NUMERIC_COLS)
  group_cols   <- Filter(has_labels,  RESULT_GROUP_COLS)

  list(x = c("Sample", numeric_cols), y = numeric_cols, colour = group_cols)
}

#' Plot one result column against another
#'
#' Scatter plot of the per-sample results, one point per sample, optionally
#' coloured by a grouping column. When `x` is `Actual_Time` and `y` is a
#' predicted time (`time_1st_peak` or `weighted_mean_time_pred`) a dashed
#' y = x line marks perfect prediction. Times are plotted as plain numbers
#' (no wrapping at 24 h). Samples missing either value are left out.
#'
#' @param results A data.frame from [export_results()].
#' @param x Column for the x axis: `"Sample"` (one position per sample) or one
#'   of the numeric columns listed by [results_plot_vars()].
#' @param y Numeric column for the y axis.
#' @param colour Optional grouping column to colour by; `NULL` or `""` for none.
#' @param interactive If `TRUE`, return an interactive plotly widget whose hover
#'   shows the sample name and values. Defaults to `FALSE` (a static ggplot).
#' @return A ggplot object, or a plotly object when `interactive = TRUE`.
#' @export
plot_results <- function(results, x, y, colour = NULL, interactive = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_results() requires the 'ggplot2' package.")
  }

  # --- Check the requested columns against what this table offers ----------
  vars <- results_plot_vars(results)
  if (!x %in% vars$x) {
    stop("Cannot plot '", x, "' on the x axis; available: ",
         paste(vars$x, collapse = ", "), ".")
  }
  if (!y %in% vars$y) {
    stop("Cannot plot '", y, "' on the y axis; available: ",
         paste(vars$y, collapse = ", "), ".")
  }
  use_colour <- !is.null(colour) && nzchar(colour)
  if (use_colour && !colour %in% vars$colour) {
    stop("Cannot colour by '", colour, "'; available: ",
         if (length(vars$colour)) paste(vars$colour, collapse = ", ") else "none",
         ".")
  }

  # --- Assemble the plotting data ------------------------------------------
  samples <- as.character(results$Sample)
  x_val <- if (x == "Sample") {
    factor(samples, levels = unique(samples))       # keep the upload order
  } else {
    suppressWarnings(as.numeric(results[[x]]))
  }
  df <- data.frame(sample = samples, x = x_val,
                   y = suppressWarnings(as.numeric(results[[y]])),
                   stringsAsFactors = FALSE)
  if (use_colour) {
    grp <- as.character(results[[colour]])
    grp[is.na(grp) | !nzchar(grp)] <- "(unspecified)"
    df$colour <- grp
  }

  # Leave out samples missing either value.
  x_ok <- if (is.numeric(df$x)) is.finite(df$x) else !is.na(df$x)
  df   <- df[x_ok & is.finite(df$y), , drop = FALSE]
  if (nrow(df) == 0L) {
    stop("No samples have values for both '", x, "' and '", y, "'.")
  }

  # Hover text for the interactive version: sample name plus the plotted values.
  df$hover <- paste0("Sample: ", df$sample,
                     "<br>", x, ": ", if (x == "Sample") df$sample else signif(df$x, 4),
                     "<br>", y, ": ", signif(df$y, 4))
  if (use_colour) df$hover <- paste0(df$hover, "<br>", colour, ": ", df$colour)

  # --- Build the plot -------------------------------------------------------
  # `text` is used only by plotly's tooltip; in the global mapping ggplot ignores it.
  p <- if (use_colour) {
    ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y,
                                     colour = .data$colour, text = .data$hover))
  } else {
    ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, text = .data$hover))
  }

  # Reference line for perfect prediction (drawn first so points sit on top).
  if (x == "Actual_Time" && y %in% RESULT_PRED_TIME_COLS) {
    p <- p + ggplot2::geom_abline(slope = 1, intercept = 0,
                                  linetype = "dashed", colour = "grey55")
  }

  p <- if (use_colour) {
    p + ggplot2::geom_point(size = 2.5, alpha = 0.85) +
      ggplot2::scale_colour_viridis_d(end = 0.92)
  } else {
    p + ggplot2::geom_point(size = 2.5, alpha = 0.85, colour = "#14506e")  # app primary
  }

  p <- p +
    ggplot2::labs(x = x, y = y, colour = if (use_colour) colour else NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "right",
                   panel.grid.minor = ggplot2::element_blank())
  if (x == "Sample") {
    # Sample names can be long: angle them so they stay readable.
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  if (!interactive) return(p)

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("interactive = TRUE requires the 'plotly' package.")
  }
  plotly::ggplotly(p, tooltip = "text")
}
