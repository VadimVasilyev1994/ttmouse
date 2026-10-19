# Results visualisation: a simple plot builder over the exported results table
# from export_results().
#
# results_plot_vars() -- list the columns usable for the x, y, colour, shape and
#                        facet selectors.
# plot_results()      -- scatter plot of one result column against another,
#                        optionally coloured, shaped and faceted by grouping
#                        columns, with a dashed y = x reference line when a
#                        predicted time is plotted against Actual_Time.
#
# Column roles follow RESULT_EXPORT_COLS (export_results.R). Columns absent on a
# given path (e.g. Actual_Time / Group_* on intergene without metadata) are simply
# not offered.

#' Numeric result columns (usable on either axis)
#' @keywords internal
#' @noRd
RESULT_NUMERIC_COLS <- c("time_1st_peak", "weighted_mean_time_pred", "Theta",
                         "Actual_Time", "Pred_Error")

#' Grouping result columns (usable for colour, shape and facets)
#' @keywords internal
#' @noRd
RESULT_GROUP_COLS <- c("Group_1", "Group_2", "Group_3", "Replicate")

#' Grouping columns also offered on the x axis
#' @keywords internal
#' @noRd
RESULT_X_GROUP_COLS <- c("Group_1", "Group_2", "Group_3")

#' Predicted-time columns that get a y = x line when plotted against Actual_Time
#' @keywords internal
#' @noRd
RESULT_PRED_TIME_COLS <- c("time_1st_peak", "weighted_mean_time_pred")

#' Point shapes for shape_by: distinct, mostly filled, in a readable order
#' @keywords internal
#' @noRd
RESULT_SHAPES <- c(16, 17, 15, 18, 1, 2, 0, 5, 3, 4, 8, 6)

#' Columns available for the results plot selectors
#'
#' Lists which columns of an [export_results()] table can be offered for each
#' selector of the results plot. Only columns that are present and carry
#' information are returned: numeric columns need at least one finite value,
#' grouping columns at least one non-empty value.
#'
#' @param results A data.frame from [export_results()].
#' @return A list of character vectors: `x` (`"Sample"`, then the informative
#'   `Group_1/2/3` columns, then the numeric columns), `y` (the numeric
#'   columns), and `colour`, `shape` and `facet` (the informative grouping
#'   columns, including `Replicate`; may be empty).
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
  x_group_cols <- intersect(RESULT_X_GROUP_COLS, group_cols)

  list(x      = c("Sample", x_group_cols, numeric_cols),
       y      = numeric_cols,
       colour = group_cols,
       shape  = group_cols,
       facet  = group_cols)
}

#' Plot one result column against another
#'
#' Scatter plot of the per-sample results, one point per sample, optionally
#' coloured, shaped and faceted by grouping columns. When `x` is `Actual_Time`
#' and `y` is a predicted time (`time_1st_peak` or `weighted_mean_time_pred`) a
#' dashed y = x line marks perfect prediction (in every facet). Times are
#' plotted as plain numbers (no wrapping at 24 h). Samples missing either value
#' are left out.
#'
#' When `x` is a grouping column, points are spread slightly sideways (a fixed,
#' reproducible jitter) so samples in the same group do not hide each other;
#' their y values are not changed.
#'
#' @param results A data.frame from [export_results()].
#' @param x Column for the x axis: `"Sample"` (one position per sample), a
#'   `Group_1/2/3` column (one position per group), or a numeric column, as
#'   listed by [results_plot_vars()].
#' @param y Numeric column for the y axis.
#' @param colour Optional grouping column to colour by; `NULL` or `""` for none.
#' @param shape Optional grouping column to set point shapes by (up to 12
#'   levels); `NULL` or `""` for none.
#' @param facet Optional grouping column to split the plot into panels, one per
#'   level; `NULL` or `""` for none.
#' @param interactive If `TRUE`, return an interactive plotly widget whose hover
#'   shows the sample name and values. Defaults to `FALSE` (a static ggplot).
#' @return A ggplot object, or a plotly object when `interactive = TRUE`.
#' @export
plot_results <- function(results, x, y, colour = NULL, shape = NULL,
                         facet = NULL, interactive = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_results() requires the 'ggplot2' package.")
  }

  # --- Check the requested columns against what this table offers ----------
  vars <- results_plot_vars(results)
  check_choice <- function(value, choices, what) {
    if (!value %in% choices) {
      stop("Cannot use '", value, "' for ", what, "; available: ",
           if (length(choices)) paste(choices, collapse = ", ") else "none", ".")
    }
  }
  # NULL or "" means "not used" for the optional selectors.
  is_used <- function(value) !is.null(value) && nzchar(value)

  check_choice(x, vars$x, "the x axis")
  check_choice(y, vars$y, "the y axis")
  use_colour <- is_used(colour)
  use_shape  <- is_used(shape)
  use_facet  <- is_used(facet)
  if (use_colour) check_choice(colour, vars$colour, "colour")
  if (use_shape)  check_choice(shape,  vars$shape,  "shape")
  if (use_facet)  check_choice(facet,  vars$facet,  "facets")

  # Grouping labels as character, with empty labels shown explicitly.
  group_labels <- function(col) {
    g <- as.character(results[[col]])
    g[is.na(g) | !nzchar(g)] <- "(unspecified)"
    g
  }

  # --- Assemble the plotting data ------------------------------------------
  samples   <- as.character(results$Sample)
  x_is_cat  <- x == "Sample" || x %in% RESULT_X_GROUP_COLS
  x_val <- if (x == "Sample") {
    factor(samples, levels = unique(samples))        # keep the upload order
  } else if (x_is_cat) {
    g <- group_labels(x)
    factor(g, levels = unique(g))                    # groups in upload order
  } else {
    suppressWarnings(as.numeric(results[[x]]))
  }
  df <- data.frame(sample = samples, x = x_val,
                   y = suppressWarnings(as.numeric(results[[y]])),
                   stringsAsFactors = FALSE)
  # Groups in upload order (not alphabetical), matching a group on the x axis,
  # so legends and facet panels list them in the order the user supplied.
  in_upload_order <- function(g) factor(g, levels = unique(g))
  if (use_colour) df$colour <- in_upload_order(group_labels(colour))
  if (use_shape)  df$shape  <- in_upload_order(group_labels(shape))
  if (use_facet)  df$facet  <- in_upload_order(group_labels(facet))

  # Leave out samples missing either value.
  x_ok <- if (is.numeric(df$x)) is.finite(df$x) else !is.na(df$x)
  keep <- x_ok & is.finite(df$y)
  df   <- df[keep, , drop = FALSE]
  if (nrow(df) == 0L) {
    stop("No samples have values for both '", x, "' and '", y, "'.")
  }

  # Shapes beyond the palette cannot be told apart, so stop clearly instead.
  if (use_shape && length(unique(df$shape)) > length(RESULT_SHAPES)) {
    stop("'", shape, "' has ", length(unique(df$shape)), " levels; shapes can ",
         "show at most ", length(RESULT_SHAPES), ". Use colour or facets instead.")
  }

  # Hover text for the interactive version: sample name plus the plotted values.
  x_shown  <- if (is.numeric(df$x)) signif(df$x, 4) else as.character(df$x)
  df$hover <- paste0("Sample: ", df$sample,
                     "<br>", x, ": ", x_shown,
                     "<br>", y, ": ", signif(df$y, 4))
  # Add each grouping column used for colour / shape / facets once: the same
  # column may drive several of them, or already be on the x axis.
  extra_cols <- setdiff(unique(c(if (use_colour) colour, if (use_shape) shape,
                                 if (use_facet) facet)), x)
  for (col in extra_cols) {
    df$hover <- paste0(df$hover, "<br>", col, ": ", group_labels(col)[keep])
  }

  # --- Build the plot -------------------------------------------------------
  # `text` is used only by plotly's tooltip; in the global mapping ggplot ignores it.
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, text = .data$hover))
  # Optional encodings, added to the plot-wide mapping.
  if (use_colour) p <- p + ggplot2::aes(colour = .data$colour)
  if (use_shape)  p <- p + ggplot2::aes(shape  = .data$shape)

  # Reference line for perfect prediction (drawn first so points sit on top).
  if (x == "Actual_Time" && y %in% RESULT_PRED_TIME_COLS) {
    p <- p + ggplot2::geom_abline(slope = 1, intercept = 0,
                                  linetype = "dashed", colour = "grey55")
  }

  # Points: a small fixed sideways jitter when x is a group, so samples in the
  # same group stay visible; single colour (app primary) unless coloured.
  position <- if (x_is_cat && x != "Sample") {
    ggplot2::position_jitter(width = 0.15, height = 0, seed = 1)
  } else {
    "identity"
  }
  p <- if (use_colour) {
    p + ggplot2::geom_point(size = 2.5, alpha = 0.85, position = position) +
      ggplot2::scale_colour_viridis_d(end = 0.92)
  } else {
    p + ggplot2::geom_point(size = 2.5, alpha = 0.85, colour = "#14506e",
                            position = position)
  }
  if (use_shape) p <- p + ggplot2::scale_shape_manual(values = RESULT_SHAPES)

  # One panel per level of the facet column, on shared axes for comparison.
  if (use_facet) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$facet))

  p <- p +
    ggplot2::labs(x = x, y = y,
                  colour = if (use_colour) colour else NULL,
                  shape  = if (use_shape)  shape  else NULL) +
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
