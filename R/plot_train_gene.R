# Training-data gene-expression visualisation.
#
# train_genes()      -- list the model's genes, labelled by symbol, for a selector.
# train_group_vars() -- list the informative grouping columns, for a selector.
# plot_train_gene()  -- plot one gene's expression over time, one series per group,
#                       with standard-error bars across replicates.
#
# Data source verified against VadimVasilyev1994/TimeTeller-v2 (commit f3cbd44),
# make_data_object(): object$Train$Data is a tidy data.frame with one Gene_<id>
# column per gene plus Group (= group_1), Group_2, Group_3, Time (factor) and
# Replicate; object$Metadata$Train$Time holds the numeric times (row-aligned with
# Train$Data); object$Metadata$Train$Genes_Used holds the Ensembl IDs.

#' List the training genes (for a gene selector), labelled by symbol
#'
#' Returns the genes used by the model as a named character vector mapping a
#' display label (gene symbol where available, otherwise the Ensembl ID) to the
#' Ensembl ID -- suitable for a Shiny `selectInput` where the user picks by
#' symbol but the value passed on is the Ensembl ID.
#'
#' @param model A model from [load_model()].
#' @param gene_lengths Optional gene-length table (with `gene_id`, `gene_name`
#'   columns) used to map Ensembl IDs to symbols. When `NULL`, Ensembl IDs are
#'   used as the labels.
#' @return Named character vector: names = display labels, values = Ensembl IDs.
#' @export
train_genes <- function(model, gene_lengths = NULL) {
  ids <- model[["Metadata"]][["Train"]][["Genes_Used"]]
  if (is.null(ids)) {
    stop("Model has no Metadata$Train$Genes_Used; not a usable training object.")
  }

  labels <- ids
  if (!is.null(gene_lengths) &&
      all(c("gene_id", "gene_name") %in% colnames(gene_lengths))) {
    sym <- gene_lengths$gene_name[match(ids, gene_lengths$gene_id)]
    labels <- ifelse(is.na(sym) | !nzchar(sym), ids, sym)
  }
  stats::setNames(ids, labels)
}

#' Available grouping variables in the training data (for a group selector)
#'
#' Returns the grouping columns that carry information, as a named vector mapping
#' a friendly label (`group_1`/`group_2`/`group_3`) to the column name in
#' `Train$Data` (`Group`/`Group_2`/`Group_3`). Only columns with at least one
#' non-empty value are returned; `group_1` leads when present.
#'
#' @param model A model from [load_model()].
#' @return Named character vector: names = labels, values = column names.
#' @export
train_group_vars <- function(model) {
  data <- model[["Train"]][["Data"]]
  if (is.null(data)) {
    stop("Model has no Train$Data; not a usable training object.")
  }

  map <- c(group_1 = "Group", group_2 = "Group_2", group_3 = "Group_3")
  has_info <- vapply(map, function(col) {
    col %in% colnames(data) &&
      length(unique(data[[col]][nzchar(as.character(data[[col]]))])) > 0L
  }, logical(1))

  out <- map[has_info]
  if (length(out) == 0L) out <- c(group_1 = "Group")   # fallback
  out
}

#' Plot a training gene's expression over time, grouped, with replicate SE bars
#'
#' Plots one gene's expression across training time, with one series per level of
#' the chosen grouping variable and error bars showing the standard error across
#' replicates at each time point. Uses the tidy training data in `Train$Data` and
#' the numeric times in `Metadata$Train$Time`.
#'
#' @param model A model from [load_model()].
#' @param gene Ensembl ID of the gene to plot (must be among the training genes).
#' @param group_var Column in `Train$Data` to group by: `"Group"` (group_1,
#'   default), `"Group_2"`, or `"Group_3"`.
#' @param gene_label Optional display label for the title (e.g. a gene symbol);
#'   defaults to `gene`.
#' @param interactive If `TRUE`, wrap the plot with `plotly::ggplotly()` and
#'   return an interactive plotly widget whose legend toggles groups on click
#'   (double-click to isolate one). Defaults to `FALSE` (a static ggplot).
#' @return A ggplot object, or a plotly object when `interactive = TRUE`.
#' @export
plot_train_gene <- function(model, gene, group_var = "Group", gene_label = NULL,
                            interactive = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_train_gene() requires the 'ggplot2' package.")
  }

  data <- model[["Train"]][["Data"]]
  if (is.null(data)) stop("Model has no Train$Data; not a usable training object.")

  gene_col <- paste0("Gene_", gene)
  if (!gene_col %in% colnames(data)) {
    stop("Gene '", gene, "' is not among the training genes.")
  }
  if (!group_var %in% colnames(data)) {
    stop("Grouping variable '", group_var, "' not found in the training data.")
  }

  # Numeric times, row-aligned with Train$Data (both built from the training
  # samples in column order).
  time_num <- as.numeric(model[["Metadata"]][["Train"]][["Time"]])

  df <- data.frame(
    expression = as.numeric(data[[gene_col]]),
    group      = as.character(data[[group_var]]),
    time       = time_num,
    stringsAsFactors = FALSE
  )
  # Empty group labels -> a single, clearly-named series.
  df$group[!nzchar(df$group)] <- "(unspecified)"

  # Mean +/- standard error across replicates at each group x time.
  agg <- stats::aggregate(
    expression ~ group + time, data = df,
    FUN = function(x) c(mean = mean(x), se = stats::sd(x) / sqrt(length(x))))
  summ <- data.frame(group = agg$group, time = agg$time,
                     mean  = agg$expression[, "mean"],
                     se    = agg$expression[, "se"])
  summ$se[is.na(summ$se)] <- 0   # single replicate -> SE undefined -> no bar

  ttl <- if (is.null(gene_label)) gene else gene_label

  p <- ggplot2::ggplot(summ, ggplot2::aes(x = .data$time, y = .data$mean,
                                          colour = .data$group, group = .data$group)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$mean - .data$se, ymax = .data$mean + .data$se),
      width = 0.6, linewidth = 0.5) +
    ggplot2::scale_colour_viridis_d(end = 0.92) +
    ggplot2::labs(title = ttl, x = "Time", y = "Expression", colour = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "right",
                   panel.grid.minor = ggplot2::element_blank())

  if (!interactive) return(p)

  # Interactive: legend entries toggle groups on click (double-click isolates).
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("interactive = TRUE requires the 'plotly' package.")
  }
  plotly::ggplotly(p)
}
