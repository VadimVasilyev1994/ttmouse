# test-plot_results.R
#
# Tests for the results plot builder. Uses plain data.frames shaped like the
# export_results() output, so it needs neither TimeTeller nor a real projection.
# Plots are built and rendered to a headless device to confirm they are valid.

library(testthat)

# Timecourse-style results: metadata supplied, so Actual_Time / Group_* exist.
make_tc_results <- function() {
  data.frame(
    Sample                  = paste0("s", 1:6),
    time_1st_peak           = c(1, 7, 13, 19, 2, 8),
    weighted_mean_time_pred = c(0.5, 6.5, 12.5, 18.5, 1.5, 7.5),
    Theta                   = c(0.1, 0.2, 0.15, 0.3, 0.25, 0.05),
    Actual_Time             = c(0, 6, 12, 18, 0, 6),
    Pred_Error              = c(0.5, 0.5, 0.5, 0.5, 1.5, 1.5),
    Group_1                 = c("WT", "WT", "WT", "WT", "KO", "KO"),
    Group_2                 = rep("", 6),                 # present but empty
    Replicate               = c("1", "1", "1", "1", "2", "2"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

# Intergene-style results without metadata: only the three prediction columns.
make_ig_results <- function() {
  data.frame(Sample = c("a", "b"),
             time_1st_peak = c(3, 15),
             weighted_mean_time_pred = c(3.2, 14.8),
             Theta = c(0.2, 0.4),
             stringsAsFactors = FALSE)
}

# Render a ggplot to a throwaway device; errors if the plot is not drawable.
render_ok <- function(p) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path)
  on.exit(grDevices::dev.off())
  print(p)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# results_plot_vars()
# ---------------------------------------------------------------------------

test_that("results_plot_vars lists informative columns on the timecourse path", {
  v <- results_plot_vars(make_tc_results())
  # x: Sample, informative Group_1/2/3 (not Replicate), then numeric columns.
  expect_equal(v$x, c("Sample", "Group_1", "time_1st_peak", "weighted_mean_time_pred",
                      "Theta", "Actual_Time", "Pred_Error"))
  expect_equal(v$y, c("time_1st_peak", "weighted_mean_time_pred",
                      "Theta", "Actual_Time", "Pred_Error"))
  expect_equal(v$colour, c("Group_1", "Replicate"))   # empty Group_2 left out
  expect_equal(v$shape,  c("Group_1", "Replicate"))
  expect_equal(v$facet,  c("Group_1", "Replicate"))
})

test_that("results_plot_vars offers only what exists on the intergene path", {
  v <- results_plot_vars(make_ig_results())
  expect_equal(v$x, c("Sample", "time_1st_peak", "weighted_mean_time_pred", "Theta"))
  expect_equal(v$colour, character(0))
  expect_equal(v$shape,  character(0))
  expect_equal(v$facet,  character(0))
})

test_that("an all-NA numeric column is not offered", {
  res <- make_tc_results()
  res$Actual_Time <- NA
  expect_false("Actual_Time" %in% results_plot_vars(res)$x)
})

test_that("results_plot_vars rejects non-data.frames", {
  expect_error(results_plot_vars(list()), "data.frame")
})

# ---------------------------------------------------------------------------
# plot_results()
# ---------------------------------------------------------------------------

test_that("a coloured scatter plot is built and renders", {
  p <- plot_results(make_tc_results(), x = "Theta", y = "Pred_Error",
                    colour = "Group_1")
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 6L)
  expect_true(render_ok(p))
})

test_that("no colour (NULL or \"\") gives a single-colour plot", {
  p1 <- plot_results(make_ig_results(), x = "Sample", y = "Theta")
  p2 <- plot_results(make_ig_results(), x = "Sample", y = "Theta", colour = "")
  expect_false("colour" %in% colnames(p1$data))
  expect_false("colour" %in% colnames(p2$data))
  expect_true(render_ok(p1))
})

test_that("Sample on x keeps the upload order", {
  res <- make_ig_results()
  res$Sample <- c("z_last", "a_first")
  p <- plot_results(res, x = "Sample", y = "Theta")
  expect_equal(levels(p$data$x), c("z_last", "a_first"))
})

test_that("a y = x line is added only for predicted time against Actual_Time", {
  has_abline <- function(p) {
    any(vapply(p$layers, function(l) inherits(l$geom, "GeomAbline"), logical(1)))
  }
  res <- make_tc_results()
  expect_true(has_abline(plot_results(res, "Actual_Time", "weighted_mean_time_pred")))
  expect_true(has_abline(plot_results(res, "Actual_Time", "time_1st_peak")))
  expect_false(has_abline(plot_results(res, "Actual_Time", "Theta")))
  expect_false(has_abline(plot_results(res, "Theta", "weighted_mean_time_pred")))
})

test_that("samples missing a value are left out", {
  res <- make_tc_results()
  res$Actual_Time[2] <- NA
  p <- plot_results(res, x = "Actual_Time", y = "Theta")
  expect_equal(nrow(p$data), 5L)
  expect_false("s2" %in% p$data$sample)
})

test_that("empty group labels are shown as (unspecified)", {
  res <- make_tc_results()
  res$Group_1[1] <- ""
  p <- plot_results(res, x = "Theta", y = "Pred_Error", colour = "Group_1")
  expect_true("(unspecified)" %in% levels(p$data$colour))
})

test_that("unavailable columns give clear errors", {
  res <- make_ig_results()
  expect_error(plot_results(res, x = "Actual_Time", y = "Theta"), "for the x axis")
  expect_error(plot_results(res, x = "Group_1", y = "Theta"), "for the x axis")
  expect_error(plot_results(res, x = "Sample", y = "Group_1"), "for the y axis")
  expect_error(plot_results(res, x = "Sample", y = "Theta", colour = "Group_1"),
               "for colour; available: none")
  expect_error(plot_results(res, x = "Sample", y = "Theta", shape = "Group_1"),
               "for shape")
  expect_error(plot_results(res, x = "Sample", y = "Theta", facet = "Group_1"),
               "for facets")
})

test_that("hover text carries the sample name and values", {
  p <- plot_results(make_tc_results(), x = "Actual_Time", y = "Theta",
                    colour = "Group_1")
  expect_match(p$data$hover[1], "Sample: s1")
  expect_match(p$data$hover[1], "Actual_Time: 0")
  expect_match(p$data$hover[1], "Group_1: WT")
})

test_that("interactive = TRUE returns a plotly object", {
  skip_if_not_installed("plotly")
  w <- plot_results(make_tc_results(), x = "Actual_Time",
                    y = "weighted_mean_time_pred", colour = "Group_1",
                    interactive = TRUE)
  expect_s3_class(w, "plotly")
  # Building the widget exercises the ggplot -> plotly conversion end to end.
  expect_silent(plotly::plotly_build(w))
})

# ---------------------------------------------------------------------------
# Group on x, shape_by, facet_by
# ---------------------------------------------------------------------------

test_that("a group column on x gives one position per group, with fixed jitter", {
  p <- plot_results(make_tc_results(), x = "Group_1", y = "Theta")
  expect_equal(levels(p$data$x), c("WT", "KO"))          # upload order
  pos <- p$layers[[1]]$position
  expect_s3_class(pos, "PositionJitter")
  expect_equal(pos$height, 0)                             # y values untouched
  # Same seed -> identical layout on every redraw.
  b1 <- ggplot2::layer_data(p); b2 <- ggplot2::layer_data(p)
  expect_equal(b1$x, b2$x)
  expect_equal(b1$y, p$data$y)
  expect_true(render_ok(p))
})

test_that("Sample and numeric x are not jittered", {
  res <- make_tc_results()
  expect_false(inherits(plot_results(res, "Sample", "Theta")$layers[[1]]$position,
                        "PositionJitter"))
  expect_false(inherits(plot_results(res, "Actual_Time", "Theta")$layers[[1]]$position,
                        "PositionJitter"))
})

test_that("shape_by maps shapes and renders", {
  p <- plot_results(make_tc_results(), x = "Theta", y = "Pred_Error",
                    colour = "Group_1", shape = "Replicate")
  expect_equal(levels(p$data$shape), c("1", "2"))
  expect_true("shape" %in% names(p$mapping))
  expect_equal(length(unique(ggplot2::layer_data(p)$shape)), 2L)
  expect_true(render_ok(p))
})

test_that("more shape levels than the palette gives a clear error", {
  res <- make_tc_results()
  res$Group_1 <- paste0("g", 1:6)
  res <- rbind(res, transform(res, Sample = paste0(Sample, "b"), Group_1 = paste0("h", 1:6)),
               transform(res, Sample = paste0(Sample, "c"), Group_1 = paste0("k", 1:6)))
  expect_error(plot_results(res, "Theta", "Pred_Error", shape = "Group_1"),
               "18 levels; shapes can show at most 12")
})

test_that("facet_by gives one panel per level and keeps the y = x line in each", {
  p <- plot_results(make_tc_results(), x = "Actual_Time",
                    y = "weighted_mean_time_pred", facet = "Group_1")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$layout$layout), 2L)             # WT and KO panels
  expect_true(inherits(p$layers[[1]]$geom, "GeomAbline"))
  expect_true(render_ok(p))
})

test_that("hover lists each grouping column once", {
  p <- plot_results(make_tc_results(), x = "Group_1", y = "Theta",
                    colour = "Group_1", shape = "Replicate", facet = "Replicate")
  h <- p$data$hover[1]
  expect_equal(lengths(regmatches(h, gregexpr("Group_1:", h))), 1L)
  expect_equal(lengths(regmatches(h, gregexpr("Replicate:", h))), 1L)
})

test_that("interactive plot with colour, shape and facets builds", {
  skip_if_not_installed("plotly")
  w <- plot_results(make_tc_results(), x = "Group_1", y = "Theta",
                    colour = "Group_1", shape = "Replicate", facet = "Replicate",
                    interactive = TRUE)
  expect_s3_class(w, "plotly")
  expect_silent(plotly::plotly_build(w))
})

test_that("legends and facet panels follow upload order, not alphabetical", {
  p <- plot_results(make_tc_results(), x = "Theta", y = "Pred_Error",
                    colour = "Group_1", facet = "Group_1")
  expect_equal(levels(p$data$colour), c("WT", "KO"))
  expect_equal(levels(p$data$facet),  c("WT", "KO"))
})
