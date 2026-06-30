# test-check_time_coverage.R
#
# Tests for check_time_coverage(). It is a plain base-R function (no Shiny, no
# TimeTeller), so it runs anywhere. The grouping it uses mirrors TimeTeller-v2's
# plain `timecourse` normalisation: paste0(group_1, group_2, group_3, replicate)
# with NA -> "".

library(testthat)

# ---------------------------------------------------------------------------
# Passing coverage (span strictly greater than the threshold)
# ---------------------------------------------------------------------------

test_that("a full 24 h cycle in one group passes", {
  res <- check_time_coverage(test_time = c(0, 6, 12, 18, 24),
                             test_group_1 = rep("WT", 5))
  expect_true(res$ok)
  expect_equal(res$n_groups, 1L)
  expect_equal(res$n_failing, 0L)
  expect_equal(nrow(res$failing), 0L)
  expect_equal(res$min_span, 24)
})

test_that("span just over the threshold (13 h) passes", {
  res <- check_time_coverage(test_time = c(0, 13), test_replicate = c(1, 1))
  expect_true(res$ok)
  expect_equal(res$min_span, 13)
})

# ---------------------------------------------------------------------------
# Failing coverage (span <= threshold)  -- boundary is inclusive
# ---------------------------------------------------------------------------

test_that("span exactly equal to the threshold (12 h) fails", {
  res <- check_time_coverage(test_time = c(0, 12), test_group_1 = c("A", "A"))
  expect_false(res$ok)
  expect_equal(res$n_failing, 1L)
  expect_equal(res$failing$span, 12)
  expect_match(res$summary, "Insufficient time coverage")
  expect_match(res$summary, "24 h circadian cycle")
})

test_that("span below the threshold (11 h) fails", {
  res <- check_time_coverage(test_time = c(2, 13), test_group_1 = c("A", "A"))
  expect_false(res$ok)
  expect_equal(res$failing$span, 11)
})

test_that("a single-sample group has span 0 and fails", {
  res <- check_time_coverage(test_time = 6, test_replicate = 1)
  expect_false(res$ok)
  expect_equal(res$failing$span, 0)
  expect_equal(res$failing$n, 1L)
})

# ---------------------------------------------------------------------------
# Multiple groups: overall fails if ANY group fails; the message names them
# ---------------------------------------------------------------------------

test_that("one good and one bad group fails overall and flags only the bad one", {
  # Group WT spans 0..24 (ok); group KO spans 0..6 (fails).
  res <- check_time_coverage(
    test_time    = c(0, 12, 24,  0,  3,  6),
    test_group_1 = c("WT", "WT", "WT", "KO", "KO", "KO"))
  expect_false(res$ok)
  expect_equal(res$n_groups, 2L)
  expect_equal(res$n_failing, 1L)
  expect_equal(res$failing$group, "KO")
  expect_equal(res$failing$span, 6)
  expect_equal(res$failing$n, 3L)
  expect_match(res$summary, "KO")
  expect_false(grepl("'WT'", res$summary))   # the good group is not flagged
})

test_that("all groups failing are all listed, smallest span first", {
  res <- check_time_coverage(
    test_time    = c(0, 8,  0, 4),
    test_group_1 = c("A", "A", "B", "B"))
  expect_false(res$ok)
  expect_equal(res$n_failing, 2L)
  expect_equal(res$failing$span, c(4, 8))          # B (4) before A (8)
  expect_equal(res$failing$group, c("B", "A"))
})

# ---------------------------------------------------------------------------
# Grouping key matches TimeTeller's paste0(g1, g2, g3, replicate), NA -> ""
# ---------------------------------------------------------------------------

test_that("groups are keyed by the concatenation of all supplied grouping cols", {
  # group_1 x replicate gives four distinct groups, each a single time point
  # (span 0) -> all fail; confirms the key combines columns like TimeTeller.
  res <- check_time_coverage(
    test_time      = c(0, 6, 0, 6),
    test_group_1   = c("WT", "WT", "KO", "KO"),
    test_replicate = c(1,    2,    1,    2))
  expect_equal(res$n_groups, 4L)
  expect_false(res$ok)
  expect_true(all(res$failing$span == 0))
  expect_setequal(res$failing$group, c("WT1", "WT2", "KO1", "KO2"))
})

test_that("NA grouping entries collapse to '' (matching replace_na in TimeTeller)", {
  # Two samples with group_1 = NA share the same ("") key, so they form one
  # group spanning 0..24 (ok).
  res <- check_time_coverage(test_time   = c(0, 24),
                             test_group_1 = c(NA, NA),
                             test_replicate = c("r", "r"))
  expect_equal(res$n_groups, 1L)
  expect_true(res$ok)
})

test_that("an absent (NULL) grouping column does not split groups", {
  # Only replicate supplied; group_1/2/3 NULL -> contribute "" each.
  res <- check_time_coverage(test_time = c(0, 24, 0, 24),
                             test_replicate = c("a", "a", "b", "b"))
  expect_equal(res$n_groups, 2L)
  expect_true(res$ok)
})

# ---------------------------------------------------------------------------
# min_span_hours is configurable
# ---------------------------------------------------------------------------

test_that("min_span_hours changes the boundary", {
  # Span 12 passes when the threshold is lowered below 12.
  res <- check_time_coverage(test_time = c(0, 12), test_group_1 = c("A", "A"),
                             min_span_hours = 6)
  expect_true(res$ok)
})

# ---------------------------------------------------------------------------
# Malformed input -> error (these are programming/data errors, not flags)
# ---------------------------------------------------------------------------

test_that("non-numeric times are an error", {
  expect_error(
    check_time_coverage(test_time = c("ZT0", "ZT12"), test_group_1 = c("A", "A")),
    "must be numeric")
})

test_that("character times that parse as numbers are accepted", {
  res <- check_time_coverage(test_time = c("0", "24"), test_group_1 = c("A", "A"))
  expect_true(res$ok)
})

test_that("a grouping vector of the wrong length is an error", {
  expect_error(
    check_time_coverage(test_time = c(0, 6, 12), test_group_1 = c("A", "A")),
    "one value per sample")
})

test_that("no grouping information at all is an error", {
  expect_error(
    check_time_coverage(test_time = c(0, 12, 24)),
    "No grouping information")
})

test_that("empty test_time is an error", {
  expect_error(check_time_coverage(test_time = numeric(0)), "empty")
})
