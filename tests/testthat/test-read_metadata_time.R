# test-read_metadata_time.R
#
# Tests for the strict time parsing in read_metadata(): numeric hours, ZT/CT
# labels (case-insensitive), plain numbers stored as text, and the stop / warn
# behaviour for unrecognised formats and mixed ZT/CT labels. CSV only, so these
# need only base R.

library(testthat)

# Write a metadata CSV with the given time values and a grouping column.
write_time_csv <- function(time) {
  path <- tempfile(fileext = ".csv")
  df <- data.frame(time = time, group_1 = rep("WT", length(time)),
                   stringsAsFactors = FALSE)
  utils::write.csv(df, path, row.names = FALSE)
  path
}

# ---------------------------------------------------------------------------
# parse_metadata_time() -- the internal parser
# ---------------------------------------------------------------------------

test_that("numeric time is returned unchanged", {
  p <- parse_metadata_time(c(0L, 6L, 12L))
  expect_identical(p$time, c(0L, 6L, 12L))
  expect_equal(p$n_unrecognised, 0L)
})

test_that("ZT and CT labels are parsed, case-insensitively, with optional space", {
  p <- parse_metadata_time(c("ZT4", "zt04", "Zt 8.5", " CT12 ", "ct0"))
  expect_equal(p$time, c(4, 4, 8.5, 12, 0))
  expect_equal(p$n_zt, 3L)
  expect_equal(p$n_ct, 2L)
  expect_equal(p$n_unrecognised, 0L)
})

test_that("plain numbers stored as text are accepted", {
  p <- parse_metadata_time(c("4", " 06 ", "4.5", "-2", ".5"))
  expect_equal(p$time, c(4, 6, 4.5, -2, 0.5))
  expect_equal(p$n_unrecognised, 0L)
})

test_that("anything else is unrecognised and becomes NA", {
  p <- parse_metadata_time(c("4h", "T4", "ZT4h", "noon", "1e3", "ZT-2", "ZT4", "4h"))
  expect_equal(p$time, c(NA, NA, NA, NA, NA, NA, 4, NA))
  expect_equal(p$n_unrecognised, 7L)                       # samples affected
  expect_equal(p$unrecognised, c("4h", "T4", "ZT4h", "noon", "1e3", "ZT-2"))  # distinct
})

test_that("empty cells and NA are missing, not unrecognised", {
  p <- parse_metadata_time(c("ZT4", "", "  ", NA))
  expect_equal(p$time, c(4, NA, NA, NA))
  expect_equal(p$n_unrecognised, 0L)
  # An all-NA column (read.csv gives logical) is also just missing.
  expect_equal(parse_metadata_time(c(NA, NA))$n_unrecognised, 0L)
})

test_that("format_examples keeps messages short", {
  expect_equal(format_examples(c("a", "b")), "'a', 'b'")
  expect_equal(format_examples(c("a", "b", "c", "d", "e")), "'a', 'b', 'c' and 2 more")
})

# ---------------------------------------------------------------------------
# read_metadata() -- stop / warn behaviour by path
# ---------------------------------------------------------------------------

test_that("ZT labels are converted to numeric hours on read", {
  md <- read_metadata(write_time_csv(c("ZT0", "ZT6", "ZT12", "ZT18")))
  expect_true(is.numeric(md$time))
  expect_equal(md$time, c(0, 6, 12, 18))
})

test_that("numeric time columns are read silently and unchanged", {
  path <- write_time_csv(c(0, 6, 12, 18))
  expect_silent(md <- read_metadata(path))
  expect_equal(md$time, c(0, 6, 12, 18))
})

test_that("timecourse path: unrecognised formats stop with a classed error", {
  path <- write_time_csv(c("ZT0", "6h", "ZT12", "18h"))
  err <- expect_error(read_metadata(path, require_time = TRUE),
                      "2 sample\\(s\\) have a time in an unrecognised format")
  expect_s3_class(err, "ttmouse_time_error")
  expect_match(conditionMessage(err), "'6h', '18h'")        # names the values
  expect_match(conditionMessage(err), "ZT<number> or CT<number>")  # says what works
})

test_that("intergene path: unrecognised formats warn and become NA", {
  path <- write_time_csv(c("ZT0", "6h", "ZT12"))
  expect_warning(md <- read_metadata(path, require_time = FALSE),
                 "unrecognised format.*projection continues")
  expect_equal(md$time, c(0, NA, 12))
})

test_that("mixed ZT and CT labels warn but are kept (both paths)", {
  path <- write_time_csv(c("ZT0", "ZT6", "CT12"))
  expect_warning(md <- read_metadata(path, require_time = TRUE),
                 "mixes ZT \\(2 sample\\(s\\)\\) and CT \\(1 sample\\(s\\)\\)")
  expect_equal(md$time, c(0, 6, 12))
  expect_warning(read_metadata(path, require_time = FALSE), "mixes ZT")
})

test_that("a single-prefix column does not trigger the mixed warning", {
  expect_silent(read_metadata(write_time_csv(c("CT0", "ct6", "CT 12"))))
})

test_that("an empty time column raises no format warning", {
  path <- write_time_csv(c(NA, NA, NA))
  expect_silent(md <- read_metadata(path, require_time = FALSE))
  expect_true(all(is.na(md$time)))
})
