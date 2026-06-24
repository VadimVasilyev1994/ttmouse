# test-read_metadata.R
#
# Tests for read_metadata(). The CSV tests need only base R and run anywhere;
# the XLSX round-trip is skipped unless openxlsx is installed.
#
# Run with:  source("read_metadata.R"); testthat::test_file("test-read_metadata.R")
# (or load the package, once read_metadata is part of it).

library(testthat)

# Helper: write a data.frame to a temp CSV and return the path.
write_csv_tmp <- function(df) {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE)
  path
}

# A minimal valid metadata table: time + two optional columns.
make_meta <- function() {
  data.frame(
    time      = c(0, 6, 12, 18),
    group_1   = c("WT", "WT", "KO", "KO"),
    replicate = c(1, 2, 1, 2),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Reading + column contract
# ---------------------------------------------------------------------------

test_that("a valid CSV with time + optional columns is read", {
  path <- write_csv_tmp(make_meta())
  md <- read_metadata(path)
  expect_s3_class(md, "data.frame")
  expect_equal(nrow(md), 4L)
  expect_true(all(c("time", "group_1", "replicate") %in% colnames(md)))
})

test_that("only 'time' is required; optional columns may be absent", {
  path <- write_csv_tmp(data.frame(time = c(0, 6, 12)))
  md <- read_metadata(path)
  expect_equal(colnames(md), "time")
  expect_equal(nrow(md), 3L)
})

test_that("column names are matched case-insensitively and canonicalised", {
  df <- make_meta()
  colnames(df) <- c("Time", "Group_1", "REPLICATE")   # mixed case
  path <- write_csv_tmp(df)
  md <- read_metadata(path)
  expect_true(all(c("time", "group_1", "replicate") %in% colnames(md)))
  expect_false(any(c("Time", "Group_1", "REPLICATE") %in% colnames(md)))
})

test_that("recognised columns may appear in any order", {
  df <- data.frame(group_2 = c("a", "b"), time = c(0, 6),
                   group_1 = c("x", "y"), stringsAsFactors = FALSE)
  path <- write_csv_tmp(df)
  md <- read_metadata(path)
  expect_true(all(c("time", "group_1", "group_2") %in% colnames(md)))
})

test_that("unrecognised columns are kept unchanged", {
  df <- make_meta(); df$notes <- c("a", "b", "c", "d")
  path <- write_csv_tmp(df)
  md <- read_metadata(path)
  expect_true("notes" %in% colnames(md))
})

test_that("surrounding whitespace in headers still matches", {
  df <- make_meta()
  colnames(df) <- c(" time ", "group_1", "replicate")
  path <- write_csv_tmp(df)
  md <- read_metadata(path)
  expect_true("time" %in% colnames(md))
})

# ---------------------------------------------------------------------------
# Validation / errors
# ---------------------------------------------------------------------------

test_that("missing 'time' column is rejected", {
  df <- data.frame(group_1 = c("a", "b"), replicate = c(1, 2))
  path <- write_csv_tmp(df)
  expect_error(read_metadata(path), "time")
})

test_that("two columns collapsing to the same name are rejected", {
  df <- data.frame(time = c(0, 6), TIME = c(1, 2), check.names = FALSE)
  path <- write_csv_tmp(df)
  expect_error(read_metadata(path), "multiple columns")
})

test_that("sample-count mismatch against the count matrix is rejected", {
  path   <- write_csv_tmp(make_meta())          # 4 rows
  counts <- matrix(0, nrow = 3, ncol = 3)       # 3 samples
  expect_error(read_metadata(path, counts = counts), "mismatch")
})

test_that("matching sample count passes the check", {
  path   <- write_csv_tmp(make_meta())          # 4 rows
  counts <- matrix(0, nrow = 3, ncol = 4)       # 4 samples
  md <- read_metadata(path, counts = counts)
  expect_equal(nrow(md), 4L)
})

test_that("a NULL count matrix skips the sample-count check", {
  path <- write_csv_tmp(make_meta())
  expect_silent(read_metadata(path))            # no counts -> no check, no error
})

test_that("unsupported file extension is rejected", {
  path <- tempfile(fileext = ".txt")
  writeLines("time\n0", path)
  expect_error(read_metadata(path), "Unsupported file type")
})

test_that("a missing file is rejected", {
  expect_error(read_metadata("does_not_exist.csv"), "not found")
})

# ---------------------------------------------------------------------------
# XLSX path (needs openxlsx)
# ---------------------------------------------------------------------------

test_that("an XLSX metadata file is read the same way", {
  skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(make_meta(), path)
  md <- read_metadata(path)
  expect_equal(nrow(md), 4L)
  expect_true(all(c("time", "group_1", "replicate") %in% colnames(md)))
})
