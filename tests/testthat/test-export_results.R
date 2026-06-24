# test-export_results.R
#
# Tests for export_results(). Operates on a plain list with a Results_df
# data.frame, so it needs neither TimeTeller nor a real projection.

library(testthat)

# Build a fake post-projection object whose Results_df has the given columns.
make_results_obj <- function(cols) {
  df <- as.data.frame(stats::setNames(lapply(cols, function(x) c(1, 2, 3)), cols),
                      check.names = FALSE)
  rownames(df) <- c("s1", "s2", "s3")
  list(Test_Data = list(Results_df = df))
}

test_that("export_results returns Sample + the present subset, dropping diagnostics", {
  obj <- make_results_obj(c("time_1st_peak", "weighted_mean_time_pred", "Theta",
                            "Actual_Time", "Pred_Error",
                            "Group_1", "Group_2", "Group_3", "Replicate",
                            "npeaks", "max_1st_peak"))   # + internal diagnostics
  out <- export_results(obj)
  expect_equal(colnames(out)[1], "Sample")
  expect_true(all(c("time_1st_peak", "Theta", "Group_1") %in% colnames(out)))
  expect_false(any(c("npeaks", "max_1st_peak") %in% colnames(out)))
  expect_equal(out$Sample, c("s1", "s2", "s3"))
})

test_that("subset columns are kept in the requested order", {
  obj <- make_results_obj(c("Theta", "time_1st_peak", "Group_1"))  # scrambled input
  out <- export_results(obj)
  # RESULT_EXPORT_COLS order: time_1st_peak before Theta before Group_1.
  expect_equal(colnames(out), c("Sample", "time_1st_peak", "Theta", "Group_1"))
})

test_that("absent subset columns are quietly skipped (intergene path)", {
  obj <- make_results_obj(c("time_1st_peak", "weighted_mean_time_pred", "Theta"))
  out <- export_results(obj)
  expect_equal(colnames(out),
               c("Sample", "time_1st_peak", "weighted_mean_time_pred", "Theta"))
  expect_silent(export_results(obj))   # no message/warning for missing Group_* etc.
})

test_that("export_results writes a CSV when a path is given", {
  obj  <- make_results_obj(c("time_1st_peak", "Theta"))
  path <- tempfile(fileext = ".csv")
  export_results(obj, path = path)
  expect_true(file.exists(path))
  back <- utils::read.csv(path, check.names = FALSE)
  expect_equal(colnames(back), c("Sample", "time_1st_peak", "Theta"))
  expect_equal(nrow(back), 3L)
})

test_that("export_results errors without a projection", {
  expect_error(export_results(list()), "Results_df")
})
