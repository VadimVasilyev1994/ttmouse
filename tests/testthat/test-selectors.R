# test-selectors.R
#
# Tests for local_projection_choices() and n_samples(). Both read plain list
# structure, so they need neither TimeTeller nor a real model and run anywhere.

library(testthat)

test_that("local_projection_choices strips the Time_ prefix", {
  model <- list(Projections = list(
    All_Projections = list(Time_0 = 1, Time_6 = 1, Time_12 = 1)))
  expect_equal(local_projection_choices(model), c("0", "6", "12"))
})

test_that("local_projection_choices errors when projections are missing", {
  expect_error(local_projection_choices(list()), "All_Projections")
})

test_that("n_samples reads the 2nd dim of the test likelihood array", {
  obj <- list(Test_Data = list(Test_Likelihood_Array = array(0, dim = c(8, 5, 2))))
  expect_equal(n_samples(obj, "test"), 5)
})

test_that("n_samples reads the train array when asked", {
  obj <- list(Train_Data = list(Train_Likelihood_Array = array(0, dim = c(8, 3, 2))))
  expect_equal(n_samples(obj, "train"), 3)
})

test_that("n_samples defaults to the test array", {
  obj <- list(Test_Data = list(Test_Likelihood_Array = array(0, dim = c(8, 7, 2))))
  expect_equal(n_samples(obj), 7)
})

test_that("n_samples errors when the array is absent", {
  expect_error(n_samples(list(), "test"), "likelihood array")
})

# ---------------------------------------------------------------------------
# sample_label()
# ---------------------------------------------------------------------------

make_obj_with_meta <- function() {
  list(Metadata = list(Test = list(
    Group_1   = c("Adrenal", "Adrenal", "Adrenal"),
    Group_2   = c("ALF", "TRF", "ALF"),
    Group_3   = as.character(rep(NA, 3)),
    Time      = c(0, 2, 4),
    Replicate = as.character(rep(NA, 3))
  )))
}

test_that("sample_label builds a label from present, non-NA fields", {
  obj <- make_obj_with_meta()
  lab <- sample_label(obj, 2)
  expect_true(grepl("group_1: Adrenal", lab))
  expect_true(grepl("group_2: TRF", lab))
  expect_true(grepl("time: 2", lab))
  # Group_3 and Replicate are all NA -> omitted.
  expect_false(grepl("group_3", lab))
  expect_false(grepl("replicate", lab))
})

test_that("sample_label keeps the canonical field order", {
  obj <- make_obj_with_meta()
  expect_equal(sample_label(obj, 1), "group_1: Adrenal | group_2: ALF | time: 0")
})

test_that("sample_label returns empty string when no metadata block exists", {
  expect_equal(sample_label(list(), 1), "")
  expect_equal(sample_label(list(Metadata = list()), 1), "")
})

test_that("sample_label returns empty string when all fields are NA", {
  obj <- list(Metadata = list(Test = list(
    Group_1 = NA, Group_2 = NA, Time = NA)))
  expect_equal(sample_label(obj, 1), "")
})
