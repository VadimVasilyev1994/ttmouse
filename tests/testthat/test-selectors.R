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
