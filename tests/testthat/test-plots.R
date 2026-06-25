# test-plots.R
#
# Tests for the four plot wrappers. They mock the internal `.call_plot_*` seams
# via testthat::local_mocked_bindings (as test-project.R mocks .call_test_model),
# so TimeTeller need not be installed. These run under devtools::test() with the
# package loaded; they are not runnable in plain sourced mode (local_mocked_
# bindings requires a loaded package).

library(testthat)

# Minimal fake objects -------------------------------------------------------
fake_model <- function() {
  list(
    Projections = list(All_Projections = list(Time_0 = 1, Time_6 = 1)),
    Train_Data  = list(LogThresh_Train       = -5,
                       Train_Likelihood_Array = array(0, dim = c(8, 3, 2)))
  )
}
fake_projected <- function() {
  m <- fake_model()
  m$Test_Data <- list(Test_Likelihood_Array = array(0, dim = c(8, 4, 2)))
  m
}

# --- 3D projection plots ----------------------------------------------------

test_that("plot_training_projection forwards a valid projection with density = FALSE", {
  captured <- NULL
  local_mocked_bindings(
    .call_plot_3d_projection = function(...) { captured <<- list(...); "fig" })
  res <- plot_training_projection(fake_model(), "0")
  expect_equal(res, "fig")
  expect_equal(captured$selected_local_projection, "0")
  expect_false(captured$density)
})

test_that("plot_training_projection rejects an unknown projection", {
  expect_error(plot_training_projection(fake_model(), "99"), "must be one of")
})

test_that("plot_test_projection requires a completed projection", {
  expect_error(plot_test_projection(fake_model(), "0"), "Test_Data")
})

test_that("plot_test_projection forwards when test data is present", {
  captured <- NULL
  local_mocked_bindings(
    .call_plot_3d_projection_with_test = function(...) { captured <<- list(...); "fig" })
  res <- plot_test_projection(fake_projected(), "6")
  expect_equal(res, "fig")
  expect_equal(captured$selected_local_projection, "6")
  expect_false(captured$density)
})

# --- Per-sample diagnostic plots --------------------------------------------

test_that("plot_sample_likelihoods defaults logthresh from the model and forwards", {
  captured <- NULL
  local_mocked_bindings(
    .call_plot_raw_likelis = function(...) { captured <<- list(...); invisible(NULL) })
  plot_sample_likelihoods(fake_projected(), sample_num = 2)
  expect_equal(captured$sample_num, 2)
  expect_equal(captured$logthresh, -5)            # from Train_Data$LogThresh_Train
  expect_equal(captured$train_or_test, "test")
})

test_that("plot_sample_likelihoods rejects an out-of-range sample_num", {
  expect_error(plot_sample_likelihoods(fake_projected(), sample_num = 99), "1:4")
})

test_that("plot_sample_curve defaults logthresh and forwards", {
  captured <- NULL
  local_mocked_bindings(
    .call_plot_ind_curve = function(...) { captured <<- list(...); "recorded" })
  res <- plot_sample_curve(fake_projected(), sample_num = 1)
  expect_equal(res, "recorded")
  expect_equal(captured$logthresh, -5)
  expect_equal(captured$train_or_test, "test")
})

test_that("an explicit logthresh overrides the model default", {
  captured <- NULL
  local_mocked_bindings(
    .call_plot_raw_likelis = function(...) { captured <<- list(...); invisible(NULL) })
  plot_sample_likelihoods(fake_projected(), sample_num = 1, logthresh = -2)
  expect_equal(captured$logthresh, -2)
})

# --- Per-sample metadata annotation -----------------------------------------
# When the object carries metadata, the per-sample wrappers draw a banner. These
# use a drawing mock on a headless device and assert the call completes.

fake_projected_meta <- function() {
  m <- fake_projected()
  m$Metadata <- list(Test = list(
    Group_1 = rep("Adrenal", 4), Group_2 = c("ALF","TRF","ALF","TRF"),
    Time = c(0, 2, 4, 6)))
  m
}

test_that("plot_sample_likelihoods draws a metadata banner without error", {
  local_mocked_bindings(
    .call_plot_raw_likelis = function(...) { plot(1); invisible(NULL) })
  pdf(NULL); on.exit(dev.off())
  expect_no_error(plot_sample_likelihoods(fake_projected_meta(), sample_num = 2))
})

test_that("plot_sample_curve draws a metadata banner without error", {
  local_mocked_bindings(
    .call_plot_ind_curve = function(...) { par(mfrow = c(2,1)); plot(1); plot(2); "rec" })
  pdf(NULL); on.exit(dev.off())
  res <- plot_sample_curve(fake_projected_meta(), sample_num = 2)
  expect_equal(res, "rec")    # underlying return value preserved
})

test_that("no banner is drawn when the object has no metadata (no device needed)", {
  captured <- NULL
  local_mocked_bindings(
    .call_plot_raw_likelis = function(...) { captured <<- list(...); invisible(NULL) })
  # fake_projected() has no Metadata -> sample_label() is "" -> no par()/mtext().
  expect_no_error(plot_sample_likelihoods(fake_projected(), sample_num = 1))
  expect_equal(captured$sample_num, 1)
})
