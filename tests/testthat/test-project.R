# Tests for project_test_data().
#
# project_test_data() reaches the external TimeTeller::test_model() through the
# internal seam .call_test_model(); the wiring tests mock that seam with
# testthat::local_mocked_bindings(), so they verify which arguments are forwarded
# without the TimeTeller package installed. The guard tests need no mock (they
# error before the seam). The end-to-end / round-trip tests need the package +
# a real model + data and are skipped here.

make_model <- function(norm = "intergene", log_thresh = -5,
                       genes = c("ENSMUSG00000000001", "ENSMUSG00000000002")) {
  list(
    Normalisation_choice = norm,
    Metadata   = list(Train = list(Genes_Used = genes)),
    Train_Data = list(LogThresh_Train = log_thresh)
  )
}

make_expr <- function(genes, n = 2) {
  matrix(rnorm(length(genes) * n), nrow = length(genes),
         dimnames = list(genes, paste0("s", seq_len(n))))
}

GENES <- c("ENSMUSG00000000001", "ENSMUSG00000000002")

# ---------------------------------------------------------------------------
# Call wiring (mock the seam to capture forwarded arguments)
# ---------------------------------------------------------------------------

test_that("the model's stored log threshold is applied by default", {
  captured <- NULL
  local_mocked_bindings(.call_test_model = function(...) {
    captured <<- list(...)
    list(Test_Data = list(Results_df = data.frame(time_1st_peak = 1)))
  })
  project_test_data(make_model(log_thresh = -5), make_expr(GENES))
  expect_equal(captured$log_thresh, -5)            # taken from the model, not supplied
})

test_that("an explicit log_thresh overrides the model default", {
  captured <- NULL
  local_mocked_bindings(.call_test_model = function(...) {
    captured <<- list(...); list(Test_Data = list(Results_df = data.frame(x = 1)))
  })
  project_test_data(make_model(log_thresh = -5), make_expr(GENES), log_thresh = -3)
  expect_equal(captured$log_thresh, -3)
})

test_that("mat_normalised_test is forced TRUE (app controls the scale)", {
  captured <- NULL
  local_mocked_bindings(.call_test_model = function(...) {
    captured <<- list(...); list(Test_Data = list(Results_df = data.frame(x = 1)))
  })
  project_test_data(make_model(), make_expr(GENES))
  expect_true(captured$mat_normalised_test)
})

test_that("unsupplied optional args are omitted, not passed as NULL", {
  captured <- NULL
  local_mocked_bindings(.call_test_model = function(...) {
    captured <<- list(...); list(Test_Data = list(Results_df = data.frame(x = 1)))
  })
  project_test_data(make_model(), make_expr(GENES))
  # intergene + no groups -> these must not appear in the call at all, so
  # add_test_data()'s missing() defaults to NA can take effect.
  expect_false("test_group_1" %in% names(captured))
  expect_false("test_time"    %in% names(captured))
})

test_that("supplied grouping is forwarded for a timecourse model", {
  captured <- NULL
  local_mocked_bindings(.call_test_model = function(...) {
    captured <<- list(...); list(Test_Data = list(Results_df = data.frame(x = 1)))
  })
  project_test_data(make_model(norm = "timecourse"), make_expr(GENES),
                    test_group_1 = c("A", "A"), test_time = c(0, 12))
  expect_equal(captured$test_group_1, c("A", "A"))
  expect_equal(captured$test_time, c(0, 12))
})

# ---------------------------------------------------------------------------
# Guards (fire before the seam is reached)
# ---------------------------------------------------------------------------

test_that("a timecourse model without grouping fails early and clearly", {
  expect_error(
    project_test_data(make_model(norm = "timecourse"), make_expr(GENES)),
    "requires per-group structure")
})

test_that("a missing/invalid log threshold is rejected", {
  m <- make_model(); m$Train_Data$LogThresh_Train <- NULL  # default would be NULL
  expect_error(project_test_data(m, make_expr(GENES)), "single numeric")
})

test_that("a malformed expr_matrix is rejected", {
  expect_error(project_test_data(make_model(), "not a matrix"),
               "matrix or data.frame")
})

# ---------------------------------------------------------------------------
# Integration / round-trip (need the TimeTeller package + real model + data)
# ---------------------------------------------------------------------------

test_that("[integration] projection runs end-to-end on real data", {
  skip("Requires the TimeTeller package + a real model .rds; run in a full environment.")
  # model <- load_model("intergene_model.rds")
  # expr  <- preprocess_counts(read_counts("counts.csv"), gene_lengths)
  # stopifnot(validate_genes(model, expr)$ok)
  # obj   <- project_test_data(model, expr)
  # expect_true(all(c("time_1st_peak", "time_2nd_peak", "Theta") %in%
  #                 colnames(obj$Test_Data$Results_df)))
})

test_that("[round-trip] training-derived samples recover sensible phase", {
  skip("Requires the TimeTeller package + the model's own training data; run locally.")
  # Project samples whose true circadian time is known and check predicted phase
  # is close. Also measures the filtered-train vs unfiltered-projection TPM
  # difference flagged in preprocess_counts rather than assuming it negligible.
})
