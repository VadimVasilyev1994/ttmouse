# test-phaseB.R
#
# Tests for load_model() / accessors (load_model.R) and validate_genes()
# (validate_genes.R). All base R — no edgeR/Bioconductor — so these run anywhere.
#
# Run with:
#   source("load_model.R"); source("validate_genes.R")
#   testthat::test_file("test-phaseB.R")

library(testthat)

# Minimal fixture with the exact structure verified from the package source.
make_model <- function(norm       = "intergene",
                       genes      = c("ENSMUSG00000000001",
                                      "ENSMUSG00000000002",
                                      "ENSMUSG00000000003"),
                       log_thresh = -5) {
  list(
    Normalisation_choice = norm,
    Metadata   = list(Train = list(Genes_Used = genes)),
    Train_Data = list(LogThresh_Train = log_thresh)
  )
}

# Expression matrix whose rownames are exactly `present` (genes x samples).
make_expr <- function(present, n_samples = 2) {
  matrix(rnorm(length(present) * n_samples),
         nrow = length(present),
         dimnames = list(present, paste0("sample", seq_len(n_samples))))
}

# ---------------------------------------------------------------------------
# load_model()
# ---------------------------------------------------------------------------

test_that("a valid model loads and accessors read the verified fields", {
  p <- tempfile(fileext = ".rds")
  saveRDS(make_model(log_thresh = -4.5), p)

  m <- load_model(p)
  expect_true(is.list(m))
  expect_equal(model_normalisation(m), "intergene")
  expect_equal(model_genes(m),
               c("ENSMUSG00000000001", "ENSMUSG00000000002", "ENSMUSG00000000003"))
  expect_equal(model_log_thresh(m), -4.5)
})

test_that("a missing or non-numeric LogThresh_Train is rejected", {
  bad <- make_model(); bad$Train_Data$LogThresh_Train <- NULL
  p <- tempfile(fileext = ".rds"); saveRDS(bad, p)
  expect_error(load_model(p), "LogThresh_Train")

  bad2 <- make_model(); bad2$Train_Data$LogThresh_Train <- "not numeric"
  p2 <- tempfile(fileext = ".rds"); saveRDS(bad2, p2)
  expect_error(load_model(p2), "LogThresh_Train")
})

test_that("each valid normalisation method is accepted", {
  for (nm in c("intergene", "clr", "timecourse", "timecourse_matched", "combined")) {
    p <- tempfile(fileext = ".rds")
    saveRDS(make_model(norm = nm), p)
    expect_equal(model_normalisation(load_model(p)), nm)
  }
})

test_that("a missing file is reported", {
  expect_error(load_model(tempfile(fileext = ".rds")), "not found")
})

test_that("a non-list object is rejected", {
  p <- tempfile(fileext = ".rds")
  saveRDS(matrix(1:4, 2), p)
  expect_error(load_model(p), "not a TimeTeller model")
})

test_that("an invalid Normalisation_choice is rejected", {
  p <- tempfile(fileext = ".rds")
  saveRDS(make_model(norm = "quantile"), p)   # not a real TT method
  expect_error(load_model(p), "not one of")
})

test_that("a missing Normalisation_choice is rejected", {
  bad <- make_model(); bad$Normalisation_choice <- NULL
  p <- tempfile(fileext = ".rds"); saveRDS(bad, p)
  expect_error(load_model(p), "Normalisation_choice")
})

test_that("missing or empty Genes_Used is rejected", {
  bad <- make_model(); bad$Metadata$Train$Genes_Used <- NULL
  p <- tempfile(fileext = ".rds"); saveRDS(bad, p)
  expect_error(load_model(p), "Genes_Used")

  empty <- make_model(genes = character(0))
  p2 <- tempfile(fileext = ".rds"); saveRDS(empty, p2)
  expect_error(load_model(p2), "Genes_Used")
})

# ---------------------------------------------------------------------------
# validate_genes()
# ---------------------------------------------------------------------------

test_that("all genes present -> ok with empty missing set", {
  m    <- make_model()
  expr <- make_expr(present = model_genes(m))
  res  <- validate_genes(m, expr)

  expect_true(res$ok)
  expect_equal(res$n_required, 3L)
  expect_equal(res$n_present, 3L)
  expect_equal(res$n_missing, 0L)
  expect_length(res$missing, 0L)
  expect_match(res$summary, "All 3 required genes present")
})

test_that("missing genes -> ok = FALSE with correct report", {
  m    <- make_model()
  # Supply only the first required gene; the other two are missing.
  expr <- make_expr(present = model_genes(m)[1])
  res  <- validate_genes(m, expr)

  expect_false(res$ok)
  expect_equal(res$n_required, 3L)
  expect_equal(res$n_present, 1L)
  expect_equal(res$n_missing, 2L)
  expect_setequal(res$missing, model_genes(m)[2:3])
  expect_match(res$summary, "2 of 3 required genes missing")
})

test_that("extra genes beyond the required set do not affect validity", {
  m    <- make_model()
  expr <- make_expr(present = c(model_genes(m), "ENSMUSG00000099999"))
  res  <- validate_genes(m, expr)
  expect_true(res$ok)
  expect_equal(res$n_present, 3L)
})

test_that("the displayed missing list is capped but the full vector is returned", {
  many   <- sprintf("ENSMUSG%011d", 1:50)
  m      <- make_model(genes = many)
  expr   <- make_expr(present = many[1:5])         # 45 missing
  res    <- validate_genes(m, expr, max_show = 20)

  expect_equal(res$n_missing, 45L)
  expect_length(res$missing, 45L)                  # full vector retained
  expect_match(res$summary, "and 25 more")         # 45 - 20 capped
})

test_that("malformed expr_matrix inputs are rejected", {
  m <- make_model()
  expect_error(validate_genes(m, "not a matrix"), "matrix or data.frame")

  no_rn <- matrix(1:4, 2)                           # no rownames
  expect_error(validate_genes(m, no_rn), "rownames")
})
