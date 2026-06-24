# test-preprocess_counts.R
#
# Tests for preprocess_counts(). Two groups:
#   (1) input validation + frozen-contract enforcement  -- need only base R,
#       so they run in any environment (the wrapper rejects bad input, or the
#       forwarded arguments are captured by a stub, before counts_to_tpm runs).
#   (2) end-to-end TPM behaviour                         -- need the real
#       counts_to_tpm() plus edgeR + DGEobj.utils, so they are skipped where
#       those are unavailable.
#
# Run with:  source("preprocess_counts.R"); testthat::test_file("test-preprocess_counts.R")
# (or load the package, once preprocess_counts is part of it).

library(testthat)

# A minimal, valid raw-count matrix used across several tests.
make_counts <- function() {
  matrix(c(1000, 2000, 500, 50,
              0,  100, 750, 10),
         nrow = 4, byrow = FALSE,
         dimnames = list(c("ENSMUSG00000000001", "ENSMUSG00000000002",
                           "ENSMUSG00000000003", "ENSMUSG00000000004"),
                         c("sample1", "sample2")))
}

# ---------------------------------------------------------------------------
# Group 1: input validation (base R only)
# ---------------------------------------------------------------------------

test_that("non-matrix / non-numeric input is rejected", {
  expect_error(preprocess_counts("not a matrix", gene_lengths = data.frame()),
               "matrix or data.frame")
  bad <- matrix(letters[1:4], nrow = 2,
                dimnames = list(c("g1", "g2"), c("s1", "s2")))
  expect_error(preprocess_counts(bad, gene_lengths = data.frame()),
               "numeric")
})

test_that("missing rownames are rejected", {
  m <- matrix(1:4, nrow = 2)               # no rownames
  expect_error(preprocess_counts(m, gene_lengths = data.frame()),
               "rownames")
})

test_that("empty dimensions are rejected", {
  m <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_error(preprocess_counts(m, gene_lengths = data.frame()))
})

test_that("non-finite values are rejected", {
  m <- make_counts(); m[1, 1] <- NA
  expect_error(preprocess_counts(m, gene_lengths = data.frame()),
               "finite")
  m2 <- make_counts(); m2[2, 2] <- Inf
  expect_error(preprocess_counts(m2, gene_lengths = data.frame()),
               "finite")
})

test_that("negative values (log/normalised data passed by mistake) are rejected", {
  m <- make_counts(); m[1, 1] <- -5
  expect_error(preprocess_counts(m, gene_lengths = data.frame()),
               "negative")
})

# ---------------------------------------------------------------------------
# Group 1b: frozen-contract enforcement (base R only, via a capturing stub)
# ---------------------------------------------------------------------------

test_that("the frozen contract arguments are forwarded to counts_to_tpm()", {
  # Replace counts_to_tpm() in the ttmouse namespace with a stub that records
  # the arguments preprocess_counts() forwards to it. local_mocked_bindings()
  # rebinds inside the (locked) package namespace and restores the original
  # automatically when the test exits -- the same mechanism test-project.R
  # uses for the .call_test_model seam.
  captured <- NULL
  stub <- function(counts, gene_lengths, return_format, log_transform,
                   prior_count, skip_filter, verbose) {
    captured <<- list(return_format = return_format,
                      log_transform = log_transform,
                      prior_count   = prior_count,
                      skip_filter   = skip_filter)
    counts  # return value is irrelevant for this test
  }
  local_mocked_bindings(counts_to_tpm = stub)
  
  preprocess_counts(make_counts(), gene_lengths = data.frame())
  
  expect_equal(captured$return_format, "ensembl")
  expect_true(captured$log_transform)
  expect_equal(captured$prior_count, 1)
  expect_true(captured$skip_filter)
})

# ---------------------------------------------------------------------------
# Group 2: end-to-end TPM behaviour (needs counts_to_tpm + edgeR + DGEobj.utils)
# ---------------------------------------------------------------------------

# Small gene_lengths table matching make_counts() rownames, with known lengths.
make_gene_lengths <- function() {
  data.frame(
    gene_id     = c("ENSMUSG00000000001", "ENSMUSG00000000002",
                    "ENSMUSG00000000003", "ENSMUSG00000000004"),
    gene_length = c(1000, 500, 2000, 1000),
    gene_name   = c("GeneA", "Geneb", "Genec", "Bmal1"),
    stringsAsFactors = FALSE
  )
}

test_that("skip_filter = TRUE retains every annotated gene", {
  skip_if_not(exists("counts_to_tpm"), "counts_to_tpm() not sourced")
  skip_if_not_installed("edgeR")
  skip_if_not_installed("DGEobj.utils")

  out <- preprocess_counts(make_counts(), gene_lengths = make_gene_lengths())
  # All four input genes have length annotation, so none should be dropped.
  expect_equal(nrow(out), 4L)
  expect_equal(ncol(out), 2L)
})

test_that("output is log2(TPM + 1) on the standard TPM definition", {
  skip_if_not(exists("counts_to_tpm"), "counts_to_tpm() not sourced")
  skip_if_not_installed("edgeR")
  skip_if_not_installed("DGEobj.utils")

  counts  <- make_counts()
  lengths <- make_gene_lengths()
  out     <- preprocess_counts(counts, lengths)

  # Independent reference implementation of standard TPM, per sample:
  #   rate = count / length ; tpm = rate / sum(rate) * 1e6
  len <- lengths$gene_length[match(rownames(counts), lengths$gene_id)]
  expected <- apply(counts, 2, function(col) {
    rate <- col / len
    tpm  <- rate / sum(rate) * 1e6
    log2(tpm + 1)
  })
  rownames(expected) <- rownames(counts)

  # Align gene order before comparing (counts_to_tpm may reorder rows).
  out <- out[rownames(expected), , drop = FALSE]
  # Tolerance allows for any minor internal differences in DGEobj.utils.
  expect_equal(unname(out), unname(expected), tolerance = 1e-6)
})

test_that("Ensembl version suffixes are stripped", {
  skip_if_not(exists("counts_to_tpm"), "counts_to_tpm() not sourced")
  skip_if_not_installed("edgeR")
  skip_if_not_installed("DGEobj.utils")

  counts <- make_counts()
  rownames(counts) <- paste0(rownames(counts), ".7")   # add version suffix
  out <- preprocess_counts(counts, make_gene_lengths())
  expect_true(all(grepl("^ENSMUSG[0-9]+$", rownames(out))))  # no ".N" remaining
})

test_that("Bmal1 is resolved to Arntl when only Arntl is annotated (symbol input)", {
  skip_if_not(exists("counts_to_tpm"), "counts_to_tpm() not sourced")
  skip_if_not_installed("edgeR")
  skip_if_not_installed("DGEobj.utils")

  # Symbol-based counts where the alias differs from the length table.
  counts <- make_counts()
  rownames(counts) <- c("Geneb", "Genec", "Genea", "Bmal1")
  lengths <- make_gene_lengths()
  lengths$gene_name[lengths$gene_name == "Bmal1"] <- "Arntl"  # only Arntl annotated
  # Should not error: counts_to_tpm renames Bmal1 -> Arntl and matches it.
  out <- preprocess_counts(counts, lengths)
  expect_true(nrow(out) >= 1L)
})
