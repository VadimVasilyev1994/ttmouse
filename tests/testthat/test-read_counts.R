# test-read_counts.R
#
# Tests for read_counts(). The CSV path is pure base R. The XLSX path needs
# openxlsx (used both to build fixtures and to read them); those tests skip if
# openxlsx is unavailable.
#
# Run with: source("read_counts.R"); testthat::test_file("test-read_counts.R")

library(testthat)

# Write a well-formed counts CSV (col 1 = gene IDs, rest = samples) to a tempfile.
write_csv_fixture <- function(genes = c("ENSMUSG001", "ENSMUSG002", "ENSMUSG003"),
                              samples = c("s1", "s2"),
                              header_gene_col = TRUE) {
  p   <- tempfile(fileext = ".csv")
  mat <- matrix(seq_len(length(genes) * length(samples)),
                nrow = length(genes), dimnames = list(genes, samples))
  df  <- data.frame(gene_id = genes, mat, check.names = FALSE,
                    stringsAsFactors = FALSE)
  # Optionally drop the gene-ID header name to simulate a "ragged" upload.
  if (header_gene_col) {
    write.csv(df, p, row.names = FALSE)
  } else {
    lines <- c(paste(samples, collapse = ","),
               apply(df, 1, function(r) paste(r, collapse = ",")))
    writeLines(lines, p)
  }
  p
}

# ---------------------------------------------------------------------------
# CSV path (base R)
# ---------------------------------------------------------------------------

test_that("a well-formed CSV reads into a numeric matrix with correct dimnames", {
  p   <- write_csv_fixture()
  mat <- read_counts(p)

  expect_true(is.matrix(mat))
  expect_true(is.numeric(mat))
  expect_equal(rownames(mat), c("ENSMUSG001", "ENSMUSG002", "ENSMUSG003"))
  expect_equal(colnames(mat), c("s1", "s2"))
  expect_equal(dim(mat), c(3L, 2L))
  # Values round-trip (column-major fill from the fixture).
  expect_equal(mat["ENSMUSG001", "s1"], 1)
  expect_equal(mat["ENSMUSG003", "s2"], 6)
})

test_that("a ragged header (missing gene-ID column name) is still parsed correctly", {
  p   <- write_csv_fixture(header_gene_col = FALSE)
  mat <- read_counts(p)
  # Gene IDs must remain the rownames, not be swallowed / mis-shifted.
  expect_equal(rownames(mat), c("ENSMUSG001", "ENSMUSG002", "ENSMUSG003"))
  expect_equal(colnames(mat), c("s1", "s2"))
  expect_true(is.numeric(mat))
})

test_that("a single sample column is accepted", {
  p   <- write_csv_fixture(samples = "only_sample")
  mat <- read_counts(p)
  expect_equal(ncol(mat), 1L)
  expect_equal(colnames(mat), "only_sample")
})

test_that("duplicated gene IDs are rejected", {
  p <- write_csv_fixture(genes = c("ENSMUSG001", "ENSMUSG001", "ENSMUSG002"))
  expect_error(read_counts(p), "duplicated gene ID")
})

test_that("missing/empty gene IDs are rejected", {
  p <- write_csv_fixture(genes = c("ENSMUSG001", "", "ENSMUSG003"))
  expect_error(read_counts(p), "missing/empty gene IDs")
})

test_that("a non-numeric sample column is rejected and named", {
  p <- tempfile(fileext = ".csv")
  df <- data.frame(gene_id = c("ENSMUSG001", "ENSMUSG002"),
                   s1 = c(10, 20),
                   annotation = c("protein_coding", "lincRNA"),  # stray column
                   check.names = FALSE, stringsAsFactors = FALSE)
  write.csv(df, p, row.names = FALSE)
  expect_error(read_counts(p), "annotation")
})

test_that("a file with no sample columns is rejected", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("gene_id", "ENSMUSG001", "ENSMUSG002"), p)
  expect_error(read_counts(p), "at least one sample column")
})

test_that("missing files and unsupported extensions are rejected", {
  expect_error(read_counts(tempfile(fileext = ".csv")), "not found")

  p <- tempfile(fileext = ".txt"); writeLines("x", p)
  expect_error(read_counts(p), "Unsupported file type")
})

# ---------------------------------------------------------------------------
# XLSX path (needs openxlsx)
# ---------------------------------------------------------------------------

test_that("a well-formed XLSX reads into the same matrix as CSV", {
  skip_if_not_installed("openxlsx")
  p  <- tempfile(fileext = ".xlsx")
  df <- data.frame(gene_id = c("ENSMUSG001", "ENSMUSG002"),
                   s1 = c(10, 30), s2 = c(20, 40),
                   check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, p)

  mat <- read_counts(p)
  expect_equal(rownames(mat), c("ENSMUSG001", "ENSMUSG002"))
  expect_equal(colnames(mat), c("s1", "s2"))
  expect_equal(mat["ENSMUSG002", "s2"], 40)
  expect_true(is.numeric(mat))
})

test_that("the sheet argument selects the right worksheet", {
  skip_if_not_installed("openxlsx")
  p  <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "decoy")
  openxlsx::addWorksheet(wb, "counts")
  openxlsx::writeData(wb, "decoy",
                      data.frame(x = 1, y = 2))
  openxlsx::writeData(wb, "counts",
                      data.frame(gene_id = c("ENSMUSG001", "ENSMUSG002"),
                                 s1 = c(7, 8), check.names = FALSE))
  openxlsx::saveWorkbook(wb, p)

  mat <- read_counts(p, sheet = "counts")
  expect_equal(rownames(mat), c("ENSMUSG001", "ENSMUSG002"))
  expect_equal(mat["ENSMUSG002", "s1"], 8)
})

test_that("a non-numeric column in XLSX is rejected", {
  skip_if_not_installed("openxlsx")
  p  <- tempfile(fileext = ".xlsx")
  df <- data.frame(gene_id = c("ENSMUSG001", "ENSMUSG002"),
                   s1 = c(10, 20),
                   note = c("a", "b"),
                   check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, p)
  expect_error(read_counts(p), "note")
})
