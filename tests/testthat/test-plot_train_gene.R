# test-plot_train_gene.R
#
# Tests for the training gene-expression visualisation helpers and plot. The
# listers are pure data; the plot test builds the ggplot and renders it to a
# headless device to confirm it is valid.

library(testthat)

# A minimal fake training object: 2 genes, 2 studies, 3 times, 2 replicates.
make_train_model <- function() {
  times  <- rep(c(0, 8, 16), each = 4)              # 12 samples
  groups <- rep(c("StudyA", "StudyB"), times = 6)
  reps   <- rep(c("1", "2"), times = 6)
  data <- data.frame(
    Gene_ENSMUSG01 = rnorm(12, 5),
    Gene_ENSMUSG02 = rnorm(12, 8),
    Group     = groups,
    Group_2   = rep("", 12),                         # all empty -> no info
    Group_3   = rep("", 12),
    Time      = as.factor(times),
    Replicate = reps,
    stringsAsFactors = FALSE
  )
  list(
    Train = list(Data = data),
    Metadata = list(Train = list(
      Genes_Used = c("ENSMUSG01", "ENSMUSG02"),
      Time       = times
    ))
  )
}

make_gene_lengths <- function() {
  data.frame(gene_id   = c("ENSMUSG01", "ENSMUSG02"),
             gene_name = c("Arntl", "Per2"),
             gene_length = c(1000, 2000),
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# train_genes()
# ---------------------------------------------------------------------------

test_that("train_genes returns Ensembl IDs labelled by symbol", {
  g <- train_genes(make_train_model(), make_gene_lengths())
  expect_equal(unname(g), c("ENSMUSG01", "ENSMUSG02"))
  expect_equal(names(g), c("Arntl", "Per2"))
})

test_that("train_genes falls back to Ensembl IDs without a gene_lengths map", {
  g <- train_genes(make_train_model())
  expect_equal(names(g), c("ENSMUSG01", "ENSMUSG02"))
})

test_that("train_genes labels unmapped genes with their Ensembl ID", {
  gl <- make_gene_lengths()[1, , drop = FALSE]    # only ENSMUSG01 maps
  g <- train_genes(make_train_model(), gl)
  expect_equal(names(g), c("Arntl", "ENSMUSG02"))
})

test_that("train_genes errors on a non-training object", {
  expect_error(train_genes(list()), "Genes_Used")
})

# ---------------------------------------------------------------------------
# train_group_vars()
# ---------------------------------------------------------------------------

test_that("train_group_vars returns only informative grouping columns", {
  gv <- train_group_vars(make_train_model())
  expect_equal(gv, c(group_1 = "Group"))            # Group_2/3 are all empty
})

test_that("train_group_vars includes group_2 when it carries values", {
  m <- make_train_model()
  m$Train$Data$Group_2 <- rep(c("x", "y"), 6)
  gv <- train_group_vars(m)
  expect_true(all(c("group_1", "group_2") %in% names(gv)))
  expect_equal(unname(gv[["group_2"]]), "Group_2")
})

# ---------------------------------------------------------------------------
# plot_train_gene()
# ---------------------------------------------------------------------------

test_that("plot_train_gene returns a ggplot for a valid gene", {
  skip_if_not_installed("ggplot2")
  p <- plot_train_gene(make_train_model(), "ENSMUSG01", gene_label = "Arntl")
  expect_s3_class(p, "ggplot")
})

test_that("plot_train_gene renders without error", {
  skip_if_not_installed("ggplot2")
  p <- plot_train_gene(make_train_model(), "ENSMUSG02")
  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, p, width = 5, height = 4, dpi = 72)
  expect_true(file.exists(tmp))
})

test_that("plot_train_gene errors for an unknown gene", {
  expect_error(plot_train_gene(make_train_model(), "ENSMUSG99"),
               "not among the training genes")
})

test_that("plot_train_gene errors for an unknown grouping variable", {
  expect_error(plot_train_gene(make_train_model(), "ENSMUSG01", group_var = "Nope"),
               "not found")
})

test_that("the title defaults to the Ensembl ID when no label is given", {
  skip_if_not_installed("ggplot2")
  p <- plot_train_gene(make_train_model(), "ENSMUSG01")
  expect_equal(p$labels$title, "ENSMUSG01")
})

test_that("interactive = TRUE returns a plotly object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  p <- plot_train_gene(make_train_model(), "ENSMUSG01", gene_label = "Arntl",
                       interactive = TRUE)
  expect_s3_class(p, "plotly")
})
