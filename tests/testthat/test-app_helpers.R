# test-app_helpers.R
#
# Tests for discover_assets() and metadata_to_projection_args(). Both are plain
# functions (no Shiny, no TimeTeller), so they run anywhere.

library(testthat)

# Build a temp data_dir with a shared gene_lengths.rds and one subdir per organ.
# `organs` is a named list: organ -> character vector of model types to create.
make_data_dir <- function(organs = list(liver = c("intergene", "timecourse"),
                                        scn   = c("intergene", "timecourse")),
                          gene_lengths = TRUE) {
  d <- tempfile("data")
  dir.create(d)
  if (gene_lengths) file.create(file.path(d, "gene_lengths.rds"))
  for (organ in names(organs)) {
    od <- file.path(d, organ); dir.create(od)
    for (type in organs[[organ]]) file.create(file.path(od, paste0(type, ".rds")))
  }
  d
}

# ---------------------------------------------------------------------------
# discover_assets()
# ---------------------------------------------------------------------------

test_that("discover_assets finds the shared gene_lengths and all organs", {
  d <- make_data_dir()
  found <- discover_assets(data_dir = d)
  expect_equal(found$gene_lengths, file.path(d, "gene_lengths.rds"))
  expect_setequal(names(found$organs), c("liver", "scn"))
  expect_setequal(names(found$organs$liver), c("intergene", "timecourse"))
  expect_true(file.exists(found$organs$scn$intergene))
})

test_that("a single organ is allowed (no error)", {
  d <- make_data_dir(organs = list(liver = c("intergene", "timecourse")))
  found <- discover_assets(data_dir = d)
  expect_equal(names(found$organs), "liver")
})

test_that("an organ with only one model type is kept with just that type", {
  d <- make_data_dir(organs = list(scn = "timecourse"))
  found <- discover_assets(data_dir = d)
  expect_equal(names(found$organs$scn), "timecourse")
  expect_null(found$organs$scn$intergene)
})

test_that("subdirectories with no model files are ignored", {
  d <- make_data_dir(organs = list(liver = "intergene"))
  dir.create(file.path(d, "notes"))           # empty subdir, no model files
  found <- discover_assets(data_dir = d)
  expect_equal(names(found$organs), "liver")
})

test_that("a missing gene_lengths.rds is an error", {
  d <- make_data_dir(gene_lengths = FALSE)
  expect_error(discover_assets(data_dir = d), "gene_lengths")
})

test_that("no organ models at all is an error", {
  d <- make_data_dir(organs = list())          # only gene_lengths, no organs
  expect_error(discover_assets(data_dir = d), "No organ models")
})

test_that("discover_assets falls back to TTMOUSE_DATA_DIR", {
  d <- make_data_dir(organs = list(liver = "intergene"))
  withr::with_envvar(c(TTMOUSE_DATA_DIR = d), {
    found <- discover_assets()
    expect_equal(names(found$organs), "liver")
  })
})

test_that("no directory available is an error", {
  withr::with_envvar(c(TTMOUSE_DATA_DIR = ""), {
    expect_error(discover_assets(), "No data directory")
  })
})

test_that("an explicit gene_lengths path overrides the conventional location", {
  d <- make_data_dir(organs = list(liver = "intergene"))
  custom <- tempfile(fileext = ".rds"); file.create(custom)
  found <- discover_assets(data_dir = d, gene_lengths = custom)
  expect_equal(found$gene_lengths, custom)
})

# ---------------------------------------------------------------------------
# metadata_to_projection_args() -- timecourse path (require_groups = TRUE)
# ---------------------------------------------------------------------------

test_that("metadata maps to test_time plus present grouping columns", {
  md <- data.frame(time = c(0, 6, 12, 18),
                   group_1 = c("WT", "WT", "KO", "KO"),
                   replicate = c(1, 2, 1, 2),
                   stringsAsFactors = FALSE)
  args <- metadata_to_projection_args(md)
  expect_equal(args$test_time, md$time)
  expect_equal(args$test_group_1, md$group_1)
  expect_equal(args$test_replicate, md$replicate)
  expect_null(args$test_group_2)
  expect_null(args$test_group_3)
})

test_that("missing all grouping columns is rejected by default (timecourse)", {
  md <- data.frame(time = c(0, 6, 12))
  expect_error(metadata_to_projection_args(md), "at least one grouping")
})

test_that("a non-data.frame input is rejected", {
  expect_error(metadata_to_projection_args(list(a = 1)), "data.frame")
})

test_that("all four grouping columns are forwarded when present", {
  md <- data.frame(time = 1:2, group_1 = 1:2, group_2 = 1:2,
                   group_3 = 1:2, replicate = 1:2)
  args <- metadata_to_projection_args(md)
  expect_true(all(c("test_time", "test_group_1", "test_group_2",
                    "test_group_3", "test_replicate") %in% names(args)))
})

# ---------------------------------------------------------------------------
# metadata_to_projection_args() -- intergene path (require_groups = FALSE)
# ---------------------------------------------------------------------------

test_that("require_groups = FALSE allows metadata with no grouping columns", {
  md <- data.frame(time = c(0, 6, 12))
  args <- metadata_to_projection_args(md, require_groups = FALSE)
  expect_equal(args$test_time, md$time)
  expect_named(args, "test_time")
})

test_that("require_groups = FALSE returns an empty list when nothing is recognised", {
  md <- data.frame(notes = c("x", "y"))
  args <- metadata_to_projection_args(md, require_groups = FALSE)
  expect_length(args, 0L)
})
