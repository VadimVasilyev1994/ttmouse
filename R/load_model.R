#' Canonical TimeTeller normalisation methods
#'
#' The normalisation methods a trained TimeTeller model may carry, taken verbatim
#' from the dispatch in `normalise_test_data()` (Testing_functions.R).
#' @keywords internal
#' @noRd
TT_NORM_METHODS <- c("intergene", "clr", "timecourse",
                     "timecourse_matched", "combined")

#' Load and validate a pre-built TimeTeller model
#'
#' Reads a model `.rds` once at startup and sanity-checks that it is a usable
#' TimeTeller model, so a wrong or corrupt object fails loudly at startup rather
#' than deep inside projection.
#'
#' Verified field paths (against VadimVasilyev1994/TimeTeller-v2, master):
#' `Normalisation_choice` at `object[["Normalisation_choice"]]`
#' (Training_functions.R:92 / Testing_functions.R:3); the gene set at
#' `object[["Metadata"]][["Train"]][["Genes_Used"]]` (Training_functions.R:34,
#' hard-checked Testing_functions.R:67); and the training log threshold at
#' `object[["Train_Data"]][["LogThresh_Train"]]` (Training_functions.R:540,
#' read :723). The log threshold is applied automatically at projection time, so
#' a model lacking it is rejected here.
#'
#' @param path Path to the model `.rds`.
#' @param verbose If `TRUE`, print a one-line summary.
#' @return The loaded model object (a list), unchanged.
#' @export
load_model <- function(path, verbose = FALSE) {
  if (!file.exists(path)) {
    stop("Model file not found: ", path)
  }

  model <- readRDS(path)

  # --- Structural sanity checks -------------------------------------------
  if (!is.list(model)) {
    stop("Loaded object is not a TimeTeller model (expected a list), got: ",
         class(model)[1])
  }

  norm <- model[['Normalisation_choice']]
  if (is.null(norm) || length(norm) != 1L || !is.character(norm)) {
    stop("Model is missing a valid 'Normalisation_choice'.")
  }
  if (!norm %in% TT_NORM_METHODS) {
    stop("Model 'Normalisation_choice' is '", norm,
         "', not one of: ", paste(TT_NORM_METHODS, collapse = ", "))
  }

  genes <- model[['Metadata']][['Train']][['Genes_Used']]
  if (is.null(genes) || !is.character(genes) || length(genes) == 0L) {
    stop("Model is missing Metadata$Train$Genes_Used ",
         "(expected a non-empty character vector of gene IDs).")
  }

  # The app applies this threshold automatically at projection time, so a model
  # without it cannot be used and should be rejected here.
  log_thresh <- model[['Train_Data']][['LogThresh_Train']]
  if (is.null(log_thresh) || !is.numeric(log_thresh) || length(log_thresh) != 1L) {
    stop("Model is missing Train_Data$LogThresh_Train ",
         "(expected a single numeric log threshold).")
  }

  if (verbose) {
    cat(sprintf("Loaded model: normalisation = '%s', %d training genes, log_thresh = %g\n",
                norm, length(genes), log_thresh))
  }

  model
}

#' Read the model's normalisation method
#' @param model A model loaded by [load_model()].
#' @return Character scalar, one of the TimeTeller normalisation methods.
#' @export
model_normalisation <- function(model) {
  model[['Normalisation_choice']]
}

#' Read the model's required gene set
#' @param model A model loaded by [load_model()].
#' @return Character vector of gene IDs the model requires.
#' @export
model_genes <- function(model) {
  model[['Metadata']][['Train']][['Genes_Used']]
}

#' Read the model's training log threshold
#'
#' Returns the log threshold the model was calibrated with; used as the default
#' `log_thresh` in [project_test_data()], so it is applied automatically.
#' @param model A model loaded by [load_model()].
#' @return Numeric scalar.
#' @export
model_log_thresh <- function(model) {
  model[['Train_Data']][['LogThresh_Train']]
}
