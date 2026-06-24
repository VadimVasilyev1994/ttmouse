#' Convert raw counts to the model's training scale (log2 TPM + 1)
#'
#' Thin wrapper that converts a raw count matrix into the exact expression scale
#' the pre-built mouse models were trained on: log2(TPM + 1) with Ensembl gene
#' IDs. Its only jobs are to (1) pin the frozen preprocessing contract so it can
#' never drift, and (2) guard that the input really is a raw count matrix. All
#' gene-ID handling and gene-length matching is delegated to `counts_to_tpm()`.
#'
#' The frozen contract (must match how the models were trained) is
#' `return_format = "ensembl"`, `log_transform = TRUE`, `prior_count = 1`, and
#' `skip_filter = TRUE` (projection mode never drops genes, or a model's required
#' genes could go missing and `test_model()` would hard-error).
#'
#' @section Dependency:
#' Requires `counts_to_tpm()` to be available in the package (the user's own
#' function lives at `R/counts_to_tpm.R`). It uses `edgeR` and `DGEobj.utils`.
#'
#' @param counts Raw count matrix (genes x samples) with gene IDs as rownames.
#' @param gene_lengths Data frame with columns `gene_id`, `gene_length`,
#'   `gene_name` (built once by `data-raw/build_gene_lengths.R`).
#' @param verbose If `TRUE`, print progress.
#'
#' @return A numeric matrix of log2(TPM + 1) values with Ensembl rownames.
#' @export
preprocess_counts <- function(counts,
                              gene_lengths,
                              verbose = FALSE) {

  # --- Validate that `counts` is a raw count matrix -------------------------
  # counts_to_tpm() already validates rownames and the gene_lengths columns, so
  # here we only add the checks it does NOT make: that the input is numeric,
  # complete, and non-negative (i.e. genuinely raw counts, not already-logged or
  # normalised data, which would otherwise pass silently and give wrong TPM).
  if (is.data.frame(counts)) counts <- as.matrix(counts)
  if (!is.matrix(counts)) {
    stop("`counts` must be a matrix or data.frame of raw counts (genes x samples).")
  }
  if (!is.numeric(counts)) {
    stop("`counts` must be numeric (raw counts).")
  }
  if (is.null(rownames(counts)) || all(rownames(counts) == "")) {
    stop("`counts` must have rownames (Ensembl IDs or gene symbols).")
  }
  if (nrow(counts) < 1L || ncol(counts) < 1L) {
    stop("`counts` must have at least one gene (row) and one sample (column).")
  }
  if (any(!is.finite(counts))) {
    stop("`counts` contains NA/NaN/Inf; raw counts must be finite.")
  }
  if (any(counts < 0)) {
    stop("`counts` contains negative values; expected raw counts, not log/normalised data.")
  }
  # Note: non-integer values are allowed on purpose. Estimated counts from tools
  # such as salmon/RSEM are non-integer but valid input for TPM conversion.

  if (verbose) {
    cat(sprintf("preprocess_counts: %d genes x %d samples; applying frozen TPM contract\n",
                nrow(counts), ncol(counts)))
  }

  # --- Delegate to the project's TPM routine with the pinned contract -------
  counts_to_tpm(
    counts        = counts,
    gene_lengths  = gene_lengths,
    return_format = "ensembl",
    log_transform = TRUE,
    prior_count   = 1,
    skip_filter   = TRUE,
    verbose       = verbose
  )
}
