
#' @importFrom stats complete.cases
counts_to_tpm <- function(counts,
                          gene_lengths,
                          return_format  = c("ensembl", "symbol"),
                          log_transform  = FALSE,
                          prior_count    = 1,
                          skip_filter    = FALSE,
                          verbose        = TRUE) {
  
  return_format <- match.arg(return_format)
  
  required_cols <- c("gene_id", "gene_length", "gene_name")
  missing_cols  <- setdiff(required_cols, colnames(gene_lengths))
  if (length(missing_cols) > 0) {
    stop("gene_lengths is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  if (!is.matrix(counts)) counts <- as.matrix(counts)
  if (is.null(rownames(counts)) || all(rownames(counts) == "")) {
    stop("Count matrix must have rownames (gene symbols or Ensembl IDs).")
  }
  
  n_genes_input <- nrow(counts)
  n_samples     <- ncol(counts)
  if (verbose) cat("Input:", n_genes_input, "genes x", n_samples, "samples\n")
  
  # Detect input format
  rn <- rownames(counts)
  ens_fraction <- mean(grepl("^ENS[A-Z]*G\\d+", rn))
  input_is_ensembl <- ens_fraction > 0.5
  
  if (verbose) {
    fmt_detected <- if (input_is_ensembl) "Ensembl IDs" else "gene symbols"
    cat("Detected input format:", fmt_detected,
        sprintf("(%.0f%% Ensembl pattern)\n", ens_fraction * 100))
  }
  
  # Strip Ensembl version suffixes
  if (input_is_ensembl) {
    rn_clean <- sub("\\.\\d+$", "", rn)
    n_stripped <- sum(rn != rn_clean)
    if (n_stripped > 0 && verbose)
      cat("Stripped version suffix from", n_stripped, "Ensembl IDs\n")
    rownames(counts) <- rn_clean
  }
  
  # Mouse Arntl/Bmal1 alias resolution
  if (!input_is_ensembl) {
    count_names  <- rownames(counts)
    length_names <- gene_lengths$gene_name
    
    has_arntl_counts  <- "Arntl" %in% count_names
    has_bmal1_counts  <- "Bmal1" %in% count_names
    has_arntl_lengths <- "Arntl" %in% length_names
    has_bmal1_lengths <- "Bmal1" %in% length_names
    if (has_bmal1_counts && !has_arntl_counts && has_arntl_lengths && !has_bmal1_lengths) {
      rownames(counts)[rownames(counts) == "Bmal1"] <- "Arntl"
      if (verbose) cat("Renamed Bmal1 -> Arntl in count matrix\n")
    } else if (has_arntl_counts && !has_bmal1_counts && has_bmal1_lengths && !has_arntl_lengths) {
      rownames(counts)[rownames(counts) == "Arntl"] <- "Bmal1"
      if (verbose) cat("Renamed Arntl -> Bmal1 in count matrix\n")
    } else if (has_arntl_counts && has_bmal1_counts) {
      warning("Both Arntl and Bmal1 found in count matrix. Review manually.")
    }
    
    # Human ARNTL/BMAL1 alias resolution (parallel block for uppercase symbols)
    has_ARNTL_counts  <- "ARNTL" %in% count_names
    has_BMAL1_counts  <- "BMAL1" %in% count_names
    has_ARNTL_lengths <- "ARNTL" %in% length_names
    has_BMAL1_lengths <- "BMAL1" %in% length_names
    if (has_BMAL1_counts && !has_ARNTL_counts && has_ARNTL_lengths && !has_BMAL1_lengths) {
      rownames(counts)[rownames(counts) == "BMAL1"] <- "ARNTL"
      if (verbose) cat("Renamed BMAL1 -> ARNTL in count matrix\n")
    } else if (has_ARNTL_counts && !has_BMAL1_counts && has_BMAL1_lengths && !has_ARNTL_lengths) {
      rownames(counts)[rownames(counts) == "ARNTL"] <- "BMAL1"
      if (verbose) cat("Renamed ARNTL -> BMAL1 in count matrix\n")
    } else if (has_ARNTL_counts && has_BMAL1_counts) {
      warning("Both ARNTL and BMAL1 found in count matrix. Review manually.")
    }
  }
  
  match_col <- if (input_is_ensembl) "gene_id" else "gene_name"
  
  gl <- gene_lengths[complete.cases(gene_lengths[, c(match_col, "gene_length")]), ]
  
  if (anyDuplicated(gl[[match_col]])) {
    n_dup_gl <- sum(duplicated(gl[[match_col]]))
    if (verbose)
      cat("Removed", n_dup_gl, "duplicate entries from gene_lengths (kept first)\n")
    gl <- gl[!duplicated(gl[[match_col]]), ]
  }
  
  if (anyDuplicated(rownames(counts))) {
    dup_names <- unique(rownames(counts)[duplicated(rownames(counts))])
    row_sums  <- rowSums(counts)
    keep_idx  <- logical(nrow(counts))
    keep_idx[!rownames(counts) %in% dup_names] <- TRUE
    for (dn in dup_names) {
      idx      <- which(rownames(counts) == dn)
      best_idx <- idx[which.max(row_sums[idx])]
      keep_idx[best_idx] <- TRUE
    }
    n_dup_removed <- nrow(counts) - sum(keep_idx)
    counts <- counts[keep_idx, , drop = FALSE]
    if (verbose)
      cat("Resolved", length(dup_names), "duplicated gene names; removed",
          n_dup_removed, "lower-count rows\n")
  }
  
  genes_present <- intersect(rownames(counts), gl[[match_col]])
  if (length(genes_present) == 0) {
    stop("No genes in common between count matrix and gene_lengths. ",
         "Check identifier format.")
  }
  rownames(gl) <- gl[[match_col]]
  gl     <- gl[genes_present, ]
  counts <- counts[genes_present, , drop = FALSE]
  if (verbose) {
    n_lost <- n_genes_input - length(genes_present)
    cat("Matched", length(genes_present), "genes with length info;",
        n_lost, "genes had no length annotation\n")
  }
  
  # filterByExpr — now skippable for projection
  if (skip_filter) {
    if (verbose) cat("Skipping filterByExpr (projection mode)\n")
    filtered_counts <- counts
  } else {
    dge  <- edgeR::DGEList(counts = counts)
    keep <- edgeR::filterByExpr(dge)
    dge  <- dge[keep, , keep.lib.sizes = FALSE]
    gl   <- gl[rownames(dge$counts), ]
    filtered_counts <- dge$counts
    if (verbose)
      cat("After filterByExpr:", nrow(filtered_counts), "genes retained",
          sprintf("(%d removed)\n", length(genes_present) - nrow(filtered_counts)))
  }
  
  tpm <- DGEobj.utils::convertCounts(
    filtered_counts,
    unit       = "tpm",
    geneLength = gl$gene_length,
    log        = FALSE,
    normalize  = "none"
  )
  
  if (log_transform) {
    tpm <- log2(tpm + prior_count)
    if (verbose) cat("Applied log2(TPM +", prior_count, ") transformation\n")
  }
  
  if (return_format == "ensembl" && !input_is_ensembl) {
    rownames(tpm) <- gl$gene_id
  } else if (return_format == "symbol" && input_is_ensembl) {
    rownames(tpm) <- gl$gene_name
  }
  
  if (verbose)
    cat("Output:", nrow(tpm), "genes x", ncol(tpm), "samples",
        sprintf("(rownames: %s)\n", return_format))
  
  tpm
}