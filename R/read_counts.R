#' Read an uploaded raw-count file into a numeric matrix
#'
#' Reads an uploaded raw-count file (CSV or XLSX) and returns the clean numeric
#' matrix the rest of the pipeline expects: genes in rows, samples in columns,
#' rownames = gene IDs. This is the ingestion front-door before
#' [preprocess_counts()].
#'
#' Input file contract: column 1 holds the gene IDs (used as rownames) and every
#' other column is one numeric sample-count column (no annotation columns). CSV is
#' comma-separated; XLSX is read from `sheet` (default the first) with the first
#' row as the header.
#'
#' Only the *structure* is validated here (a numeric matrix with unique gene-ID
#' rownames and named sample columns); count *values* (finite, non-negative) are
#' checked later in [preprocess_counts()], so they are not duplicated.
#'
#' @param path Path to the uploaded `.csv` or `.xlsx` file.
#' @param sheet For `.xlsx`, the worksheet to read (name or index; default first).
#' @param verbose If `TRUE`, print a one-line summary.
#'
#' @return A numeric matrix (genes x samples) with gene IDs as rownames.
#' @export
read_counts <- function(path, sheet = 1, verbose = FALSE) {

  if (!file.exists(path)) {
    stop("Counts file not found: ", path)
  }

  ext <- tolower(tools::file_ext(path))

  # --- Read into a data.frame: col 1 = IDs, cols 2+ = samples ---------------
  if (ext == "csv") {
    # row.names = NULL is defensive: without it, a file whose header is missing
    # the gene-ID column name makes read.csv silently consume column 1 as row
    # names, mis-shifting every column. With it, column 1 always stays the
    # gene-ID column (verified behaviour).
    df <- utils::read.csv(path, header = TRUE, check.names = FALSE,
                          stringsAsFactors = FALSE, row.names = NULL)
  } else if (ext == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Reading .xlsx requires the 'openxlsx' package.")
    }
    df <- openxlsx::read.xlsx(path, sheet = sheet, colNames = TRUE)
  } else {
    stop("Unsupported file type '.", ext,
         "'. Please upload a .csv or .xlsx file.")
  }

  # --- Structural validation ------------------------------------------------
  if (ncol(df) < 2L) {
    stop("Counts file must have a gene-ID column plus at least one sample column.")
  }
  if (nrow(df) < 1L) {
    stop("Counts file has no rows (genes).")
  }

  gene_ids     <- as.character(df[[1]])
  sample_cols  <- df[, -1, drop = FALSE]
  sample_names <- colnames(sample_cols)

  # Gene IDs must be present and unique: they become matrix rownames, and
  # duplicates would make downstream gene subsetting (exp_matrix[genes, ]) pick
  # an arbitrary row.
  if (any(is.na(gene_ids) | gene_ids == "")) {
    stop("Column 1 contains missing/empty gene IDs.")
  }
  if (anyDuplicated(gene_ids)) {
    dups <- unique(gene_ids[duplicated(gene_ids)])
    stop(sprintf("Column 1 has %d duplicated gene ID(s), e.g.: %s",
                 length(dups), paste(utils::head(dups, 5), collapse = ", ")))
  }

  # Every column after the first must be a numeric sample-count column. A
  # non-numeric column usually means a stray annotation column was left in,
  # which violates the agreed contract (samples only after column 1).
  non_numeric <- !vapply(sample_cols, is.numeric, logical(1))
  if (any(non_numeric)) {
    stop(sprintf("These column(s) are not numeric sample counts: %s. ",
                 paste(sample_names[non_numeric], collapse = ", ")),
         "Expected gene IDs in column 1 and only sample counts after it.")
  }

  # --- Build the numeric matrix --------------------------------------------
  mat <- as.matrix(sample_cols)
  rownames(mat) <- gene_ids
  colnames(mat) <- sample_names

  if (verbose) {
    cat(sprintf("read_counts: %d genes x %d samples from %s\n",
                nrow(mat), ncol(mat), basename(path)))
  }

  mat
}
