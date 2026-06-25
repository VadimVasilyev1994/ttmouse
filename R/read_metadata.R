#' Read an uploaded sample-metadata file
#'
#' Reads the per-sample metadata file (CSV or XLSX) that accompanies a count
#' matrix and returns it as a data.frame, ready to supply the grouping/time
#' arguments of [project_test_data()].
#'
#' Column contract: the recognised columns are `time`, `group_1`, `group_2`,
#' `group_3` and `replicate`. They may appear in any order and are matched
#' case-insensitively (e.g. `Time`, `TIME` and `time` all match). Recognised
#' columns are renamed to their canonical lower-case spelling so downstream code
#' can rely on exact names; any other columns are kept as-is. `time` is required
#' on the time-course path (`require_time = TRUE`) and optional on the intergene
#' path (`require_time = FALSE`), where all columns are optional.
#'
#' Row order is the caller's responsibility: the metadata rows must already be
#' in the same order as the columns (samples) of the count matrix. This function
#' does NOT reorder rows. When `counts` is supplied it only checks that the
#' number of metadata rows equals the number of samples.
#'
#' @param path Path to the uploaded `.csv` or `.xlsx` metadata file.
#' @param counts Optional count matrix (genes x samples), e.g. from
#'   [read_counts()]. When supplied, the number of metadata rows is checked
#'   against the number of sample columns (`ncol(counts)`); when `NULL` the
#'   check is skipped.
#' @param require_time If `TRUE` (default), a `time` column is required (the
#'   time-course path). Set `FALSE` for the intergene path, where metadata and
#'   all of its columns are optional.
#' @param sheet For `.xlsx`, the worksheet to read (name or index; default first).
#' @param verbose If `TRUE`, print a one-line summary of the columns found.
#'
#' @return A data.frame with one row per sample; recognised columns renamed to
#'   their canonical names (`time`, `group_1`, `group_2`, `group_3`,
#'   `replicate`).
#' @export
read_metadata <- function(path, counts = NULL, require_time = TRUE,
                          sheet = 1, verbose = FALSE) {

  if (!file.exists(path)) {
    stop("Metadata file not found: ", path)
  }

  ext <- tolower(tools::file_ext(path))

  # --- Read into a data.frame (each column is one metadata variable) --------
  if (ext == "csv") {
    # check.names = FALSE keeps the header names verbatim so the
    # case-insensitive matching below sees the user's original spelling;
    # row.names = NULL stops read.csv from silently consuming column 1 as row
    # names when the header width looks off.
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

  # --- Basic shape ----------------------------------------------------------
  if (ncol(df) < 1L) {
    stop("Metadata file has no columns.")
  }
  if (nrow(df) < 1L) {
    stop("Metadata file has no rows (samples).")
  }

  # --- Canonical column matching (case-insensitive) -------------------------
  # The recognised metadata columns.
  canonical <- c("time", "group_1", "group_2", "group_3", "replicate")

  # Map each actual column name to a canonical name by lower-casing and
  # trimming surrounding whitespace. Unrecognised columns map to NA.
  raw_names <- colnames(df)
  key       <- tolower(trimws(raw_names))
  matched   <- canonical[match(key, canonical)]   # canonical name, or NA

  # Two different columns collapsing to the same canonical name (e.g. `Time`
  # and `TIME`) is ambiguous: we would not know which one to use downstream.
  dup_canon <- matched[!is.na(matched) & duplicated(matched)]
  if (length(dup_canon) > 0L) {
    stop(sprintf("Metadata has multiple columns mapping to: %s. ",
                 paste(unique(dup_canon), collapse = ", ")),
         "Please keep a single column per name.")
  }

  # `time` is required on the time-course path, optional on intergene.
  if (require_time && !"time" %in% matched) {
    stop("Metadata must contain a 'time' column (got: ",
         paste(raw_names, collapse = ", "), ").")
  }

  # Rename only the recognised columns to their canonical spelling.
  rename_idx <- !is.na(matched)
  colnames(df)[rename_idx] <- matched[rename_idx]

  # --- Sample-count check (only when the count matrix is supplied) ----------
  if (!is.null(counts)) {
    n_samples <- ncol(counts)
    if (is.null(n_samples)) {
      stop("`counts` has no columns; expected a genes x samples matrix.")
    }
    if (nrow(df) != n_samples) {
      stop(sprintf(
        "Sample count mismatch: metadata has %d row(s) but the count matrix has %d sample(s). ",
        nrow(df), n_samples),
        "Metadata rows must correspond one-to-one (and in order) with count columns.")
    }
  }

  if (verbose) {
    found <- intersect(canonical, matched)
    cat(sprintf("read_metadata: %d sample(s); columns [%s] from %s\n",
                nrow(df),
                if (length(found)) paste(found, collapse = ", ") else "none recognised",
                basename(path)))
  }

  df
}
