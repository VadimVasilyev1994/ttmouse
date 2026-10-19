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
#' Time parsing: a numeric `time` column is used unchanged. A text `time`
#' column is converted to numeric hours, accepting only three unambiguous forms
#' (case-insensitive, surrounding whitespace ignored): a plain number (`4`,
#' `04`, `4.5`), `ZT<number>` (`ZT4`, `zt 04`) or `CT<number>` (`CT12`). Empty
#' cells are treated as missing. Any other value (e.g. `4h`, `T4`, `ZT4h`) is
#' not guessed at:
#' * on the time-course path (`require_time = TRUE`) it is an error, raised
#'   with class `ttmouse_time_error`, because time drives the normalisation;
#' * on the intergene path (`require_time = FALSE`) it is a warning, and those
#'   samples are given a missing (`NA`) time.
#'
#' A column mixing `ZT` and `CT` labels is accepted (both read as hours) with a
#' warning, since the two usually denote different lighting conditions.
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
#'   `replicate`), with `time` (when present) as numeric hours.
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

  # --- Time column -> numeric hours (strict ZT/CT parsing) -------------------
  if ("time" %in% colnames(df)) {
    parsed <- parse_metadata_time(df[["time"]])

    # Values in an unrecognised format are never guessed at.
    if (parsed$n_unrecognised > 0L) {
      msg <- sprintf(
        "Time column: %d sample(s) have a time in an unrecognised format (%s). %s",
        parsed$n_unrecognised, format_examples(parsed$unrecognised),
        TIME_FORMAT_HINT)
      if (require_time) {
        # Time-course path: time drives the normalisation, so stop here.
        stop_time_error(paste(msg, "Please correct the time column and re-upload."))
      }
      # Intergene path: time is optional, so continue with those times missing.
      warning(msg, " Those samples were given no time; the projection continues.",
              call. = FALSE)
    }

    # ZT and CT in one column usually means mixed lighting conditions.
    if (parsed$n_zt > 0L && parsed$n_ct > 0L) {
      warning(sprintf(
        "Time column mixes ZT (%d sample(s)) and CT (%d sample(s)) labels; both were read as hours. Please check this is intended.",
        parsed$n_zt, parsed$n_ct), call. = FALSE)
    }

    df[["time"]] <- parsed$time
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

# ---------------------------------------------------------------------------
# Time parsing helpers (internal)
# ---------------------------------------------------------------------------

#' Accepted time formats, quoted in time-format messages
#' @keywords internal
#' @noRd
TIME_FORMAT_HINT <- paste(
  "Accepted formats: a number of hours (e.g. 4), ZT<number> or CT<number>",
  "(e.g. ZT4, CT12; not case-sensitive).")

#' Parse a metadata time column into numeric hours (strict)
#'
#' Accepts only three unambiguous forms, matched case-insensitively and ignoring
#' surrounding whitespace: a plain number (`4`, `04`, `4.5`, `-2`), `ZT<number>`
#' (`ZT4`, `zt 04`) or `CT<number>` (`CT12`). Anything else is left as `NA` and
#' reported, so the caller decides whether to stop or warn. Empty cells and `NA`
#' are treated as missing and are not reported.
#'
#' @param x The raw `time` column as read from the file.
#' @return A list with `time` (numeric, one value per sample), `unrecognised`
#'   (the distinct unrecognised values), `n_unrecognised` (number of samples
#'   with one), and `n_zt` / `n_ct` (number of samples using each prefix).
#' @keywords internal
#' @noRd
parse_metadata_time <- function(x) {
  # Already numeric (a numeric CSV/XLSX column): nothing to parse.
  if (is.numeric(x)) {
    return(list(time = x, unrecognised = character(0), n_unrecognised = 0L,
                n_zt = 0L, n_ct = 0L))
  }

  val     <- trimws(as.character(x))
  missing <- is.na(val) | !nzchar(val)

  # An unsigned decimal number: 4, 04, 4.5 or .5 (no exponents, no units).
  num <- "([0-9]+(\\.[0-9]+)?|\\.[0-9]+)"

  # The three accepted forms; anything else matches none of them.
  is_plain <- grepl(paste0("^-?", num, "$"), val)
  is_zt    <- grepl(paste0("^zt[[:space:]]*", num, "$"), val, ignore.case = TRUE)
  is_ct    <- grepl(paste0("^ct[[:space:]]*", num, "$"), val, ignore.case = TRUE)
  prefixed <- is_zt | is_ct

  time <- rep(NA_real_, length(val))
  time[is_plain] <- as.numeric(val[is_plain])
  # Drop the two-letter prefix (and any space after it), keep the number.
  time[prefixed] <- as.numeric(sub("^[[:alpha:]]{2}[[:space:]]*", "",
                                   val[prefixed]))

  bad <- !missing & !is_plain & !prefixed
  list(time           = time,
       unrecognised   = unique(val[bad]),
       n_unrecognised = sum(bad),
       n_zt           = sum(is_zt),
       n_ct           = sum(is_ct))
}

#' Quote a few example values for a message: "'4h', 'T8' and 3 more"
#'
#' Keeps messages short when many values are affected.
#' @keywords internal
#' @noRd
format_examples <- function(values, max_show = 3L) {
  shown <- paste0("'", utils::head(values, max_show), "'", collapse = ", ")
  extra <- length(values) - max_show
  if (extra > 0L) paste0(shown, " and ", extra, " more") else shown
}

#' Signal a time-format error with its own condition class
#'
#' An ordinary error, but classed `ttmouse_time_error` so the app can show it as
#' a blocking (red) message.
#' @keywords internal
#' @noRd
stop_time_error <- function(message) {
  stop(structure(class = c("ttmouse_time_error", "error", "condition"),
                 list(message = message, call = NULL)))
}
