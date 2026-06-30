#' Check per-group time coverage before time-course projection
#'
#' Time-course normalisation scales each time-series group across its own
#' samples (TimeTeller-v2 `normalise_test_data()`, the plain `timecourse`
#' method). When a group covers only a small window of circadian time, that
#' within-group scaling is unreliable. This check computes each group's time
#' span (`max(time) - min(time)`, in hours) and flags any whose span does not
#' exceed `min_span_hours`.
#'
#' Groups are formed exactly as the model forms them: by concatenating
#' `group_1/2/3` and `replicate` with `NA` treated as `""`
#' (TimeTeller-v2 @ f3cbd44, `Testing_functions.R`:
#' `paste0(replace_na(g1,''), ..., replace_na(rep,''))`). The group key is used
#' verbatim as the group label, matching TimeTeller's own group naming.
#'
#' Like [validate_genes()], this reports failures (it does not stop on them) so
#' the app can show a clear message and block projection gracefully; it only
#' stops on malformed input (non-numeric times, or grouping vectors whose length
#' does not match `test_time`).
#'
#' @param test_time Numeric sample times, in hours (one per sample). Character
#'   values that parse as numbers are accepted; anything else is an error.
#' @param test_group_1,test_group_2,test_group_3,test_replicate Optional grouping
#'   vectors (one value per sample), as produced by
#'   [metadata_to_projection_args()]. Whichever are supplied define the groups;
#'   at least one must be supplied (the time-course path always supplies one).
#' @param min_span_hours Smallest acceptable per-group span. A group is flagged
#'   when its span is **<=** this value (default 12); a full ~24 h circadian
#'   cycle is recommended for reliable results.
#'
#' @return A list with: `ok` (TRUE only if every group's span exceeds
#'   `min_span_hours`), `n_groups`, `n_failing`, a `failing` data.frame
#'   (`group`, `n`, `span`) for the flagged groups (smallest span first), the
#'   overall smallest `min_span`, and a display `summary` string.
#' @export
check_time_coverage <- function(test_time,
                                test_group_1   = NULL,
                                test_group_2   = NULL,
                                test_group_3   = NULL,
                                test_replicate = NULL,
                                min_span_hours = 12) {

  # --- Coerce + guard time (coverage is undefined without numeric hours) ----
  n <- length(test_time)
  if (n == 0L) {
    stop("`test_time` is empty; expected one time value per sample.")
  }
  time_num <- suppressWarnings(as.numeric(test_time))
  if (anyNA(time_num)) {
    bad <- unique(test_time[is.na(time_num)])
    stop("`test_time` must be numeric (hours); could not parse: ",
         paste(bad, collapse = ", "), ".")
  }

  # --- Build the group key EXACTLY as TimeTeller's timecourse path does ------
  # Absent grouping vectors (NULL) contribute "" for every sample, matching
  # add_test_data()'s default of rep(NA, n) -> "" in normalise_test_data().
  as_key_part <- function(v, label) {
    if (is.null(v)) return(rep("", n))
    if (length(v) != n) {
      stop(sprintf("`%s` has length %d but `test_time` has length %d; ",
                   label, length(v), n),
           "grouping vectors must be one value per sample.")
    }
    v <- as.character(v)
    v[is.na(v)] <- ""
    v
  }
  key <- paste0(as_key_part(test_group_1,   "test_group_1"),
                as_key_part(test_group_2,   "test_group_2"),
                as_key_part(test_group_3,   "test_group_3"),
                as_key_part(test_replicate, "test_replicate"))

  if (all(key == "")) {
    stop("No grouping information supplied; time-course grouping needs at ",
         "least one of test_group_1/2/3 or test_replicate.")
  }

  # --- Per-group span = max(time) - min(time) -------------------------------
  groups <- unique(key)
  span   <- vapply(groups, function(g) {
    tt <- time_num[key == g]
    max(tt) - min(tt)
  }, numeric(1))
  n_in_grp <- vapply(groups, function(g) sum(key == g), integer(1))

  fail <- span <= min_span_hours          # <= threshold => flagged
  ok   <- !any(fail)

  # Flagged groups, smallest span first (most problematic on top).
  failing <- data.frame(group = groups[fail],
                        n     = n_in_grp[fail],
                        span  = span[fail],
                        stringsAsFactors = FALSE,
                        row.names = NULL)
  failing <- failing[order(failing$span), , drop = FALSE]

  # --- Human-readable summary ----------------------------------------------
  if (ok) {
    summary <- sprintf("Time coverage OK: all %d group(s) span more than %g h.",
                       length(groups), min_span_hours)
  } else {
    lines <- sprintf("  - group '%s' (%d sample(s)): span %g h",
                     failing$group, failing$n, failing$span)
    summary <- paste0(
      sprintf(paste0("Insufficient time coverage for time-course ",
                     "normalisation: %d of %d group(s) span <= %g h.\n"),
              nrow(failing), length(groups), min_span_hours),
      paste(lines, collapse = "\n"),
      "\n\nTime-course normalisation is unreliable over such a short window; ",
      "a full ~24 h circadian cycle is recommended for accurate results. ",
      "Please extend the sampled time range (or add time points) for the ",
      "flagged group(s) and re-run.")
  }

  list(
    ok        = ok,                  # TRUE only if every group's span > threshold
    n_groups  = length(groups),      # distinct time-course groups
    n_failing = nrow(failing),       # how many were flagged
    failing   = failing,             # group / n / span for the flagged groups
    min_span  = min(span),           # smallest span across all groups
    summary   = summary              # display string (lists flagged groups)
  )
}
