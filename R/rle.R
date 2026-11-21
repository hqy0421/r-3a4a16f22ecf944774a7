#' Read and write run-length encoded tibbles
#'
#' @description
#' A warehouse staging file stores every column as a sequence of runs,
#' each run pairing a value with the number of consecutive rows it spans.
#' Columns are independent: the run boundaries of one column generally do
#' not line up with those of another column.
#'
#' `write_tibble_rle()` compresses a tibble into this representation.
#' `read_tibble_rle()` is the reader entry point: it validates the staging
#' file, optionally filters rows on several columns at once, pages through
#' the surviving rows in their original order, and only expands the runs
#' for the rows that are actually returned.
#'
#' @section File format:
#' The file is serialized with [saveRDS()] and contains a list with the
#' following elements:
#'
#' * `version`: format version, currently `1L`.
#' * `nrow`: the declared number of rows, a nonnegative integer.
#' * `columns`: a named list, one element per column. Each element is a list
#'   with two elements:
#'     * `values`: the run values, in run order.
#'     * `lengths`: an integer vector of positive run lengths, one per run.
#'       The lengths of every column sum to `nrow`.
#'
#' @section Filtering:
#' `filter` is a named list of conditions. Names refer to columns, and a
#' column may carry at most one condition. A row is kept only if every
#' condition holds:
#'
#' * a bare value (e.g. `x = "a"`) is an equality condition. Comparison is
#'   type-stable via [vctrs::vec_equal()], and `NA` never matches an equality
#'   condition, even against an `NA` run value;
#' * `rle_is_na()` matches only runs whose value is `NA`.
#'
#' A missing value is therefore matched exclusively by the `rle_is_na()`
#' condition; equality conditions can never match it.
#'
#' All filtering and paging operate directly on the run-length segments.
#' The vectors stored in the file are expanded only for the rows that end
#' up in the result.
#'
#' @param file A connection or the path of the staging file to read.
#' @param x A tibble or data frame to write.
#' @param filter A named list of conditions, see section *Filtering*.
#'   `NULL` returns every row.
#' @param columns Columns to return, given as character names, integer
#'   positions, or `NULL` for every column. The result uses this order.
#' @param offset Number of surviving rows to skip before returning rows.
#'   Must be a nonnegative whole number.
#' @param limit Maximum number of surviving rows to return, or `Inf` for
#'   all remaining rows. Must be a positive whole number or `Inf`.
#' @param version File format version, currently only `1L` is supported.
#'
#' @return A [tibble][tibble-package] with the original column types,
#'   ordered as requested in `columns`. If no row matches, the columns are
#'   still present with zero rows and their original types. If `columns`
#'   selects zero columns, the result still carries the correct number of
#'   rows.
#'
#' @export
#' @examples
#' tbl <- tibble(
#'   group = c("a", "a", "b", "b", "b", NA),
#'   value = c(1, 1, 2, 2, 3, NA)
#' )
#'
#' path <- tempfile(fileext = ".rds")
#' write_tibble_rle(tbl, path)
#'
#' read_tibble_rle(path)
#' read_tibble_rle(path, filter = list(group = "b"))
#' read_tibble_rle(path, filter = list(group = rle_is_na()))
#' read_tibble_rle(path, filter = list(group = "b", value = 2))
#' read_tibble_rle(path, offset = 1, limit = 2)
#' read_tibble_rle(path, columns = c("value", "group"))
read_tibble_rle <- function(
  file,
  filter = NULL,
  columns = NULL,
  offset = 0,
  limit = Inf
) {
  staged <- readRDS(file)
  staged <- validate_rle_file(staged)

  nrow <- staged$nrow
  rle_cols <- staged$columns

  selected <- resolve_rle_columns(columns, names(rle_cols))
  filter <- validate_rle_filter(filter, names(rle_cols))
  offset <- validate_rle_scalar(offset, "offset", min = 0)
  limit <- validate_rle_limit(limit)

  if (nrow == 0L) {
    # Nothing to scan: construct empty columns straight from the prototypes.
    out <- lapply(rle_cols[selected], rle_col_ptype)
    return(new_tibble(out, nrow = 0L))
  }

  # Everything below works on the run-length segments. Stored vectors are
  # expanded only at the very end, for the rows that are returned.

  # Segment-level filtering: intersect matching runs across filter columns,
  # honoring their different segment boundaries, into kept row intervals.
  intervals <- rle_filter_intervals(rle_cols, filter, nrow)

  # Segment-level paging over the surviving rows, in original row order.
  locs <- rle_page_intervals(intervals, offset, limit)

  # Only the returned rows are expanded, and only for requested columns.
  out <- lapply(
    rle_cols[selected],
    rle_expand_at,
    locs = locs
  )
  new_tibble(out, nrow = length(locs))
}

#' @rdname read_tibble_rle
#' @export
write_tibble_rle <- function(x, file, version = 1L) {
  if (!is.data.frame(x)) {
    abort_rle_must_be_tibble()
  }
  if (!identical(version, 1L)) {
    abort_rle_unknown_version(version)
  }

  nrow <- nrow(x)
  columns <- lapply(unclass(x), rle_encode)

  staged <- list(
    version = 1L,
    nrow = as.integer(nrow),
    columns = columns
  )
  saveRDS(staged, file)
  invisible(staged)
}

#' @rdname read_tibble_rle
#' @export
rle_is_na <- function() {
  structure(list(), class = "tibble_rle_is_na")
}

# Encoding ----------------------------------------------------------------

rle_encode <- function(x) {
  n <- vec_size(x)
  if (n == 0L) {
    return(list(values = x, lengths = integer()))
  }

  # `x[i] == x[i - 1]` is not NA-safe, so compare every position to the
  # first position of the current run instead.
  run_start <- c(1L, which(!rle_equal_adjacent(x)) + 1L)
  run_end <- c(run_start[-1L] - 1L, n)
  lengths <- as.integer(run_end - run_start + 1L)
  values <- vctrs::vec_slice(x, run_start)

  list(values = values, lengths = lengths)
}

rle_equal_adjacent <- function(x) {
  # FALSE for every position that starts a new run. NA-safe: adjacent NAs
  # start separate runs so row-by-row round trips never collapse values.
  if (vec_size(x) < 2L) {
    return(logical())
  }
  vctrs::vec_equal(
    vctrs::vec_slice(x, -1L),
    vctrs::vec_slice(x, -vec_size(x)),
    na_equal = TRUE
  )
}

# Validation --------------------------------------------------------------

validate_rle_file <- function(staged) {
  if (!is.list(staged) || is.null(names(staged))) {
    abort_rle_bad_file()
  }
  if (!identical(staged$version, 1L)) {
    abort_rle_unknown_version(staged$version)
  }

  nrow <- staged$nrow
  if (
    !is.numeric(nrow) ||
      length(nrow) != 1L ||
      is.na(nrow) ||
      nrow < 0 ||
      !is_integerish(nrow, 1)
  ) {
    abort_rle_bad_nrow()
  }
  nrow <- as.integer(nrow)

  columns <- staged$columns
  if (!is.list(columns) || is.null(names(columns))) {
    abort_rle_bad_columns()
  }
  if (any(names(columns) == "" | is.na(names(columns)))) {
    abort_rle_bad_columns()
  }

  for (j in seq_along(columns)) {
    col <- columns[[j]]
    if (!is.list(col) || !all(c("values", "lengths") %in% names(col))) {
      abort_rle_bad_column(names(columns)[[j]])
    }

    lengths <- col$lengths
    if (
      !is.numeric(lengths) ||
        anyNA(lengths) ||
        length(lengths) != length(col$values) ||
        (nrow > 0L && length(lengths) == 0L) ||
        any(lengths <= 0) ||
        !all(is_integerish(lengths))
    ) {
      abort_rle_bad_lengths(names(columns)[[j]])
    }
    lengths <- as.integer(lengths)
    columns[[j]]$lengths <- lengths

    total <- sum(lengths)
    if (total != nrow) {
      abort_rle_nrow_mismatch(names(columns)[[j]], total, nrow)
    }
  }

  staged$nrow <- nrow
  staged$columns <- columns
  staged
}

resolve_rle_columns <- function(columns, col_names) {
  if (is.null(columns)) {
    return(seq_along(col_names))
  }

  locs <- vec_as_location(
    columns,
    n = length(col_names),
    names = col_names,
    arg = "columns"
  )
  if (anyDuplicated(locs) > 0) {
    abort_rle_duplicate_columns()
  }
  locs
}

validate_rle_filter <- function(filter, col_names) {
  if (is.null(filter)) {
    return(list())
  }
  if (!is.list(filter) || is.null(names(filter))) {
    abort_rle_bad_filter()
  }
  if (any(names(filter) == "" | is.na(names(filter)))) {
    abort_rle_bad_filter()
  }

  unknown <- setdiff(names(filter), col_names)
  if (has_length(unknown)) {
    abort_rle_unknown_filter_column(unknown)
  }
  if (anyDuplicated(names(filter)) > 0) {
    abort_rle_duplicate_filter_column(names(filter)[[anyDuplicated(names(filter))]])
  }

  for (nm in names(filter)) {
    cond <- filter[[nm]]
    if (inherits(cond, "tibble_rle_is_na")) {
      filter[[nm]] <- list(type = "is_na")
    } else {
      # Equality condition: any length-one value keeps its type, so that
      # comparisons stay type-stable (Date, factor, ... all supported).
      if (is.null(cond) || !vec_is(cond) || vec_size(cond) != 1L) {
        abort_rle_bad_condition(nm)
      }
      filter[[nm]] <- list(type = "eq", value = cond)
    }
  }
  filter
}

validate_rle_scalar <- function(x, arg, min) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x < min ||
      !is_integerish(x, 1)
  ) {
    abort_rle_bad_scalar(arg, min)
  }
  as.integer(x)
}

validate_rle_limit <- function(limit) {
  if (identical(limit, Inf)) {
    return(Inf)
  }
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      is.na(limit) ||
      limit <= 0 ||
      !is_integerish(limit, 1)
  ) {
    abort_rle_bad_limit()
  }
  as.integer(limit)
}

# Segment-level filtering and paging --------------------------------------
#
# All intermediate structures are closed [start, end] row intervals derived
# directly from the runs; no column is ever expanded to length `nrow`.

rle_filter_intervals <- function(columns, filter, nrow) {
  if (!has_length(filter)) {
    return(cbind(start = 1L, end = nrow))
  }

  intervals <- NULL
  for (nm in names(filter)) {
    col <- columns[[nm]]
    matched <- rle_match_runs(col, filter[[nm]])
    col_intervals <- rle_run_intervals(matched, col$lengths)
    intervals <- if (is.null(intervals)) {
      col_intervals
    } else {
      rle_intersect_intervals(intervals, col_intervals)
    }
  }
  intervals
}

rle_match_runs <- function(col, cond) {
  values <- col$values
  if (identical(cond$type, "is_na")) {
    is.na(values)
  } else {
    # NA run values never match, even if the condition value is NA.
    # na_equal = TRUE keeps the result free of NA values.
    !is.na(values) &
      vctrs::vec_equal(values, cond$value, na_equal = TRUE)
  }
}

rle_run_intervals <- function(matched, lengths) {
  starts <- integer()
  ends <- integer()
  pos <- 1L
  for (i in seq_along(lengths)) {
    if (matched[[i]]) {
      starts <- c(starts, pos)
      ends <- c(ends, pos + lengths[[i]] - 1L)
    }
    pos <- pos + lengths[[i]]
  }
  cbind(start = starts, end = ends)
}

rle_intersect_intervals <- function(x, y) {
  # Both inputs are disjoint, sorted, closed intervals.
  starts <- integer()
  ends <- integer()
  i <- 1L
  j <- 1L
  while (i <= nrow(x) && j <= nrow(y)) {
    lo <- max(x[[i, "start"]], y[[j, "start"]])
    hi <- min(x[[i, "end"]], y[[j, "end"]])
    if (lo <= hi) {
      starts <- c(starts, lo)
      ends <- c(ends, hi)
    }
    if (x[[i, "end"]] < y[[j, "end"]]) {
      i <- i + 1L
    } else {
      j <- j + 1L
    }
  }
  cbind(start = starts, end = ends)
}

rle_page_intervals <- function(intervals, offset, limit) {
  # Walk the kept intervals in order, skip the first `offset` kept rows,
  # then take at most `limit` rows. Returns the 1-based row locations.
  locs <- integer()
  taken <- 0L
  skipped <- 0L
  for (i in seq_len(nrow(intervals))) {
    start <- intervals[[i, "start"]]
    end <- intervals[[i, "end"]]
    width <- end - start + 1L

    if (skipped < offset) {
      skip_here <- min(width, offset - skipped)
      skipped <- skipped + skip_here
      start <- start + skip_here
      if (start > end) {
        next
      }
    }

    if (!identical(limit, Inf)) {
      remaining <- limit - taken
      if (remaining <= 0L) {
        break
      }
      if (end - start + 1L > remaining) {
        end <- start + remaining - 1L
      }
    }

    locs <- c(locs, start:end)
    taken <- length(locs)
    if (!identical(limit, Inf) && taken >= limit) {
      break
    }
  }
  locs
}

# Expansion of the final rows only ----------------------------------------

rle_expand_at <- function(col, locs) {
  if (length(locs) == 0L) {
    return(rle_col_ptype(col))
  }

  # Map each requested row to the run it belongs to, walking the segments
  # once instead of expanding the column to its full length.
  run_id <- integer(length(locs))
  boundaries <- cumsum(col$lengths)
  j <- 1L
  for (i in seq_along(locs)) {
    while (locs[[i]] > boundaries[[j]]) {
      j <- j + 1L
    }
    run_id[[i]] <- j
  }
  vctrs::vec_slice(col$values, run_id)
}

rle_col_ptype <- function(col) {
  vctrs::vec_slice(col$values, integer())
}

# Errors ------------------------------------------------------------------

abort_rle_must_be_tibble <- function(call = caller_env()) {
  tibble_abort(call = call, "`x` must be a data frame or tibble.")
}

abort_rle_unknown_version <- function(version, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("Unsupported staging file format version: ", version, ".")
  )
}

abort_rle_bad_file <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "Staging file must contain a named list with `version`, `nrow` and `columns`."
  )
}

abort_rle_bad_nrow <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "`nrow` in the staging file must be a nonnegative whole number."
  )
}

abort_rle_bad_columns <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "`columns` in the staging file must be a fully named list."
  )
}

abort_rle_bad_column <- function(name, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("Column ", tick(name), " must contain `values` and `lengths`.")
  )
}

abort_rle_bad_lengths <- function(name, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Run lengths of column ", tick(name),
      " must be positive integers, one per run value."
    )
  )
}

abort_rle_nrow_mismatch <- function(name, total, nrow, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Run lengths of column ", tick(name), " sum to ", total,
      ", not the declared ", nrow, " rows."
    )
  )
}

abort_rle_bad_filter <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "`filter` must be a fully named list of column conditions."
  )
}

abort_rle_unknown_filter_column <- function(names, call = caller_env()) {
  tibble_abort(
    call = call,
    c(
      "`filter` references unknown columns.",
      i = paste0("Unknown: ", paste(tick(names), collapse = ", "), ".")
    )
  )
}

abort_rle_duplicate_filter_column <- function(name, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("Column ", tick(name), " appears more than once in `filter`.")
  )
}

abort_rle_bad_condition <- function(name, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Filter condition for ", tick(name),
      " must be a length-one value or `rle_is_na()`."
    )
  )
}

abort_rle_duplicate_columns <- function(call = caller_env()) {
  tibble_abort(call = call, "`columns` must not contain duplicates.")
}

abort_rle_bad_scalar <- function(arg, min, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("`", arg, "` must be a whole number greater than or equal to ", min, ".")
  )
}

abort_rle_bad_limit <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "`limit` must be a positive whole number or `Inf`."
  )
}
