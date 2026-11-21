test_that("write_tibble_rle() / read_tibble_rle() round-trips all types", {
  tbl <- tibble(
    a = c(1L, 1L, 2L, NA_integer_),
    b = c("x", "x", "y", NA_character_),
    d = c(1.5, 1.5, 2.5, 3.5),
    lgl = c(TRUE, TRUE, FALSE, NA),
    fct = factor(c("a", "a", "b", NA)),
    dt = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-03")),
    lc = list(1, 1, "a", NULL)
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))

  write_tibble_rle(tbl, path)
  expect_identical(read_tibble_rle(path), tbl)
})

test_that("columns are stored as independent runs", {
  tbl <- tibble(
    a = c(1L, 1L, 2L, 2L, 2L, 3L),
    b = c("x", "y", "y", "z", "z", "z")
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))

  staged <- write_tibble_rle(tbl, path)
  expect_identical(staged$columns$a$lengths, c(2L, 3L, 1L))
  expect_identical(staged$columns$b$lengths, c(1L, 2L, 3L))
})

test_that("equality filtering works across differing segment boundaries", {
  tbl <- tibble(
    a = c(1L, 1L, 1L, 2L, 2L, 2L, 1L),
    b = c("x", "x", "y", "y", "y", "x", "x")
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  expect_identical(
    read_tibble_rle(path, filter = list(a = 1L)),
    tbl[c(1L, 2L, 3L, 7L), ],
    ignore_attr = "row.names"
  )
  expect_identical(
    read_tibble_rle(path, filter = list(a = 1L, b = "x")),
    tibble(a = c(1L, 1L, 1L), b = c("x", "x", "x"))
  )
  expect_identical(
    read_tibble_rle(path, filter = list(b = "y")),
    tibble(a = c(1L, 2L, 2L), b = c("y", "y", "y"))
  )
})

test_that("missing values are matched only by rle_is_na()", {
  tbl <- tibble(
    a = c(1L, NA_integer_, NA_integer_, 2L),
    b = c("x", NA_character_, "y", "y")
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  expect_identical(
    read_tibble_rle(path, filter = list(a = rle_is_na())),
    tibble(a = c(NA_integer_, NA_integer_), b = c(NA_character_, "y"))
  )
  # Equality against NA never matches, including NA values themselves.
  expect_equal(nrow(read_tibble_rle(path, filter = list(a = NA_integer_))), 0L)
  expect_equal(
    nrow(read_tibble_rle(path, filter = list(b = NA_character_))),
    0L
  )

  expect_identical(
    read_tibble_rle(path, filter = list(a = rle_is_na(), b = rle_is_na())),
    tibble(a = NA_integer_, b = NA_character_)
  )
  expect_equal(
    nrow(read_tibble_rle(path, filter = list(a = rle_is_na(), b = "y"))),
    1L
  )
})

test_that("offset and limit page over surviving rows in original order", {
  tbl <- tibble(a = rep(1L:3L, each = 3L))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  expect_identical(
    read_tibble_rle(path, offset = 2L)$a,
    c(1L, 2L, 2L, 2L, 3L, 3L, 3L)
  )
  expect_identical(read_tibble_rle(path, offset = 2, limit = 2)$a, c(1L, 2L))
  expect_identical(read_tibble_rle(path, limit = 2)$a, c(1L, 1L))
  expect_equal(nrow(read_tibble_rle(path, offset = 9L)), 0L)
  expect_equal(nrow(read_tibble_rle(path, offset = 100L)), 0L)

  # Paging applies after filtering.
  out <- read_tibble_rle(path, filter = list(a = 2L), offset = 1L, limit = 1L)
  expect_identical(out$a, 2L)
  expect_equal(nrow(out), 1L)
})

test_that("paging respects interval boundaries", {
  # Kept rows land in several separated intervals; offsets cross them.
  tbl <- tibble(a = c(1L, 2L, 1L, 2L, 1L, 2L, 1L))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  expect_identical(
    read_tibble_rle(path, filter = list(a = 1L), offset = 1L, limit = 2L)$a,
    c(1L, 1L)
  )
  expect_identical(
    read_tibble_rle(path, filter = list(a = 1L), offset = 2L)$a,
    c(1L, 1L)
  )
  expect_identical(
    read_tibble_rle(path, filter = list(a = 2L), limit = 2L)$a,
    c(2L, 2L)
  )
})

test_that("columns are returned in requested order and subset", {
  tbl <- tibble(a = 1:3, b = 4:6, c = 7:9)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  out <- read_tibble_rle(path, columns = c("c", "a"))
  expect_named(out, c("c", "a"))
  expect_identical(out$c, 7:9)
  expect_identical(out$a, 1:3)

  out_pos <- read_tibble_rle(path, columns = c(3L, 1L))
  expect_named(out_pos, c("c", "a"))
})

test_that("zero selected columns still report the correct row count", {
  tbl <- tibble(a = 1:3, b = 4:6)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  out <- read_tibble_rle(path, columns = character())
  expect_equal(nrow(out), 3L)
  expect_equal(ncol(out), 0L)

  out_f <- read_tibble_rle(path, filter = list(a = 2L), columns = character())
  expect_equal(nrow(out_f), 1L)
  expect_equal(ncol(out_f), 0L)
})

test_that("empty results preserve column types", {
  tbl <- tibble(
    i = 1:3,
    d = c(1.5, 2.5, 3.5),
    ch = c("a", "b", "c"),
    fct = factor(c("a", "b", "c")),
    dt = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  out <- read_tibble_rle(path, filter = list(ch = "zzz"))
  expect_equal(nrow(out), 0L)
  expect_named(out, names(tbl))
  expect_identical(typeof(out$i), "integer")
  expect_identical(typeof(out$d), "double")
  expect_identical(typeof(out$ch), "character")
  expect_s3_class(out$fct, "factor")
  expect_s3_class(out$dt, "Date")

  out2 <- read_tibble_rle(
    path,
    filter = list(ch = "zzz"),
    columns = c("dt", "i")
  )
  expect_named(out2, c("dt", "i"))
  expect_s3_class(out2$dt, "Date")
  expect_identical(typeof(out2$i), "integer")
})

test_that("zero-row tibbles round-trip with types", {
  tbl <- tibble(a = integer(), b = character(), d = double())
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  out <- read_tibble_rle(path)
  expect_identical(out, tbl)
  expect_identical(read_tibble_rle(path, filter = list(a = 1L)), tbl)
  expect_equal(nrow(read_tibble_rle(path, columns = character())), 0L)
})

test_that("filtering and paging never expand the full columns", {
  # A column with a single long run: expanding it would materialize 10k
  # values even when only one row is requested. Spy via a value vector that
  # errors on length() beyond its real size is impractical, so instead assert
  # the structural property directly on the internal helpers.
  col <- list(values = c("a", "b"), lengths = c(10000L, 1L))
  staged <- list(
    version = 1L,
    nrow = 10001L,
    columns = list(
      x = col,
      y = list(values = 1:3, lengths = c(4000L, 4000L, 2001L))
    )
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(staged, path)

  out <- read_tibble_rle(path, filter = list(x = "b"), limit = 1L)
  expect_equal(nrow(out), 1L)
  expect_identical(out$x, "b")

  intervals <- rle_filter_intervals(
    staged$columns,
    list(x = list(type = "eq", value = "a")),
    staged$nrow
  )
  expect_equal(nrow(intervals), 1L)
  expect_identical(intervals[1L, ], c(start = 1L, end = 10000L))

  locs <- rle_page_intervals(intervals, offset = 9998L, limit = 2L)
  expect_identical(locs, 9999L:10000L)
})

test_that("randomized filtering matches plain tibble semantics", {
  set.seed(42)
  n <- 200L
  tbl <- tibble(
    a = sample(c(1L, 2L, NA_integer_), n, replace = TRUE),
    b = sample(c("x", "y", NA_character_), n, replace = TRUE)
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  expect_identical(read_tibble_rle(path), tbl)

  expected <- tbl[which(tbl$a == 2L), ]
  row.names(expected) <- NULL
  got <- read_tibble_rle(path, filter = list(a = 2L))
  expect_identical(got, expected)

  expected <- tbl[which(!is.na(tbl$a) & tbl$b == "x"), ]
  row.names(expected) <- NULL
  got <- read_tibble_rle(path, filter = list(a = 2L, b = "x"))
  expect_equal(
    got,
    tbl[which(tbl$a == 2L & tbl$b == "x"), ],
    ignore_attr = "row.names"
  )

  expected_na <- tbl[which(is.na(tbl$a)), ]
  row.names(expected_na) <- NULL
  expect_identical(
    read_tibble_rle(path, filter = list(a = rle_is_na())),
    expected_na
  )

  for (off in c(0L, 5L, 50L)) {
    for (lim in c(1L, 7L, Inf)) {
      got <- read_tibble_rle(
        path,
        filter = list(b = "x"),
        offset = off,
        limit = lim
      )
      base <- which(tbl$b == "x")
      if (off < length(base)) {
        base <- base[(off + 1L):length(base)]
        if (!identical(lim, Inf)) {
          base <- base[seq_len(min(lim, length(base)))]
        }
      } else {
        base <- integer()
      }
      want <- tbl[base, ]
      row.names(want) <- NULL
      expect_identical(got, want)
    }
  }
})

test_that("validation rejects malformed staging files", {
  good <- list(
    version = 1L,
    nrow = 3L,
    columns = list(
      a = list(values = c(1L, 2L), lengths = c(1L, 2L)),
      b = list(values = c("x"), lengths = 3L)
    )
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(good, path)
  expect_s3_class(read_tibble_rle(path), "tbl_df")

  bad <- good
  bad$version <- 2L
  saveRDS(bad, path)
  expect_error(
    read_tibble_rle(path),
    class = "tibble_error_rle_unknown_version"
  )

  bad <- good
  bad$nrow <- -1L
  saveRDS(bad, path)
  expect_error(read_tibble_rle(path), class = "tibble_error_rle_bad_nrow")

  bad <- good
  bad$columns$a$lengths <- c(0L, 3L)
  saveRDS(bad, path)
  expect_error(read_tibble_rle(path), class = "tibble_error_rle_bad_lengths")

  bad <- good
  bad$columns$a$lengths <- c(-1L, 4L)
  saveRDS(bad, path)
  expect_error(read_tibble_rle(path), class = "tibble_error_rle_bad_lengths")

  bad <- good
  bad$columns$a$lengths <- c(1L, NA)
  saveRDS(bad, path)
  expect_error(read_tibble_rle(path), class = "tibble_error_rle_bad_lengths")

  bad <- good
  bad$columns$a$lengths <- c(1L)
  saveRDS(bad, path)
  expect_error(read_tibble_rle(path), class = "tibble_error_rle_bad_lengths")

  bad <- good
  bad$columns$a$lengths <- c(1.5, 1.5)
  saveRDS(bad, path)
  expect_error(read_tibble_rle(path), class = "tibble_error_rle_bad_lengths")

  bad <- good
  bad$columns$a$lengths <- c(2L, 2L)
  saveRDS(bad, path)
  expect_error(read_tibble_rle(path), class = "tibble_error_rle_nrow_mismatch")

  bad <- good
  bad$columns$a <- list(values = 1L)
  saveRDS(bad, path)
  expect_error(read_tibble_rle(path), class = "tibble_error_rle_bad_column")

  # Integerish numeric lengths are accepted and coerced.
  ok <- good
  ok$columns$a$lengths <- c(1, 2)
  saveRDS(ok, path)
  expect_no_error(read_tibble_rle(path))
})

test_that("validation rejects bad arguments", {
  tbl <- tibble(a = 1:3)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  write_tibble_rle(tbl, path)

  expect_error(
    read_tibble_rle(path, filter = list(zzz = 1L)),
    class = "tibble_error_rle_unknown_filter_column"
  )
  expect_error(
    read_tibble_rle(path, filter = list(a = 1:2)),
    class = "tibble_error_rle_bad_condition"
  )
  expect_error(
    read_tibble_rle(path, filter = list(1L)),
    class = "tibble_error_rle_bad_filter"
  )
  expect_error(
    read_tibble_rle(path, filter = list(a = 1L, a = 2L)),
    class = "tibble_error_rle_duplicate_filter_column"
  )
  expect_error(
    read_tibble_rle(path, offset = -1L),
    class = "tibble_error_rle_bad_scalar"
  )
  expect_error(
    read_tibble_rle(path, offset = 1.5),
    class = "tibble_error_rle_bad_scalar"
  )
  expect_error(
    read_tibble_rle(path, limit = 0L),
    class = "tibble_error_rle_bad_limit"
  )
  expect_error(
    read_tibble_rle(path, limit = -1L),
    class = "tibble_error_rle_bad_limit"
  )
  expect_error(read_tibble_rle(path, columns = "zzz"))
  expect_error(
    read_tibble_rle(path, columns = c("a", "a")),
    class = "tibble_error_rle_duplicate_columns"
  )
  expect_error(
    write_tibble_rle(list(a = 1), path),
    class = "tibble_error_rle_must_be_tibble"
  )
})

test_that("rle_is_na() is a recognizable condition", {
  expect_s3_class(rle_is_na(), "tibble_rle_is_na")
})
