# argument checks
test_that("dots check", {
  dtjoin(on="id", foo=TRUE) |>
    expect_error("^Invalid argument name\\(s\\)")
})

test_that("true/false arg check", {
  dtjoin(on="id", match.na=TRUE) |>
    expect_no_error()
  dtjoin(on="id", match.na=NA) |>
    expect_error("Argument 'match.na' must be TRUE or FALSE")
})

test_that("order arg check", {
  fjoin_left(DF_A, DF_B, on="id_A==id_B", order="left") |>
    expect_no_error()
  fjoin_left(DF_A, DF_B, on="id_A==id_B", order="right") |>
    expect_no_error()
  fjoin_left(DF_A, DF_B, on="id_A==id_B", order=TRUE) |>
    expect_error("Argument 'order' must be \"left\" or \"right\"")
})

test_that("on arg check", {
  fjoin_left(DF_A, DF_B, on=1L) |>
    expect_error("Argument 'on' must be a non-empty character vector with no empty strings or NAs")
  fjoin_left(DF_A, DF_B, on=TRUE) |>
    expect_error("Argument 'on' must be a non-empty character vector with no empty strings or NAs")
  fjoin_left(DF_A, DF_B, on=character(0)) |>
    expect_error("Argument 'on' must be a non-empty character vector with no empty strings or NAs")
})

test_that("prefix arg check", {
  fjoin_left(DF_A, DF_B, on="id_A==id_B", prefix="i.") |>
    expect_no_error()
  fjoin_left(DF_A, DF_B, on="id_A==id_B", prefix="i. ") |>
    expect_error("^Argument 'prefix' must be")
  fjoin_left(DF_A, DF_B, on="id_A==id_B", prefix=c("i.","i.")) |>
    expect_error("^Argument 'prefix' must be")
  fjoin_left(DF_A, DF_B, on="id_A==id_B", prefix=1L) |>
    expect_error("^Argument 'prefix' must be")
  fjoin_left(DF_A, DF_B, on="id_A==id_B", prefix=NA_character_) |>
    expect_error("^Argument 'prefix' must be")
  fjoin_left(DF_A, DF_B, on="id_A==id_B", prefix=NA) |>
    expect_error("^Argument 'prefix' must be")
  fjoin_left(DF_A, DF_B, on="id_A==id_B", prefix=NULL) |>
    expect_error("^Argument 'prefix' must be")
})

test_that("select arg check", {
  fjoin_left(DF_A, DF_B, on="id_A==id_B", select="") |>
    expect_no_error()
  fjoin_left(DF_A, DF_B, on="id_A==id_B", select=NULL) |>
    expect_no_error()
  fjoin_left(DF_A, DF_B, on="id_A==id_B", select=NA) |>
    expect_no_error()
  fjoin_left(DF_A, DF_B, on="id_A==id_B", select=c(1L)) |>
    expect_error("Argument 'select' must be a character vector, NA, or NULL")
  fjoin_left(DF_A, DF_B, on="id_A==id_B", select=c(NA,NA)) |>
    expect_error("Argument 'select' must be a character vector, NA, or NULL")
})

test_that("mult arg check", {
  dtjoin(on="id", mult="all") |>
    expect_no_error()
  dtjoin(on="id", mult="foo") |>
    expect_error("Argument 'mult' must be \"all\", \"first\", or \"last\"")
})

test_that("nomatch arg check", {
  dtjoin(on="id", nomatch=NULL) |>
    expect_no_error()
  dtjoin(on="id", nomatch=NA) |>
    expect_no_error()
  dtjoin(on="id", nomatch=0L) |>
    expect_no_error()
  dtjoin(on="id", nomatch=FALSE) |>
    expect_no_error()
  dtjoin(on="id", nomatch=TRUE) |>
    expect_error("Argument 'nomatch' must be NA, NULL, or 0L")
})

test_that("input class check", {
  dtjoin(data.frame(id=1L), list(id=1L), on="id") |>
    expect_no_error()
  dtjoin(data.frame(id=1L), data.table::setattr(list(id=1L),"class","foo"), on="id") |>
    expect_error("Argument '.i' must be a data.frame-like object or list")
  dtjoin(data.frame(id=1L), c(1L), on="id") |>
    expect_error("Argument '.i' must be a data.frame-like object or list")
})

# ------------------------------------------------------------------------------
# shallow_DT
test_that("shallow_DT", {
  result <- shallow_DT(x <- data.frame(id=1L))
  expect_identical(class(result), c("data.table", "data.frame"))
  expect_identical(data.table::address(x$id), data.table::address(result$id))
  result <- shallow_DT(x <- list(id=1L))
  expect_identical(class(result), c("data.table", "data.frame"))
  expect_identical(data.table::address(x$id), data.table::address(result$id))
  result <- shallow_DT(x <- data.table::data.table(id=1L))
  expect_identical(class(result), c("data.table", "data.frame"))
  expect_no_warning(result[,foo:=1L])
  result <- shallow_DT(x <- data.table::data.table(id=1L), use_setDT=FALSE)
  expect_identical(data.table::address(x$id), data.table::address(result$id))
  expect_warning(result[,foo:=1L])
})

# ------------------------------------------------------------------------------
# na_omit_heuristics
test_that("na_omit_text", {
  expect_identical(na_omit_cost(DF_A), na_omit_cost_rc(nrow(DF_A), ncol(DF_A)))
})

# ------------------------------------------------------------------------------
# na_omit_text
test_that("na_omit_text", {
  expect_identical(na_omit_text("x"), "na.omit(x)")
  expect_identical(na_omit_text("x", na_cols="id"), "na.omit(x, cols = \"id\")")
  expect_identical(na_omit_text("x", sd_cols=c("id", "v")), "x[, na.omit(.SD), .SDcols = c(\"id\", \"v\")]")
  expect_identical(na_omit_text("x", na_cols="id", sd_cols="v"), "x[, na.omit(.SD, cols = \"id\"), .SDcols = \"v\"]")
  expect_identical(na_omit_text("x", na_cols="id", sd_cols=c("id", "v")), "x[, na.omit(.SD, cols = \"id\"), .SDcols = c(\"id\", \"v\")]")
})

# ------------------------------------------------------------------------------
# subset_while_in
test_that("subset_while_in", {
  y <- letters[1:5]
  expect_null(subset_while_in(character(0),y))
  expect_null(subset_while_in(letters[6:10],y))
  expect_identical(subset_while_in(c("a","b","z"),y), c("a","b"))
  expect_identical(subset_while_in(letters[1:3],y), letters[1:3])
})
# ------------------------------------------------------------------------------
# vcat debug helper
test_that("vcat", {
  x <- c("foo","bar")
  expect_output(vcat(x), "x: foo, bar")
})

