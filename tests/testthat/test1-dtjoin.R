library(data.table)

# TODO: get round the fact the we need data.table loaded for the tests - maybe in testthat.R?
# TODO: see https://testthat.r-lib.org/articles/third-edition.html to set 3e for all tests

#library(testthat)
#local_edition(3)
#library(dplyr)

# TODO test script that tests all the frills (no garbling, column order options)

# ------------------------------------------------------------------------------
# Devise an example that inter alia gives different answers with two-way `mult`
# according to the order of operations

n_A <- 7L
n_B <- 4L
set.seed(2)
A <- data.table::data.table(id_A=sample(letters[1:2], n_A, TRUE), t_A=sample(1:5, n_A, TRUE))
B <- data.table::data.table(id_B=sample(letters[1:2], n_B, TRUE), t_B=sample(1:3, n_B, TRUE))
A <- rbind(A, data.table::data.table(id_A=c(NA,NA)), fill=TRUE)
B <- rbind(data.table::data.table(id_B=c(NA,NA)), B, fill=TRUE)
A[, c := paste0("I'm row A", formatC(.I, width = log10(.N) + 1, format = "d", flag = "0"))]
B[, c := paste0("I'm row B", formatC(.I, width = log10(.N) + 1, format = "d", flag = "0"))]
A
B

# check gives different answers
tmp <- B[A, on=.(id_B == id_A, t_B < t_A), nomatch = NULL, .(id_A, id_B=x.id_B, t_A, t_B=x.t_B, c_A=i.c, c_B=c)]
tmp

tmp[, first(.SD), by=c_A][, last(.SD), by=c_B][order(c_A, c_B), .(id_A, id_B, t_A, t_B, c_A, c_B)]
tmp[, last(.SD), by=c_B][, first(.SD), by=c_A][order(c_A, c_B), .(id_A, id_B, t_A, t_B, c_A, c_B)]

# ------------------------------------------------------------------------------
# dtjoin

dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"))
dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="first", indicate=TRUE)
dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult.DT="last", indicate=TRUE)
dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="first", mult.DT="last", indicate=TRUE)
dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, nomatch.DT=NA, mult="first", mult.DT="last", indicate=TRUE)

# ------------------------------------------------------------------------------
test_that("dtjoin inner", {
  result <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL)
  compare <-
    dplyr::inner_join(A, B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never") |>
      dplyr::select(id_A, t_B, c.y, t_A, c.x)
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("dtjoin inner with mult=\"first\"", {
  result <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="first")
  compare <-
    dplyr::inner_join(A, B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship="many-to-many", na_matches="never", multiple="first") |>
    dplyr::select(id_A, t_B, c.y, t_A, c.x)
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("dtjoin inner with mult.DT=\"first\"", {
  result <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult.DT="first")
  compare <-
    dplyr::inner_join(B, A, by=dplyr::join_by(id_B == id_A, t_B < t_A), relationship="many-to-many", na_matches="never", multiple="first") |>
    dplyr::arrange(c.y, c.x)
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("dtjoin inner with mult.DT=\"last\"", {
  result <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult.DT="last")
  compare <-
    dplyr::inner_join(B, A, by=dplyr::join_by(id_B == id_A, t_B < t_A), relationship="many-to-many", na_matches="never", multiple="last") |>
    dplyr::arrange(c.y, c.x)
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("dtjoin inner with mult=\"first\", mult.DT=\"last\"", {
  result <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="first", mult.DT="last")
  compare <-
    dplyr::inner_join(A, B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship="many-to-many", na_matches="never", multiple="first") |>
    dplyr::select(id_A, t_B, c.y, t_A, c.x) |>
    dplyr::filter(!duplicated(c.y, fromLast=TRUE))
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("dtjoin non-inner with mult=\"first\", mult.DT=\"last\"", {
  result <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="first", mult.DT="last")
  compare <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="first", mult.DT="last") |>
    _[A, on=.(i.c==c), .(id_B, t_B, c, t_A=i.t_A, i.c)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("dtjoin inner with mult=\"last\", mult.DT=\"first\"", {
  result <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="last", mult.DT="first")
  compare <-
    dplyr::inner_join(A, B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship="many-to-many", na_matches="never", multiple="last") |>
    dplyr::select(id_A, t_B, c.y, t_A, c.x) |>
    dplyr::filter(!duplicated(c.y))
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("dtjoin non-inner with mult=\"last\", mult.DT=\"first\"", {
  result <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="last", mult.DT="first")
  compare <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="last", mult.DT="first") |>
    _[A, on=.(i.c==c), .(id_B, t_B, c, t_A=i.t_A, i.c)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

