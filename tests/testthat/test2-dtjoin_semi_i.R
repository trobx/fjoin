library(data.table)

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

test_that("dtjoin_semi_i single equality", {
  result <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A"))
  compare <-
    dplyr::semi_join(A, B, by=dplyr::join_by(id_A == id_B), na_matches = "never")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_semi_i single equality with mult=\"first\"", {
  result <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A"), mult="first")
  compare <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A"))
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_semi_i general", {
  result <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A", "t_B < t_A"))
  compare <-
    dplyr::semi_join(A, B, by=dplyr::join_by(id_A == id_B, t_A > t_B), na_matches = "never")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_semi_i general with mult=\"first\"", {
  result <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="first")
  compare <-
    # NB dplyr::semi_join does not support 'multiple'
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="first", nomatch=NULL, i.main=TRUE) |>
      _[, .(id_A, t_A, c)]
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_semi_i with mult.DT=\"first\"", {
  result <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A", "t_B < t_A"), mult.DT="first")
  compare <-
    # NB dplyr::semi_join does not support 'multiple'
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult.DT="first", nomatch=NULL, i.main=TRUE) |>
    _[, .(id_A, t_A, c)][!duplicated(c)]
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_semi_i with mult.DT=\"last\"", {
  result <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A", "t_B < t_A"), mult.DT="last")
  compare <-
    # NB dplyr::semi_join does not support 'multiple'
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult.DT="last", nomatch=NULL, i.main=TRUE) |>
    _[, .(id_A, t_A, c)][!duplicated(c)]
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_semi_i with mult=\"first\" and mult.DT=\"last\"", {
  result <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="first", mult.DT="last")
  compare <-
    # NB dplyr::semi_join does not support 'multiple'
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="first", mult.DT="last", nomatch=NULL, i.main=TRUE) |>
    _[, .(id_A, t_A, c)][!duplicated(c)]
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_semi_i with mult=\"last\" and mult.DT=\"first\"", {
  result <-
    dtjoin_semi_i(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="last", mult.DT="first")
  compare <-
    # NB dplyr::semi_join does not support 'multiple'
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult="last", mult.DT="first", nomatch=NULL, i.main=TRUE) |>
    _[, .(id_A, t_A, c)][!duplicated(c)]
  print(result)
  print(compare)
  expect_identical(result, compare)
})
