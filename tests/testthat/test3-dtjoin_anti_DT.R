library(data.table)

# TODO: get round the fact the we need data.table loaded for the tests - maybe in testthat.R?
# TODO: see https://testthat.r-lib.org/articles/third-edition.html to set 3e for all tests

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

test_that("dtjoin_anti_DT single equality", {
  result <-
    dtjoin_anti_DT(A, B, on=c("id_A == id_B"))
  compare <-
    dplyr::anti_join(A, B, by=dplyr::join_by(id_A == id_B), na_matches = "never")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_anti_DT multiple equalities", {
  result <-
    dtjoin_anti_DT(A, B, on=c("id_A == id_B", "t_A == t_B"))
  compare <-
    dplyr::anti_join(A, B, by=dplyr::join_by(id_A == id_B, t_A == t_B), na_matches = "never")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_anti_DT non-equality", {
  result <-
    dtjoin_anti_DT(A, B, on=c("id_A == id_B", "t_A > t_B"))
  compare <-
    dplyr::anti_join(A, B, by=dplyr::join_by(id_A == id_B, t_A > t_B), na_matches = "never")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_anti_DT non-equality with mult=\"first\"", {
  result <-
    dtjoin_anti_DT(A, B, on=c("id_A == id_B", "t_A > t_B"), mult="first")
  compare <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult.DT = "first", indicate = TRUE, i.main = TRUE) |>
      _[.join == 1L, mget(names(A))]
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("dtjoin_anti_DT non-equality with mult.DT=\"last\"", {
  result <-
    dtjoin_anti_DT(A, B, on=c("id_A == id_B", "t_A > t_B"), mult.DT="last")
  compare <-
    dtjoin(B, A, on=c("id_B == id_A", "t_B < t_A"), mult = "last", indicate = TRUE, i.main = TRUE) |>
    _[.join == 1L, .(id_A, t_A, c)]
  print(result)
  print(compare)
  expect_identical(result, compare)
})
