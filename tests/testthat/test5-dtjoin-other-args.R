library(data.table)

A <- data.table(
  idA=c(NA,2:4),
  tA=c(1,2,3,NA),
  colA=1L,
  colC="A"
)

B <- data.table(
  idB=c(3:1,NA),
  tB=c(NA,2,2,2),
  colB=0L,
  colC="B"
)

# ------------------------------------------------------------------------------
# .DT taller than .i

# TODO

# ------------------------------------------------------------------------------
# i.main
# These tests based on names only; tests of fjoin functions cover column values

test_that("dtjoin names with i.main", {
  result <- dtjoin(B, A, on="idB==idA", i.main=TRUE)
  expect_named(result, c("idA","tA","colA","colC","tB","colB","x.colC"))
})

test_that("dtjoin names with i.main and nomatch.DT", {
  result <- dtjoin(B, A, on="idB==idA", nomatch.DT=NA, i.main=TRUE)
  expect_named(result, c("idA","tA","colA","colC","tB","colB","x.colC"))
})

# ------------------------------------------------------------------------------
# on.first

test_that("dtjoin names with on.first", {
  result <- dtjoin(B, A, on="colB==colA", on.first=TRUE)
  expect_named(result, c("colB","idB","tB","colC","idA","tA","i.colC"))
})

test_that("dtjoin names with on.first and i.main", {
  result <- dtjoin(B, A, on="colB==colA", on.first=TRUE, i.main=TRUE)
  expect_named(result, c("colA","idA","tA","colC","idB","tB","x.colC"))
})

# ------------------------------------------------------------------------------
# preserve

test_that("dtjoin names with preserve", {
  result <- dtjoin(B, A, on="idB==idA", preserve=TRUE)
  expect_named(result, c("idB","tB","colB","colC","idA","tA","colA","i.colC"))
})

test_that("dtjoin names with preserve and nomatch.DT", {
  result <- dtjoin(B, A, on="idB==idA", nomatch.DT=NA, preserve=TRUE)
  expect_named(result, c("idB","tB","colB","colC","idA","tA","colA","i.colC"))
})

# ------------------------------------------------------------------------------
# no garbling

test_that("dtjoin names with non-equi", {
  result <- dtjoin(B, A, on="tB>tA")
  expect_named(result, c("idB","tB","colB","colC","idA","tA","colA","i.colC"))
})

# ------------------------------------------------------------------------------
# indicate

A <- A[, .(idA, colC=paste0("A",.I))]
B <- B[, .(idB, colC=paste0("B",.I))]

test_that("fjoin_inner with indicate", {
  result <- fjoin_inner(A, B, on="idA==idB", indicate=TRUE)
  expect_equal(result$.join, c(3,3))
})

test_that("fjoin_left with indicate", {
  result <- fjoin_left(A, B, on="idA==idB", indicate=TRUE)
  expect_equal(result$.join, c(1,3,3,1))
})

test_that("fjoin_left ordered by y with indicate", {
  result <- fjoin_left(A, B, on="idA==idB", order="y", indicate=TRUE)
  expect_equal(result$.join, c(3,3,1,1))
})

test_that("fjoin_right with indicate", {
  result <- fjoin_right(A, B, on="idA==idB", indicate=TRUE)
  expect_equal(result$.join, c(3,3,2,2))
})

test_that("fjoin_right ordered by x with indicate", {
  result <- fjoin_right(A, B, on="idA==idB", order="x", indicate=TRUE)
  expect_equal(result$.join, c(3,3,2,2))
})

test_that("fjoin_full  with indicate", {
  result <- fjoin_full(A, B, on="idA==idB", indicate=TRUE)
  expect_equal(result$.join, c(1,3,3,1,2,2))
})

test_that("fjoin_full ordered by y with indicate", {
  result <- fjoin_full(A, B, on="idA==idB", order="y", indicate=TRUE)
  expect_equal(result$.join, c(3,3,2,2,1,1))
})
