# ------------------------------------------------------------------------------
# on.first

test_that("dtjoin names with on.first", {
  result <- dtjoin(DF_B, DF_A, on="v_B==v_A", on.first=TRUE)
  expect_named(result, c("v_B","id_B","t_B","c","id_A","t_A","i.c"))
})

test_that("dtjoin names with on.first and i.home", {
  result <- dtjoin(DF_B, DF_A, on="v_B==v_A", on.first=TRUE, i.home=TRUE)
  expect_named(result, c("v_A","id_A","t_A","c","id_B","t_B","x.c"))
})

# ------------------------------------------------------------------------------
# both

test_that("dtjoin names with both", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", both=TRUE)
  expect_named(result, c("id_B","t_B","c","v_B","id_A","t_A","i.c","v_A"))
})

test_that("dtjoin names with both and nomatch.DT", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", nomatch.DT=NA, both=TRUE)
  expect_named(result, c("id_B","t_B","c","v_B","id_A","t_A","i.c","v_A"))
})

# ------------------------------------------------------------------------------
# no garbling

test_that("dtjoin names with non-equi, cases 1 and 3", {
  result <- dtjoin(DF_B, DF_A, on="t_B>t_A")
  expect_named(result, c("id_B","t_B","c","v_B","id_A","t_A","i.c","v_A"))
})

test_that("dtjoin names with non-equi, cases 2 and 4", {
  result <- dtjoin(DF_B, DF_A, on="t_B>t_A", mult.DT="first")
  expect_named(result, c("id_B","t_B","c","v_B","id_A","t_A","i.c","v_A"))
})

# ------------------------------------------------------------------------------
# indicate

test_that("fjoin_inner with indicate", {
  result <- fjoin_inner(DF_A, DF_B, on="id_A==id_B", indicate=TRUE)
  expect_equal(result$.join, ifelse(is.na(result$c),2,ifelse(is.na(result$R.c),1,3)))
})

test_that("fjoin_left with indicate", {
  result <- fjoin_left(DF_A, DF_B, on="id_A==id_B", indicate=TRUE)
  expect_equal(result$.join, ifelse(is.na(result$c),2,ifelse(is.na(result$R.c),1,3)))
})

test_that("fjoin_left ordered by y with indicate", {
  result <- fjoin_left(DF_A, DF_B, on="id_A==id_B", order="right", indicate=TRUE)
  expect_equal(result$.join, ifelse(is.na(result$c),2,ifelse(is.na(result$R.c),1,3)))
})

test_that("fjoin_right with indicate", {
  result <- fjoin_right(DF_A, DF_B, on="id_A==id_B", indicate=TRUE)
  expect_equal(result$.join, ifelse(is.na(result$c),2,ifelse(is.na(result$R.c),1,3)))
})

test_that("fjoin_right ordered by x with indicate", {
  result <- fjoin_right(DF_A, DF_B, on="id_A==id_B", order="left", indicate=TRUE)
  expect_equal(result$.join, ifelse(is.na(result$c),2,ifelse(is.na(result$R.c),1,3)))
})

test_that("fjoin_full  with indicate", {
  result <- fjoin_full(DF_A, DF_B, on="id_A==id_B", indicate=TRUE)
  expect_equal(result$.join, ifelse(is.na(result$c),2,ifelse(is.na(result$R.c),1,3)))
})

test_that("fjoin_full ordered by y with indicate", {
  result <- fjoin_full(DF_A, DF_B, on="id_A==id_B", order="right", indicate=TRUE)
  expect_equal(result$.join, ifelse(is.na(result$c),2,ifelse(is.na(result$R.c),1,3)))
})
