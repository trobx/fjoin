# ------------------------------------------------------------------------------
# names with i.home=TRUE including outer.DT case
# These tests based on names only; tests of fjoin functions cover column values

test_that("dtjoin names with i.home", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", i.home=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","t_B","x.c","v_B"))
})

test_that("dtjoin names with i.home, outer.DT", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", nomatch.DT=NA, i.home=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","t_B","x.c","v_B"))
})

# rename_anti.DT (outer.DT, i.home), same name, preserve both
test_that("dtjoin names with i.home, outer.DT, preserve both", {
  data.table::setnames(DF_A,"id_A","id")
  data.table::setnames(DF_B,"id_B","id")
  result <- dtjoin(DF_B, DF_A, on="id", nomatch.DT=NA, i.home=TRUE, both=TRUE)
  expect_named(result, c("id","t_A","c","v_A","x.id","t_B","x.c","v_B"))
  data.table::setnames(DF_A,"id","id_A")
  data.table::setnames(DF_B,"id","id_B")
})

# ditto, different names
test_that("dtjoin names with i.home", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", nomatch.DT=NA, i.home=TRUE, both=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","id_B","t_B","x.c","v_B"))
})

# ------------------------------------------------------------------------------
# names with dtjoin special case mult.DT but no mult

# special case mult.DT but no mult, same name
test_that("dtjoin mult.DT but no mult, names with same name join col, outer.DT", {
  data.table::setnames(DF_A,"id_A","id")
  data.table::setnames(DF_B,"id_B","id")
  result <- dtjoin(DF_B, DF_A, on="id", mult.DT="first", nomatch.DT=NA)
  expect_named(result, c("id","t_B","c","v_B","t_A","i.c","v_A"))
  data.table::setnames(DF_A,"id","id_A")
  data.table::setnames(DF_B,"id","id_B")
})

# special case mult.DT but no mult, with i.home
test_that("dtjoin mult.DT but no mult, names with i.home, outer.DT, preserve both", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", mult.DT="first", nomatch.DT=NA, i.home=TRUE, both=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","id_B","t_B","x.c","v_B"))
})

# special case mult.DT but no mult, with i.home, same name
test_that("dtjoin mult.DT but no mult, names with i.home, outer.DT", {
  data.table::setnames(DF_A,"id_A","id")
  data.table::setnames(DF_B,"id_B","id")
  result <- dtjoin(DF_B, DF_A, on="id", mult.DT="first", nomatch.DT=NA, i.home=TRUE)
  expect_named(result, c("id","t_A","c","v_A","t_B","x.c","v_B"))
  data.table::setnames(DF_A,"id","id_A")
  data.table::setnames(DF_B,"id","id_B")
})

# special case mult.DT but no mult, with i.home, same name, preserve both
test_that("dtjoin mult.DT but no mult, names with i.home, outer.DT, preserve both", {
  data.table::setnames(DF_A,"id_A","id")
  data.table::setnames(DF_B,"id_B","id")
  result <- dtjoin(DF_B, DF_A, on="id", mult.DT="first", nomatch.DT=NA, i.home=TRUE, both=TRUE)
  expect_named(result, c("id","t_A","c","v_A","x.id","t_B","x.c","v_B"))
  data.table::setnames(DF_A,"id","id_A")
  data.table::setnames(DF_B,"id","id_B")
})

# special case mult.DT but no mult, with i.home, different names
test_that("dtjoin mult.DT but no mult, names with i.home", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", mult.DT="first", nomatch.DT=NA, i.home=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","t_B","x.c","v_B"))
})

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

test_that("dtjoin names with non-equi", {
  result <- dtjoin(DF_B, DF_A, on="t_B>t_A")
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
