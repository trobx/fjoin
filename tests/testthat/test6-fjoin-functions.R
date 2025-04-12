library(data.table)

# ------------------------------------------------------------------------------
make_A <- function() {
  data.table::data.table(id=as.integer(c(1,2,2,3,NA,NA)))[, c := paste0("I'm row A", .I)]
}
make_B <- function() {
  data.table::data.table(id=as.integer(c(NA,NA,4,4,3,3,2)))[, c := paste0("I'm row B", .I)]
}
A <- make_A()
B <- make_B()

# ------------------------------------------------------------------------------
# fjoin_inner

test_that("fjoin_inner without NA matches", {
  result <-
    fjoin_inner(A, B, on="id")
  compare <-
    dplyr::inner_join(A, B, by = "id", relationship = "many-to-many", na_matches = "never")
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_inner with NA matches", {
  result <-
    fjoin_inner(A, B, on="id", match.na = TRUE)
  compare <-
    dplyr::inner_join(A, B, by = "id", relationship = "many-to-many")
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_inner without NA matches, order right", {
  result <-
    fjoin_inner(A, B, on="id", order="right")
  compare <-
    dplyr::inner_join(A, B, by = "id", relationship = "many-to-many", na_matches = "never") |>
    _[order(c.y, c.x)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# fjoin_left

test_that("fjoin_left without NA matches", {
  result <-
    fjoin_left(A, B, on="id")
  compare <-
    dplyr::left_join(A, B, by = "id", relationship = "many-to-many", na_matches = "never")
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_left with NA matches", {
  result <-
    fjoin_left(A, B, on="id", match.na = TRUE)
  compare <-
    dplyr::left_join(A, B, by = "id", relationship = "many-to-many")
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_left without NA matches, order right", {
  result <-
    fjoin_left(A, B, on="id", order="right")
  compare <-
    dplyr::left_join(A, B, by = "id", relationship = "many-to-many", na_matches = "never") |>
    _[order(c.y, c.x)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# fjoin_right

# NB dplyr always orders left

test_that("fjoin_right without NA matches", {
  result <-
    fjoin_right(A, B, on="id")
  compare <-
    dplyr::right_join(A, B, by = "id", relationship = "many-to-many", na_matches = "never") |>
    _[order(c.y, c.x)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_right with NA matches", {
  result <-
    fjoin_right(A, B, on="id", match.na = TRUE)
  compare <-
    dplyr::right_join(A, B, by = "id", relationship = "many-to-many") |>
    _[order(c.y, c.x)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_right without NA matches, order left", {
  result <-
    fjoin_right(A, B, on="id", order="left")
  compare <-
    dplyr::right_join(A, B, by = "id", relationship = "many-to-many", na_matches = "never") |>
    _[order(c.x, c.y)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# fjoin_full

test_that("fjoin_full without NA matches", {
  result <-
    fjoin_full(A, B, on="id")
  compare <-
    dplyr::full_join(A,
                     B,
                     by = "id",
                     relationship = "many-to-many",
                     na_matches = "never")
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_full with NA matches", {
  result <-
    fjoin_full(A, B, on="id", match.na = TRUE)
  compare <-
    dplyr::full_join(A, B, by = "id", relationship = "many-to-many")
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_full without NA matches, order right", {
  result <-
    fjoin_full(A, B, on="id", order="right")
  compare <-
    dplyr::full_join(A, B, by = "id", relationship = "many-to-many", na_matches = "never") |>
    _[order(c.y, c.x)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# fjoin_cross

test_that("fjoin_cross", {
  result <-
    fjoin_cross(A, B)
  compare <-
    dplyr::cross_join(A, B)
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

test_that("fjoin_cross, order right", {
  result <-
    fjoin_cross(A, B, order="right")
  compare <-
    dplyr::cross_join(A, B) |>
    _[order(c.y, c.x)]
  print(result)
  print(compare)
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# fjoin_left_semi

test_that("fjoin_left_semi, one equality without NA matches", {
  result <-
    fjoin_left_semi(A, B, on="id")
  compare <-
    dplyr::semi_join(A, B, by="id", na_matches="never")
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
})

test_that("fjoin_left_semi, one equality with NA matches", {
  result <-
    fjoin_left_semi(A, B, on="id", match.na=TRUE)
  compare <-
    dplyr::semi_join(A, B, by="id")
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
})

test_that("fjoin_left_semi, general case without NA matches", {
  A[, id2 := id]
  B[, id2 := id]
  result <-
    fjoin_left_semi(A, B, on=c("id","id2"))
  compare <-
    dplyr::semi_join(A, B, by=c("id","id2"), na_matches="never")
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
  A[, id2 := NULL]
  B[, id2 := NULL]
})

test_that("fjoin_left_semi, general case with NA matches", {
  A[, id2 := id]
  B[, id2 := id]
  result <-
    fjoin_left_semi(A, B, on=c("id","id2"), match.na=TRUE)
  compare <-
    dplyr::semi_join(A, B, by=c("id","id2"))
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
  A[, id2 := NULL]
  B[, id2 := NULL]
})

# ------------------------------------------------------------------------------
# fjoin_right_semi

test_that("fjoin_right_semi, one equality without NA matches", {
  result <-
    fjoin_right_semi(A, B, on="id")
  compare <-
    dplyr::semi_join(B, A, by="id", na_matches="never")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("fjoin_right_semi, one equality with NA matches", {
  result <-
    fjoin_right_semi(A, B, on="id", match.na=TRUE)
  compare <-
    dplyr::semi_join(B, A, by="id")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("fjoin_right_semi, general case without NA matches", {
  A[, id2 := id]
  B[, id2 := id]
  result <-
    fjoin_right_semi(A, B, on=c("id","id2"))
  compare <-
    dplyr::semi_join(B, A, by=c("id","id2"), na_matches="never")
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
  A[, id2 := NULL]
  B[, id2 := NULL]
})

test_that("fjoin_right_semi, general case with NA matches", {
  A[, id2 := id]
  B[, id2 := id]
  result <-
    fjoin_right_semi(A, B, on=c("id","id2"), match.na=TRUE)
  compare <-
    dplyr::semi_join(B, A, by=c("id","id2"))
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
  A[, id2 := NULL]
  B[, id2 := NULL]
})

# ------------------------------------------------------------------------------
# fjoin_left_anti

test_that("fjoin_left_anti, one equality without NA matches", {
  result <-
    fjoin_left_anti(A, B, on="id")
  compare <-
    dplyr::anti_join(A, B, by="id", na_matches="never")
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
})

test_that("fjoin_left_anti, one equality with NA matches", {
  result <-
    fjoin_left_anti(A, B, on="id", match.na=TRUE)
  compare <-
    dplyr::anti_join(A, B, by="id")
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
})

test_that("fjoin_left_anti, general case without NA matches", {
  A[, id2 := id]
  B[, id2 := id]
  result <-
    fjoin_left_anti(A, B, on=c("id","id2"))
  compare <-
    dplyr::anti_join(A, B, by=c("id","id2"), na_matches="never")
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
  A[, id2 := NULL]
  B[, id2 := NULL]
})

test_that("fjoin_left_anti, general case with NA matches", {
  A[, id2 := id]
  B[, id2 := id]
  result <-
    fjoin_left_anti(A, B, on=c("id","id2"), match.na=TRUE)
  compare <-
    dplyr::anti_join(A, B, by=c("id","id2"))
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
  A[, id2 := NULL]
  B[, id2 := NULL]
})

# ------------------------------------------------------------------------------
# fjoin_right_anti

test_that("fjoin_right_anti, one equality without NA matches", {
  result <-
    fjoin_right_anti(A, B, on="id")
  compare <-
    dplyr::anti_join(B, A, by="id", na_matches="never")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("fjoin_right_anti, one equality with NA matches", {
  result <-
    fjoin_right_anti(A, B, on="id", match.na=TRUE)
  compare <-
    dplyr::anti_join(B, A, by="id")
  print(result)
  print(compare)
  expect_identical(result, compare)
})

test_that("fjoin_right_anti, general case without NA matches", {
  A[, id2 := id]
  B[, id2 := id]
  result <-
    fjoin_right_anti(A, B, on=c("id","id2"))
  compare <-
    dplyr::anti_join(B, A, by=c("id","id2"), na_matches="never")
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
  A[, id2 := NULL]
  B[, id2 := NULL]
})

test_that("fjoin_right_anti, general case with NA matches", {
  A[, id2 := id]
  B[, id2 := id]
  result <-
    fjoin_right_anti(A, B, on=c("id","id2"), match.na=TRUE)
  compare <-
    dplyr::anti_join(B, A, by=c("id","id2"))
  print(result)
  print(compare)
  expect_identical(result, compare, ignore_attr=TRUE)
  A[, id2 := NULL]
  B[, id2 := NULL]
})


