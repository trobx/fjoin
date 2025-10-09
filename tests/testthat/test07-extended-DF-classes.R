# tibble/DT
# tibble/DF
# SF/DT
# SF/tibble
# SF-tibble/DT

# ______________________________________________________________________________
# extended data.frame output classes (but not other extended attributes)
DF_class     <- "data.frame"
DT_class     <- c("data.table", DF_class)
TBL_class    <- c("tbl_df", "tbl", DF_class)
GTBL_class   <- c("grouped_df", TBL_class)
SF_class     <- c("sf", DF_class)
SFTBL_class  <- c("sf", TBL_class)
SFGTBL_class <- c("sf", GTBL_class)

# Basic DF vs DT already checked including with dtjoin_* i.class
# Check the extended (non-DT) classes (use fjoin_*)
# Other attributes tested elsewhere

# fjoin true joins
test_that("fjoin_full tibble output class", {
  result <- fjoin_full(TBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), TBL_class)
})
test_that("fjoin_full grouped tibble output class", {
  result <- fjoin_full(GTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), GTBL_class)
})
test_that("fjoin_full sf output class", {
  result <- fjoin_full(SF_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SF_class)
})
test_that("fjoin_full sf-tibble output class", {
  result <- fjoin_full(SFTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SFTBL_class)
})
test_that("fjoin_full grouped sf-tibble output class", {
  result <- fjoin_full(SFGTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SFGTBL_class)
})

# fjoin_semi
test_that("fjoin_semi tibble output class", {
  result <- fjoin_semi(TBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), TBL_class)
})
test_that("fjoin_semi grouped tibble output class", {
  result <- fjoin_semi(GTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), GTBL_class)
})
test_that("fjoin_semi sf output class", {
  result <- fjoin_semi(SF_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SF_class)
})
test_that("fjoin_semi sf-tibble output class", {
  result <- fjoin_semi(SFTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SFTBL_class)
})
test_that("fjoin_semi grouped sf-tibble output class", {
  result <- fjoin_semi(SFGTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SFGTBL_class)
})

# fjoin_right_semi
test_that("fjoin_right_semi tibble output class", {
  result <- fjoin_right_semi(DT_B, TBL_A, on="id_B == id_A")
  expect_identical(class(result), TBL_class)
})

# fjoin_anti
test_that("fjoin_anti tibble output class", {
  result <- fjoin_anti(TBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), TBL_class)
})
test_that("fjoin_anti grouped tibble output class", {
  result <- fjoin_anti(GTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), GTBL_class)
})
test_that("fjoin_anti sf output class", {
  result <- fjoin_anti(SF_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SF_class)
})
test_that("fjoin_anti sf-tibble output class", {
  result <- fjoin_anti(SFTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SFTBL_class)
})
test_that("fjoin_anti grouped sf-tibble output class", {
  result <- fjoin_anti(SFGTBL_A, DT_B, on="id_A == id_B")
  expect_identical(class(result), SFGTBL_class)
})

# fjoin_right_anti
test_that("fjoin_right_anti tibble output class", {
  result <- fjoin_right_anti(DT_B, TBL_A, on="id_B == id_A")
  expect_identical(class(result), TBL_class)
})

# fjoin_cross
test_that("fjoin_cross tibble output class", {
  result <- fjoin_cross(TBL_A, DT_B)
  expect_identical(class(result), TBL_class)
})
test_that("fjoin_cross grouped tibble output class", {
  result <- fjoin_cross(GTBL_A, DT_B)
  expect_identical(class(result), GTBL_class)
})
test_that("fjoin_cross sf output class", {
  result <- fjoin_cross(SF_A, DT_B)
  expect_identical(class(result), SF_class)
})
test_that("fjoin_cross sf-tibble output class", {
  result <- fjoin_cross(SFTBL_A, DT_B)
  expect_identical(class(result), SFTBL_class)
})
test_that("fjoin_cross grouped sf-tibble output class", {
  result <- fjoin_cross(SFGTBL_A, DT_B)
  expect_identical(class(result), SFGTBL_class)
})

