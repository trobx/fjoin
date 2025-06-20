# Test output classes or labels if show

# tibble/DT
# tibble/DF
# SF/DT
# SF/tibble
# SF-tibble/DT

# TODO add check with object that is DT and df - recast to df

# Just check output class - use different test file to test sf content

# fjoin true joins

desc <- "fjoin_full tibble and data.frame"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_full(tibble::as_tibble(DF_A), DF_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
})

desc <- "fjoin_full tibble and data.table"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_full(tibble::as_tibble(DF_A), DT_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("data.table", "data.frame"))
})

desc <- "fjoin_full sf and data.frame"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_full(SF_A, DF_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("sf", "data.frame"))
})

desc <- "fjoin_full sf and data.table"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_full(SF_A, DT_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("sf", "data.frame"))
})

desc <- "fjoin_full sf and tibble"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_full(SF_A, tibble::as_tibble(DF_B), on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
   expect_identical(class(result), c("sf", "tbl_df", "tbl", "data.frame"))
})

desc <- "fjoin_full sf-tibble and data.table"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_full(sf::st_as_sf(tibble::as_tibble(SF_A)), DT_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
   expect_identical(class(result), c("sf", "tbl_df", "tbl", "data.frame"))
})

# fjoin_semi

desc <- "fjoin_semi tibble and data.frame"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_semi(tibble::as_tibble(DF_A), DF_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
   expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
})

desc <- "fjoin_semi sf and data.frame"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_semi(SF_A, DF_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("sf", "data.frame"))
})

desc <- "fjoin_semi sf-tibble and data.table"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_semi(sf::st_as_sf(tibble::as_tibble(SF_A)), DT_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("sf", "tbl_df", "tbl", "data.frame"))
})

# fjoin_anti

desc <- "fjoin_anti tibble and data.frame"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_anti(tibble::as_tibble(DF_A), DF_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
})

desc <- "fjoin_anti sf and data.frame"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_anti(SF_A, DF_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("sf", "data.frame"))
})

desc <- "fjoin_anti sf-tibble and data.table"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_anti(sf::st_as_sf(tibble::as_tibble(SF_A)), DT_B, on=c("id_A == id_B"))
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("sf", "tbl_df", "tbl", "data.frame"))
})

# fjoin_cross

desc <- "fjoin_cross tibble and data.frame"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_cross(tibble::as_tibble(DF_A), DF_B)
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
})

desc <- "fjoin_cross sf and data.frame"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_cross(SF_A, DF_B)
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("sf", "data.frame"))
})

desc <- "fjoin_cross sf-tibble and data.table"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_cross(sf::st_as_sf(tibble::as_tibble(SF_A)), DT_B)
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(class(result), c("sf", "tbl_df", "tbl", "data.frame"))
})
