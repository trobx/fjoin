
# ------------------------------------------------------------------------------
# dtjoin cases
# ------------------------------------------------------------------------------

# both inputs data.frames
# (after, add reverse order with select - 1 per broad case)
# (then, full)

# ------------------------------------------------------------------------------
# All cases and subcases
# plain data.frames (will test other objects separately)

# (1) no mult.DT

desc <- "dtjoin 1 inner"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch = NULL)
  compare <-
    dplyr::inner_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never") |>
    dplyr::select(id_A, t_B, c.y, v_B, t_A, c.x, v_A)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "dtjoin 1 inner mult=\"first\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch = NULL, mult = "first")
  compare <-
    dplyr::inner_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never", multiple = "first") |>
    dplyr::select(id_A, t_B, c.y, v_B, t_A, c.x, v_A)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "dtjoin 1 inner mult=\"last\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch = NULL, mult = "last")
  compare <-
    dplyr::inner_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never", multiple = "last") |>
    dplyr::select(id_A, t_B, c.y, v_B, t_A, c.x, v_A)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})
# ------------------------------------------------------------------------------
# (2) mult.DT, no mult

desc <- "dtjoin 2 inner mult.DT=\"first\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult.DT="first")
  compare <-
    dplyr::inner_join(DF_B, DF_A, by=dplyr::join_by(id_B == id_A, t_B < t_A), relationship="many-to-many", na_matches="never", multiple="first") |>
    dplyr::arrange(c.y, c.x)
  if (PRINT_TEST_OBJECTS) {
    print(result)
    print(compare)
  }
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "dtjoin 2 inner mult.DT=\"last\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult.DT="last")
  compare <-
    dplyr::inner_join(DF_B, DF_A, by=dplyr::join_by(id_B == id_A, t_B < t_A), relationship="many-to-many", na_matches="never", multiple="last") |>
    dplyr::arrange(c.y, c.x)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# (3) mult.DT and mult, nomatch = NULL

desc <- "dtjoin 3 inner mult=\"first\" mult.DT=\"last\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="first", mult.DT="last")
  compare <-
    dplyr::inner_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship="many-to-many", na_matches="never", multiple="first") |>
    dplyr::select(id_A, t_B, c.y, v_B, t_A, c.x, v_A) |>
    dplyr::filter(!duplicated(c.y, fromLast=TRUE))
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "dtjoin 3 inner mult=\"last\" mult.DT=\"first\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="last", mult.DT="first")
  compare <-
    dplyr::inner_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship="many-to-many", na_matches="never", multiple="last") |>
    dplyr::select(id_A, t_B, c.y, v_B, t_A, c.x, v_A) |>
    dplyr::filter(!duplicated(c.y))
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# (4) mult.DT and mult, nomatch = NA

desc <- "dtjoin 4 non-inner mult=\"first\" mult.DT=\"last\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), mult="first", mult.DT="last")
  compare <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="first", mult.DT="last") |>
    data.table::as.data.table() |>
    _[DF_A, on=.(i.c==c), .(id_B, t_B, c, v_B, t_A=i.t_A, i.c, v_A = data.table::fifelse(is.na(v_A),i.v_A,v_A))] |>
    as.data.frame()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "dtjoin 4 non-inner mult=\"last\" mult.DT=\"first\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), mult="last", mult.DT="first")
  compare <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch=NULL, mult="last", mult.DT="first") |>
    data.table::as.data.table() |>
    _[DF_A, on=.(i.c==c), .(id_B, t_B, c, v_B, t_A=i.t_A, i.c, v_A = data.table::fifelse(is.na(v_A),i.v_A,v_A))] |>
    as.data.frame()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# Just one test per case
# Reverse the join to test na.omit mechanics
# Test select for each case (will test select.DT, select.i args separately)

# 1 reverse and select
desc <- "dtjoin 1 inner (reverse, select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), nomatch = NULL, select = "c")
  compare <-
    dplyr::inner_join(DF_B, DF_A, by=dplyr::join_by(id_B == id_A, t_B < t_A), relationship = "many-to-many", na_matches = "never") |>
    dplyr::select(id_B, t_A, t_B, c.y, c.x)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# 2 reverse and select
desc <- "dtjoin 2 inner mult.DT=\"first\" (reverse, select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), nomatch = NULL, mult = "first", select = "c")
  compare <-
    dplyr::inner_join(DF_B, DF_A, by=dplyr::join_by(id_B == id_A, t_B < t_A), relationship = "many-to-many", na_matches = "never", multiple = "first") |>
    dplyr::select(id_B, t_A, t_B, c.y, c.x)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# 3 reverse and select
desc <- "dtjoin 3 inner mult=\"first\" mult.DT=\"last\" (reverse, select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), nomatch = NULL, mult = "first", mult.DT="last", select = "c")
  compare <-
    dplyr::inner_join(DF_B, DF_A, by=dplyr::join_by(id_B == id_A, t_B < t_A), relationship = "many-to-many", na_matches = "never", multiple = "first") |>
    dplyr::select(id_B, t_A, t_B, c.y, c.x) |>
    dplyr::filter(!duplicated(c.y, fromLast=TRUE))
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# 4 reverse and select
desc <- "dtjoin 4 non-inner mult=\"first\" mult.DT=\"last\" (reverse, select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), mult = "first", mult.DT="last", select = "c")
  compare <-
    dtjoin(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), nomatch = NULL, mult = "first", mult.DT="last", select = "c") |>
    data.table::as.data.table() |>
    _[DF_B, on=.(i.c==c), .(id_A, t_A, t_B=i.t_B, c, i.c)] |>
    as.data.frame()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ------------------------------------------------------------------------------
# 1 full outer
desc <- "dtjoin 1 full"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch = NA, nomatch.DT = NA, indicate = TRUE)
  compare <-
    dplyr::full_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never") |>
    dplyr::select(id_A, t_B, c.y, v_B, t_A, c.x, v_A)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result[, -1], compare, check.attributes = FALSE))
})

# 2 full outer
desc <- "dtjoin 2 full mult.DT=\"first\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch = NA, nomatch.DT = NA, indicate = TRUE, mult.DT="first")
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), "data.frame")
})

# 3 full outer does not apply
# 4 full outer
desc <- "dtjoin 4 full mult=\"first\" mult.DT=\"last\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), nomatch = NA, nomatch.DT = NA, indicate = TRUE, mult="first", mult.DT="last")
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), "data.frame")
})
