# test select args
# do in on shot using fjoin funcs

desc <- "fjoin_inner with select"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_inner(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select="c")
  compare <-
    dplyr::inner_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never") |>
    dplyr::select(id_A, t_A, t_B, c.x, c.y)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "fjoin_left with select"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_left(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select="c")
  compare <-
    dplyr::left_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never") |>
    dplyr::select(id_A, t_A, t_B, c.x, c.y)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "fjoin_right with select"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_right(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select="c")
  compare <-
    dplyr::right_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never") |>
    dplyr::select(id_A, t_A, t_B, c.x, c.y)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "fjoin_full with select"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_full(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select="c")
  compare <-
    dplyr::full_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never") |>
    dplyr::select(id_A, t_A, t_B, c.x, c.y)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "fjoin_left_semi with select"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_left_semi(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select="c")
  compare <-
    fjoin_left(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==3, .(id_A, t_A, c)] |>
    unique() |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "fjoin_right_semi with select"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_right_semi(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select="c")
  compare <-
    fjoin_right(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), preserve=TRUE, indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==3, .(id_B, t_B, c=R.c)] |>
    unique() |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "fjoin_left_anti with select"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_left_anti(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select="c")
  compare <-
    fjoin_left(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==1, .(id_A, t_A, c)] |>
    unique() |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "fjoin_right_anti with select"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_right_anti(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select="c")
  compare <-
    fjoin_right(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), preserve=TRUE, indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==2, .(id_B, t_B, c=R.c)] |>
    unique() |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

# ______________________________________________________________________________

desc <- "fjoin_full with select.x and select.y"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_full(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select.x="c", select.y="v_B")
  compare <-
    dplyr::full_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), relationship = "many-to-many", na_matches = "never") |>
    dplyr::select(id_A, t_A, c.x, t_B, v_B)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})

desc <- "fjoin_cross with select.x and select.y"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    fjoin_cross(DF_A, DF_B, select.x="c", select.y="v_B")
  compare <-
    dplyr::cross_join(DF_A, DF_B) |>
    dplyr::select(c.x, v_B)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
})
