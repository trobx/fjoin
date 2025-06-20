
# (1a) no mult, single equality (na.omit on .DT)
desc <- "dtjoin_semi single equality (na.omit on .DT)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_A,DF_B,on="t_A==t_B",show=SHOW)
  compare <-
    dplyr::semi_join(DF_A,DF_B,by=dplyr::join_by(t_A==t_B),na_matches="never")
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

# (1b) no mult, single equality (na.omit on .i)
desc <- "dtjoin_semi single equality (na.omit on .i)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DF_A,on="t_B==t_A",show=SHOW)
  compare <-
    dplyr::semi_join(DF_B,DF_A,by=dplyr::join_by(t_B==t_A),na_matches="never")
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

# for remaining cases
on <- c("id_A==id_B", "t_A>t_B")

# (2) no mult, general case
desc <- "dtjoin_semi general"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_A,DF_B,on=on,show=SHOW)
  compare <-
    dtjoin(DF_A,DF_B,on=on,indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==3, unique(.SD), .SDcols=c(".join", names(DF_A))][order(c)][, .join:=NULL] |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

# (3) mult
desc <- "dtjoin_semi mult = \"first\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_A,DF_B,on=on,mult="first",show=SHOW)
  compare <-
    dtjoin(DF_A,DF_B,on=on,mult="first",indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==3, unique(.SD), .SDcols=c(".join", names(DF_A))][order(c)][, .join:=NULL] |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})
desc <- "dtjoin_semi mult=\"first\" mult.DT=\"last\""
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_A,DF_B,on=on,mult="first",mult.DT="last",show=SHOW)
  compare <-
    dtjoin(DF_A,DF_B,on=on,mult="first",mult.DT="last",indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==3, unique(.SD), .SDcols=c(".join", names(DF_A))][order(c)][, .join:=NULL] |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

# ------------------------------------------------------------------------------
# (1a) select
desc <- "dtjoin_semi single equality (na.omit on .DT) (select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_A,DF_B,on="t_A==t_B",select="c",show=SHOW)
  compare <-
    dplyr::semi_join(DF_A,DF_B,by=dplyr::join_by(t_A==t_B),na_matches="never") |> dplyr::select(t_A, c)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

# (1b) select
desc <- "dtjoin_semi single equality (na.omit on .i) (select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DF_A,on="t_B==t_A",select="c",show=SHOW)
  compare <-
    dplyr::semi_join(DF_B,DF_A,by=dplyr::join_by(t_B==t_A),na_matches="never") |> dplyr::select(t_B, c)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

# for remaining cases
on <- c("id_B==id_A", "t_B<t_A")

# (2) reverse and select
desc <- "dtjoin_semi general (reverse, select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B,DF_A,on=on,select="c",show=SHOW)
  compare <-
    dtjoin(DF_B,DF_A,on=on,select="c",indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==3, unique(.SD), .SDcols=c(".join", c("id_B", "t_B", "c"))][order(c)][, .join:=NULL] |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

# (3) reverse and select
desc <- "dtjoin_semi mult = \"first\" (reverse, select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B,DF_A,on=on,mult="first",select="c",show=SHOW)
  compare <-
    dtjoin(DF_B,DF_A,on=on,mult="first",select="c",indicate=TRUE) |>
    data.table::setDT() |>
    _[.join==3, unique(.SD), .SDcols=c(".join", c("id_B", "t_B", "c"))][order(c)][, .join:=NULL] |>
    data.table::setDF()
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

