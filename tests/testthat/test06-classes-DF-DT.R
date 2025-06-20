# Test output classes or labels if show

# ______________________________________________________________________________
# labels
desc <- "fjoin_left with do=FALSE, data.frames"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  expect_null(fjoin_left(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_left(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.DT : y = DF_B \\(cast as data\\.table\\)")
  expect_output(fjoin_left(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.i  : x = DF_A \\(cast as data\\.table\\)")
})
desc <- "fjoin_semi with do=FALSE, data.frames"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  expect_null(fjoin_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                 "\\.DT : x = DF_A \\(cast as data\\.table\\)")
  expect_output(fjoin_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                 "\\.i  : y = DF_B \\(cast as data\\.table\\)")
})
desc <- "fjoin_anti with do=FALSE, data.frames"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  expect_null(fjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.DT : x = DF_A \\(cast as data\\.table\\)")
  expect_output(fjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.i  : y = DF_B \\(cast as data\\.table\\)")
})
desc <- "fjoin_right_semi with do=FALSE, data.frames"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  expect_null(fjoin_right_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_right_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.DT : y = DF_B \\(cast as data\\.table\\)")
  expect_output(fjoin_right_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.i  : x = DF_A \\(cast as data\\.table\\)")
})
desc <- "fjoin_right_anti with do=FALSE, data.frames"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  expect_null(fjoin_right_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_right_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.DT : y = DF_B \\(cast as data\\.table\\)")
  expect_output(fjoin_right_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.i  : x = DF_A \\(cast as data\\.table\\)")
})
desc <- "fjoin_cross with do=FALSE, data.frames"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  expect_null(fjoin_cross(DF_A, DF_B, do=FALSE))
  expect_output(fjoin_cross(DF_A, DF_B, do=FALSE),
                "\\.DT : y = DF_B \\(cast as data\\.table\\)")
  expect_output(fjoin_cross(DF_A, DF_B, do=FALSE),
                "\\.i  : x = DF_A \\(cast as data\\.table\\)")
})

# ______________________________________________________________________________
# data.frame or data.table for all cases

# dtjoin
desc <- "dtjoin (1) data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A"), show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin (1) data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DT_A, on=c("id_B == id_A"), show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin (2) data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A"), mult.DT="last", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin (2) data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult.DT="last", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin (3) data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A"), mult="first", mult.DT="last", nomatch=NULL, show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin (3) data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult="first", mult.DT="last", nomatch=NULL, show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin (4) data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DF_A, on=c("id_B == id_A"), mult="first", mult.DT="last", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin (4) data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult="first", mult.DT="last", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})

# dtjoin_anti
desc <- "dtjoin_anti data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_anti(DF_B, DT_A, on=c("id_B == id_A"), show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_anti data.frame output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_anti(DF_B, DT_A, on=c("id_B == id_A"), select="c", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_anti data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A"), show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin_anti data.table output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A"), select="c", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})

# dtjoin_semi
desc <- "dtjoin_semi (1a) data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DT_A, on=c("id_B == id_A"), show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_semi (1a) data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A"), show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin_semi (1a) data.frame output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DT_A, on=c("id_B == id_A"), select="c", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_semi (1a) data.table output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A"), select="c", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin_semi (1b) data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DT_A, on="t_B==t_A", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_semi (1b) data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DT_B, DF_A, on="t_B==t_A", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin_semi (1b) data.frame output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DT_A, on="t_B==t_A", select="c", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_semi (1b) data.table output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DT_B, DF_A, on="t_B==t_A", select="c", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin_semi (2) data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DT_A, on=c("id_B==id_A", "t_B<t_A"), show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_semi (2) data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DT_B, DF_A, on=c("id_B==id_A", "t_B<t_A"), show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin_semi (2) data.frame output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DT_A, on=c("id_B==id_A", "t_B<t_A"), select="c", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_semi (2) data.table output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_anti(DT_B, DF_A, on=c("id_B==id_A", "t_B<t_A"), select="c", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin_semi (3) data.frame output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DT_A, on=c("id_B==id_A", "t_B<t_A"), mult="first", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_semi (3) data.table output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DT_B, DF_A, on=c("id_B==id_A", "t_B<t_A"), mult="first", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})
desc <- "dtjoin_semi (3) data.frame output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DF_B, DT_A, on=c("id_B==id_A", "t_B<t_A"), select="c", mult="first", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
})
desc <- "dtjoin_semi (3) data.table output with select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_semi(DT_B, DF_A, on=c("id_B==id_A", "t_B<t_A"), select="c", mult="first", show=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.table", "data.frame"))
})




