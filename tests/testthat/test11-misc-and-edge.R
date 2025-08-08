# ------------------------------------------------------------------------------
# as-is data.table inputs left intact
test_that("as-is data.table inputs left intact", {
  addr_A <- data.table::address(DT_A)
  DT_A_copy <- data.table::copy(DT_A)
  DT_B_copy <- data.table::copy(DT_B)

  dtjoin(DT_A, DT_B, on="id_A == id_B", nomatch.DT=NA, mult.DT="first", indicate=TRUE, show=TRUE)
  expect_equal(addr_A, data.table::address(DT_A))
  expect_true(all.equal(DT_A, DT_A_copy))
  expect_true(all.equal(DT_B, DT_B_copy))

  dtjoin_anti(DT_A, DT_B, on="id_A == id_B", mult.DT="first", show=TRUE)
  expect_equal(addr_A, data.table::address(DT_A))
  expect_true(all.equal(DT_A, DT_A_copy))
  expect_true(all.equal(DT_B, DT_B_copy))

  dtjoin_cross(DT_A, DT_B)
  expect_equal(addr_A, data.table::address(DT_A))
  expect_true(all.equal(DT_A, DT_A_copy))
  expect_true(all.equal(DT_B, DT_B_copy))
})

# ------------------------------------------------------------------------------
# zero-length outputs (esp. with indicate and setDF(list()))

desc <- "empty output"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  x <- data.frame(id=1)
  y <- data.frame(id=2)
  result <-
    fjoin_inner(x, y, on="id")
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(class(result), c("data.frame"))
  expect_true(nrow(result)==0)
})

desc <- "setDF(list()) with indicate"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  sf1 <- sf::st_sf(id=1:2, geom=sf::st_sfc(sf::st_point(c(1,1)),sf::st_point(c(2,2))))
  sf2 <- sf::st_sf(id=1:2, geom=sf::st_sfc(sf::st_point(c(3,3)),sf::st_point(c(4,4))))
  result <-
    fjoin_inner(sf1, sf2, on="id", indicate=TRUE)
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(result$.join, c(3L,3L))
})

# ------------------------------------------------------------------------------
# mock joins
test_that("dtjoin mock", {
  expect_output(dtjoin(on="id"))
  expect_null(dtjoin(on="id"))
  expect_no_error(dtjoin(on="id"))
})

test_that("dtjoin_semi mock", {
  expect_output(dtjoin_semi(on="id"))
  expect_null(dtjoin_semi(on="id"))
  expect_no_error(dtjoin_semi(on="id"))
})

test_that("dtjoin_anti mock", {
  expect_output(dtjoin_anti(on="id"))
  expect_null(dtjoin_anti(on="id"))
  expect_no_error(dtjoin_anti(on="id"))
})

test_that("dtjoin_cross mock", {
  expect_output(dtjoin_cross())
  expect_null(dtjoin_cross())
  expect_no_error(dtjoin_cross())
})

# ------------------------------------------------------------------------------
# non-valid/reserved column names
test_that("non-valid column name", {
  x <- data.table::data.table(id=1, `non valid`=1L)
  y <- data.table::copy(x)
  dtjoin(x, y, on=c("id")) |> expect_error()
  dtjoin_semi(x, y, on=c("id")) |> expect_error()
  dtjoin_anti(x, y, on=c("id")) |> expect_error()
  dtjoin_cross(x, y, on=c("id")) |> expect_error()
})

test_that("non-valid join column name in mock join", {
  dtjoin(on=c("non valid")) |> expect_error()
  dtjoin_semi(on=c("non valid")) |> expect_error()
  dtjoin_anti(on=c("non valid")) |> expect_error()
  dtjoin_cross(on=c("non valid")) |> expect_error()
})

test_that("reserved column name", {
  x <- data.table::data.table(id=1, fjoin.blah=1L)
  y <- data.table::copy(x)
  dtjoin(x, y, on=c("id")) |> expect_error()
  dtjoin_semi(x, y, on=c("id")) |> expect_error()
  dtjoin_anti(x, y, on=c("id")) |> expect_error()
  dtjoin_cross(x, y, on=c("id")) |> expect_error()
})

test_that("reserved join column name in mock join", {
  dtjoin(on=c("fjoin.blah")) |> expect_error()
  dtjoin(on=c("fjoin_blah")) |> expect_no_error()
  dtjoin(on=c("blah_fjoin.")) |> expect_no_error()
})

# ------------------------------------------------------------------------------
# non-existent join columns
test_that("dtjoin non-existent join column .DT", {
  dtjoin(DF_A, DF_B, on=c("id_A == id_B", "foo == col1")) |>
    expect_error("No column named \"foo\" found in `.DT`")
})

test_that("dtjoin non-existent join column .i", {
  dtjoin(DF_A, DF_B, on=c("id_A == id_B", "t_A == foo")) |>
    expect_error("No column named \"foo\" found in `.i`")
})

test_that("dtjoin_semi non-existent join column .DT", {
  dtjoin_semi(DF_A, DF_B, on=c("id_A == id_B", "foo == t_B")) |>
    expect_error("No column named \"foo\" found in `.DT`")
})

test_that("dtjoin_semi non-existent join column .i", {
  dtjoin_semi(DF_A, DF_B, on=c("id_A == id_B", "t_A == foo")) |>
    expect_error("No column named \"foo\" found in `.i`")
})

test_that("dtjoin_anti non-existent join column .DT", {
  dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B", "foo == t_B")) |>
    expect_error("No column named \"foo\" found in `.DT`")
})

test_that("dtjoin_anti non-existent join column .i", {
  dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B", "t_A == foo")) |>
    expect_error("No column named \"foo\" found in `.i`")
})
