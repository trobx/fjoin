# as-is data.table inputs left intact-------------------------------------------
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

# data.table outputs do not trigger shallow copy when assigned to------------------
test_that("data.table outputs are good to go", {
  ans <- fjoin_inner(data.table::setkey(data.table::copy(DT_A),"id_A"),DT_B,on=c("id_A==id_B"))
  attr(ans, "sorted") <- "id_A" # previously used this, now `setattr(ans, "sorted", key)`
  expect_warning(ans[, new := 1L], "^A shallow copy of this data.table")

  ans <- fjoin_inner(data.table::setkey(data.table::copy(DT_A),"id_A"),DT_B,on=c("id_A==id_B"))
  expect_no_warning(ans[, new := 1L])
  ans <- fjoin_semi(data.table::setkey(data.table::copy(DT_A),"id_A"),DT_B,on=c("id_A==id_B"))
  expect_no_warning(ans[, new := 1L])
  ans <- fjoin_anti(data.table::setkey(data.table::copy(DT_A),"id_A"),DT_B,on=c("id_A==id_B"))
  expect_no_warning(ans[, new := 1L])
  ans <- fjoin_cross(data.table::setkey(data.table::copy(DT_A),"id_A"),DT_B)
  expect_no_warning(ans[, new := 1L])

  rm(ans)
})

# invalid .DT, .i input---------------------------------------------------------
test_that("invalid input", {
  expect_error(dtjoin(DF_A,letters,on="id"), "^'.i' must be")
  expect_error(dtjoin(letters,letters,on="id"), "^'.DT' must be")
})


# zero-length outputs (esp. with indicate and setDF(list()))--------------------
test_that("empty output", {
  x <- data.frame(id=1)
  y <- data.frame(id=2)
  result <- fjoin_inner(x, y, on="id")
  expect_identical(class(result), c("data.frame"))
  expect_true(nrow(result)==0)
})

test_that("empty output with setDF(list()) and indicate", {
  sf1 <- sf::st_sf(id=1:2, geom=sf::st_sfc(sf::st_point(c(1,1)),sf::st_point(c(2,2))))
  sf2 <- sf::st_sf(id=3:4, geom=sf::st_sfc(sf::st_point(c(3,3)),sf::st_point(c(4,4))))
  result <- fjoin_inner(sf1, sf2, on="id", indicate=TRUE)
  expect_true(nrow(result)==0)
})

test_that("right join with empty right anti-join and indicate", {
  x <- data.frame(id=1)
  expect_no_error(fjoin_right(x,x,on="id",indicate=TRUE))
})

test_that("right join with empty inner join and indicate", {
  x <- data.frame(id=1)
  y <- data.frame(id=2)
  expect_no_error(fjoin_right(x,y,on="id",indicate=TRUE))
})

# natural joins-----------------------------------------------------------------
df1 <- data.frame(id=1:3,a=c("a","a","b"),v1=1L)
df2 <- data.frame(id=2:4,a=c("a","a","b"),v2=2L)
df3 <- df2; names(df3) <- c("i","j","k")

test_that("natural joins", {
  expect_identical(dtjoin(df1,df2,on=NA), dtjoin(df1,df2,on=intersect(names(df1),names(df2))))
  expect_identical(dtjoin_semi(df1,df2,on=NA), dtjoin_semi(df1,df2,on=intersect(names(df1),names(df2))))
  expect_identical(dtjoin_anti(df1,df2,on=NA), dtjoin_anti(df1,df2,on=intersect(names(df1),names(df2))))
})
test_that("'on' is a required argument", {
  expected_error <- "argument \"on\" is missing, with no default"
  expect_error(dtjoin(df1,df2), expected_error)
  expect_error(dtjoin_semi(df1,df2), expected_error)
  expect_error(dtjoin_anti(df1,df2), expected_error)
})
test_that("natural join fails if no common names", {
  expected_error <- "Natural join requested \\('on' = NA\\) but there are no columns with common names"
  expect_error(dtjoin(df1,df3,on=NA), expected_error)
  expect_error(dtjoin_semi(df1,df3,on=NA), expected_error)
  expect_error(dtjoin_anti(df1,df3,on=NA), expected_error)
})
test_that("mock natural join is not allowed", {
  expected_error <- "A natural join \\('on' = NA\\) requires non-NULL inputs"
  expect_error(dtjoin(on=NA), expected_error)
  expect_error(dtjoin_semi(on=NA), expected_error)
  expect_error(dtjoin_anti(on=NA), expected_error)
  expected_error <- "'on' must be a non-empty character vector with no empty strings or NAs"
  expect_error(dtjoin(on=c(NA,NA)))
  expect_error(dtjoin_semi(on=c(NA,NA)))
  expect_error(dtjoin_anti(on=c(NA,NA)))
})

# mock joins--------------------------------------------------------------------
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

# non-valid/reserved column names-----------------------------------------------
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

# non-existent join columns-----------------------------------------------------
test_that("dtjoin non-existent join column .DT", {
  dtjoin(DF_A, DF_B, on=c("id_A == id_B", "foo == col1")) |>
    expect_error("Join column\\(s\\) not found in `.DT`: foo")
})

test_that("dtjoin non-existent join column .i", {
  dtjoin(DF_A, DF_B, on=c("id_A == id_B", "t_A == foo")) |>
    expect_error("Join column\\(s\\) not found in `.i`: foo")
})

test_that("dtjoin_semi non-existent join column .DT", {
  dtjoin_semi(DF_A, DF_B, on=c("id_A == id_B", "foo == t_B")) |>
    expect_error("Join column\\(s\\) not found in `.DT`: foo")
})

test_that("dtjoin_semi non-existent join column .i", {
  dtjoin_semi(DF_A, DF_B, on=c("id_A == id_B", "t_A == foo")) |>
    expect_error("Join column\\(s\\) not found in `.i`: foo")
})

test_that("dtjoin_anti non-existent join column .DT", {
  dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B", "foo == t_B")) |>
    expect_error("Join column\\(s\\) not found in `.DT`: foo")
})

test_that("dtjoin_anti non-existent join column .i", {
  dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B", "t_A == foo")) |>
    expect_error("Join column\\(s\\) not found in `.i`: foo")
})

# na.match=FALSE but no equality predicates-------------------------------------
test_that("na.match=FALSE with no equality predicates", {
  out <- capture.output(dtjoin(DF_A, DF_B, on=c("t_A > t_B"), do=FALSE))
  expect_false(any(grepl("na\\.omit", out)))
  out <- capture.output(dtjoin_semi(DF_A, DF_B, on=c("t_A > t_B"), do=FALSE))
  expect_false(any(grepl("na\\.omit", out)))
  out <- capture.output(dtjoin_anti(DF_A, DF_B, on=c("t_A > t_B"), do=FALSE))
  expect_false(any(grepl("na\\.omit", out)))
})
