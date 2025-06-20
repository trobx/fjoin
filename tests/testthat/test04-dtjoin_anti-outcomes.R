# (1) no mult or mult.DT, single equality
test_that("dtjoin_anti single equality", {
  result <-
    dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B"))
  # compare <-
  #   dtjoin(DF_B, DF_A, on=c("id_B == id_A"), indicate = TRUE, i.main = TRUE) |>
  #   data.table::setDT() |>
  #   _[.join==1, unique(.SD), .SDcols = names(DF_A)] |>
  #   data.table::setDF()
  compare <-
    dplyr::anti_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B), na_matches = "never")
  print(result)
  print(compare)
  expect_identical(result, compare)
  expect_identical(class(result), class(compare))
})

# (2) no mult or mult.DT, other
  test_that("dtjoin_anti general", {
    result <-
      dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"))
    compare <-
      dplyr::anti_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), na_matches = "never")
    print(result)
    print(compare)
    expect_identical(result, compare)
    expect_identical(class(result), class(compare))
  })

# (3) mult, with or without mult.DT
  test_that("dtjoin_anti mult = \"first\"", {
    result <-
      dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), mult = "first")
    compare <-
      DF_A[!DF_A$c %in% dtjoin(DF_A, DF_B, on=c("id_A == id_B"), nomatch = NULL, mult = "first")$c,]
    rownames(compare) <- NULL
    print(result)
    print(compare)
    expect_identical(result, compare)
    expect_identical(class(result), class(compare))
  })

# (4) mult.DT, no mult
  test_that("dtjoin_anti mult.DT = \"last\"", {
    result <-
      dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), mult.DT = "last")
    compare <-
      DF_A[!DF_A$c %in% dtjoin(DF_A, DF_B, on=c("id_A == id_B"), nomatch = NULL, mult.DT = "last")$c,]
    rownames(compare) <- NULL
    print(result)
    print(compare)
    expect_identical(result, compare)
    expect_identical(class(result), class(compare))
  })

  # ------------------------------------------------------------------------------
  # (1) select
  test_that("dtjoin_anti single equality (select)", {
    result <-
      dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), select = "c")
    compare <-
      dplyr::anti_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B), na_matches = "never") |> dplyr::select(id_A, c)
    print(result)
    print(compare)
    expect_identical(result, compare)
  })

  # (2) select
  test_that("dtjoin_anti general (select)", {
    result <-
      dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B", "t_A > t_B"), select = "c")
    compare <-
      dplyr::anti_join(DF_A, DF_B, by=dplyr::join_by(id_A == id_B, t_A > t_B), na_matches = "never") |> dplyr::select(id_A, t_A, c)
    print(result)
    print(compare)
    expect_identical(result, compare)
  })

  # (3) select
  test_that("dtjoin_anti mult = \"first\" (select)", {
    result <-
      dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), mult = "first", select = "c")
    compare <-
      DF_A[!DF_A$c %in% dtjoin(DF_A, DF_B, on=c("id_A == id_B"), nomatch = NULL, mult = "first")$c,] |> dplyr::select(id_A, c)
    rownames(compare) <- NULL
    print(result)
    print(compare)
    expect_identical(result, compare)
  })

  # (4) select
  test_that("dtjoin_anti mult.DT = \"last\" (select)", {
    result <-
      dtjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), mult.DT = "last", select = "c")
    compare <-
      DF_A[!DF_A$c %in% dtjoin(DF_A, DF_B, on=c("id_A == id_B"), nomatch = NULL, mult.DT = "last")$c,] |> dplyr::select(id_A, c)
    rownames(compare) <- NULL
    print(result)
    print(compare)
    expect_identical(result, compare)
  })
