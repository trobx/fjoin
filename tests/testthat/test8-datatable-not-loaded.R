# ------------------------------------------------------------------------------
# data.table not loaded

test_that("do FALSE and data.table not loaded", {
  dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
    expect_output()
  dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
    expect_null()
  dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
    expect_no_error()
})

test_that("do TRUE but data.table not loaded", {
  dtjoin(iris, iris, on=c("Species", "foo == Petal.Length")) |>
    expect_error("data.table is not loaded")
})

