library(data.table)

dt <- setDT(iris[1:5])

# ------------------------------------------------------------------------------
# mock joins

test_that("dtjoin mock", {
  expect_output(dtjoin(on="id"))
  expect_null(dtjoin(on="id"))
  expect_no_error(dtjoin(on="id"))
})

test_that("dtjoin_semi_i mock", {
  expect_output(dtjoin_semi_i(on="id"))
  expect_null(dtjoin_semi_i(on="id"))
  expect_no_error(dtjoin_semi_i(on="id"))
})

test_that("dtjoin_anti_DT mock", {
  expect_output(dtjoin_anti_DT(on="id"))
  expect_null(dtjoin_anti_DT(on="id"))
  expect_no_error(dtjoin_anti_DT(on="id"))
})

# ------------------------------------------------------------------------------
# data.table setup and inputs

# test_that("do TRUE but .DT not a data.table", {
#   dtjoin(iris, iris, on=c("Species", "foo == Petal.Length")) |>
#     expect_error("'.DT' is not a data.table")
# })
#
# test_that("do TRUE but .i not a data.table", {
#   dtjoin(as.data.table(iris), iris, on=c("Species", "foo == Petal.Length")) |>
#     expect_error("'.i' is not a data.table")
# })
#
# test_that("do FALSE and .DT, .i data.frames", {
#   dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
#     expect_output()
#   dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
#     expect_null()
#   dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
#     expect_no_error()
# })
#
# test_that("do FALSE but .DT not a data.frame", {
#   dtjoin("iris", iris, on=c("Species", "foo == Petal.Length"), do = FALSE) |>
#     expect_error("'.DT' must have class \"data.frame\"")
# })
#
# test_that("do FALSE but .i not a data.frame", {
#   dtjoin(iris, "iris", on=c("Species", "foo == Petal.Length"), do = FALSE) |>
#     expect_error("'.i' must have class \"data.frame\"")
# })

# ------------------------------------------------------------------------------
# data.table not loaded

# detach(package:data.table, unload=TRUE)
#
# test_that("do FALSE and data.table not loaded", {
#   dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
#     expect_output()
#   dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
#     expect_null()
#   dtjoin(iris, iris, on=c("Species"), do = FALSE) |>
#     expect_no_error()
# })
#
# test_that("do TRUE but data.table not loaded", {
#   dtjoin(iris, iris, on=c("Species", "foo == Petal.Length")) |>
#     expect_error("data.table is not loaded")
# })
#
# library(data.table)

# ------------------------------------------------------------------------------
# non-existent join columns

test_that("dtjoin non-existent join column .DT", {
  dtjoin(dt, dt, on=c("Species", "foo == Petal.Length")) |>
    expect_error("No column named \"foo\" found in `.DT`")
})

test_that("dtjoin non-existent join column .i", {
  dtjoin(dt, dt, on=c("Species", "Petal.Length == foo")) |>
    expect_error("No column named \"foo\" found in `.i`")
})

test_that("dtjoin_semi_i non-existent join column .DT", {
  dtjoin_semi_i(dt, dt, on=c("Species", "foo == Petal.Length")) |>
    expect_error("No column named \"foo\" found in `.DT`")
})

test_that("dtjoin_semi_i non-existent join column .i", {
  dtjoin_semi_i(dt, dt, on=c("Species", "Petal.Length == foo")) |>
    expect_error("No column named \"foo\" found in `.i`")
})

test_that("dtjoin_anti_DT non-existent join column .DT", {
  dtjoin_anti_DT(dt, dt, on=c("Species", "foo == Petal.Length")) |>
    expect_error("No column named \"foo\" found in `.DT`")
})

test_that("dtjoin_anti_DT non-existent join column .i", {
  dtjoin_anti_DT(dt, dt, on=c("Species", "Petal.Length == foo")) |>
    expect_error("No column named \"foo\" found in `.i`")
})

# ------------------------------------------------------------------------------
# argument checks

test_that("order arg check", {
  fjoin_left(dt, dt, on = "Species", order = "x") |>
    expect_no_error()
  fjoin_left(dt, dt, on = "Species", order = "y") |>
    expect_no_error()
  fjoin_left(dt, dt, on = "Species", order = TRUE) |>
    expect_error("Argument 'order' must be \"x\" or \"y\"")
})

test_that("mult arg check", {
  dtjoin(on = "id", mult = "all") |>
    expect_no_error()
  dtjoin(on = "id", mult = "foo") |>
    expect_error("Argument 'mult' must be \"all\", \"first\", or \"last\"")
})

test_that("nomatch arg check", {
  dtjoin(on = "id", nomatch = NULL) |>
    expect_no_error()
  dtjoin(on = "id", nomatch = NA) |>
    expect_no_error()
  dtjoin(on = "id", nomatch = 0L) |>
    expect_no_error()
  dtjoin(on = "id", nomatch = FALSE) |>
    expect_no_error()
  dtjoin(on = "id", nomatch = TRUE) |>
    expect_error("Argument 'nomatch' must be NA, NULL, or 0L")
})

test_that("true/false arg check", {
  dtjoin(on = "id", match.na = TRUE) |>
    expect_no_error()
  dtjoin(on = "id", match.na = NA) |>
    expect_error("Argument 'match.na' must be TRUE or FALSE")
})


# ------------------------------------------------------------------------------
# strsplit_predicate and flip_on

strsplit_predicate("id1>=id2")
flip_on("id1>=id2")

strsplit_predicate("    id1   >=id2")
flip_on("    id1   >=id2")

# ------------------------------------------------------------------------------
# .DT taller than .i

# ------------------------------------------------------------------------------
# i.main

# ------------------------------------------------------------------------------
# on.first

# ------------------------------------------------------------------------------
# preserve

# ------------------------------------------------------------------------------
# no garbling

# ------------------------------------------------------------------------------
# indicate

