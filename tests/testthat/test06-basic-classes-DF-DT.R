# Test basic output classes DF vs DT and labels if show=TRUE

# TODO add check with object that is DT and df - recast to df

# ______________________________________________________________________________
# labels
test_that("labels fjoin_left with do=FALSE, data.frames", {
  expect_null(fjoin_left(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_left(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.DT : y = DF_B \\(cast as data\\.table\\)")
  expect_output(fjoin_left(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.i  : x = DF_A \\(cast as data\\.table\\)")
})
test_that("labels fjoin_semi with do=FALSE, data.frames", {
  expect_null(fjoin_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                 "\\.DT : x = DF_A \\(cast as data\\.table\\)")
  expect_output(fjoin_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                 "\\.i  : y = DF_B \\(cast as data\\.table\\)")
})
test_that("labels fjoin_anti with do=FALSE, data.frames", {
  expect_null(fjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.DT : x = DF_A \\(cast as data\\.table\\)")
  expect_output(fjoin_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.i  : y = DF_B \\(cast as data\\.table\\)")
})
test_that("labels fjoin_right_semi with do=FALSE, data.frames", {
  expect_null(fjoin_right_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_right_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.DT : y = DF_B \\(cast as data\\.table\\)")
  expect_output(fjoin_right_semi(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.i  : x = DF_A \\(cast as data\\.table\\)")
})
test_that("labels fjoin_right_anti with do=FALSE, data.frames", {
  expect_null(fjoin_right_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE))
  expect_output(fjoin_right_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.DT : y = DF_B \\(cast as data\\.table\\)")
  expect_output(fjoin_right_anti(DF_A, DF_B, on=c("id_A == id_B"), do=FALSE),
                "\\.i  : x = DF_A \\(cast as data\\.table\\)")
})
test_that("labels fjoin_cross with do=FALSE, data.frames", {
  expect_null(fjoin_cross(DF_A, DF_B, do=FALSE))
  expect_output(fjoin_cross(DF_A, DF_B, do=FALSE),
                "\\.DT : y = DF_B \\(cast as data\\.table\\)")
  expect_output(fjoin_cross(DF_A, DF_B, do=FALSE),
                "\\.i  : x = DF_A \\(cast as data\\.table\\)")
})

# ______________________________________________________________________________
# basic output class data.frame or data.table
DF_class <- "data.frame"
DT_class <- c("data.table", "data.frame")

# dtjoin (1)
test_that("dtjoin (1) data.frame output class", {
  result <- dtjoin(DF_B, DT_A, on=c("id_B == id_A"))
  expect_identical(class(result), DF_class)
})
test_that("dtjoin (1) i.class=TRUE data.table output class", {
  result <- dtjoin(DF_B, DT_A, on=c("id_B == id_A"), i.class=TRUE)
  expect_identical(class(result), DT_class)
})
test_that("dtjoin (1) data.table output class", {
  result <- dtjoin(DT_A, DF_B, on=c("id_A == id_B"))
  expect_identical(class(result), DT_class)
})
test_that("dtjoin (1) i.class=TRUE data.frame output class", {
  result <- dtjoin(DT_A, DF_B, on=c("id_A == id_B"), i.class=TRUE)
  expect_identical(class(result), DF_class)
})

# dtjoin (2)
test_that("dtjoin (2) data.frame output class", {
  result <- dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult.DT="last")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin (2) i.class=TRUE data.table output class", {
  result <- dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult.DT="last", i.class=TRUE)
  expect_identical(class(result), DT_class)
})
test_that("dtjoin (2) data.table output class", {
  result <- dtjoin(DT_A, DF_B, on=c("id_A == id_B"), mult.DT="last")
  expect_identical(class(result), DT_class)
})
test_that("dtjoin (2) i.class=TRUE data.frame output class", {
  result <- dtjoin(DT_A, DF_B, on=c("id_A == id_B"), mult.DT="last", i.class=TRUE)
  expect_identical(class(result), DF_class)
})

# dtjoin (3)
test_that("dtjoin (3) data.frame output class", {
  result <- dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult="first", mult.DT="last", nomatch=NULL)
  expect_identical(class(result), DF_class)
})
test_that("dtjoin (3) i.class=TRUE data.table output class", {
  result <- dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult="first", mult.DT="last", nomatch=NULL, i.class=TRUE)
  expect_identical(class(result), DT_class)
})
test_that("dtjoin (3) data.table output class", {
  result <- dtjoin(DT_A, DF_B, on=c("id_A == id_B"), mult="first", mult.DT="last", nomatch=NULL)
  expect_identical(class(result), DT_class)
})
test_that("dtjoin (3) i.class=TRUE data.frame output class", {
  result <- dtjoin(DT_A, DF_B, on=c("id_A == id_B"), mult="first", mult.DT="last", nomatch=NULL, i.class=TRUE)
  expect_identical(class(result), DF_class)
})

# dtjoin (4)
test_that("dtjoin (4) data.frame output class", {
  result <- dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult="first", mult.DT="last")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin (4) i.class=TRUE data.table output class", {
  result <- dtjoin(DF_B, DT_A, on=c("id_B == id_A"), mult="first", mult.DT="last", i.class=TRUE)
  expect_identical(class(result), DT_class)
})
test_that("dtjoin (4) data.table output class", {
  result <- dtjoin(DT_A, DF_B, on=c("id_A == id_B"), mult="first", mult.DT="last")
  expect_identical(class(result), DT_class)
})
test_that("dtjoin (4) i.class=TRUE data.frame output class", {
  result <- dtjoin(DT_A, DF_B, on=c("id_A == id_B"), mult="first", mult.DT="last", i.class=TRUE)
  expect_identical(class(result), DF_class)
})

# dtjoin_anti
test_that("dtjoin_anti data.frame output class", {
  result <- dtjoin_anti(DF_B, DT_A, on=c("id_B == id_A"))
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_anti with select data.frame output class", {
  result <- dtjoin_anti(DF_B, DT_A, on=c("id_B == id_A"), select="c")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_anti data.table output class", {
  result <- dtjoin_anti(DT_A, DF_B, on=c("id_A == id_B"))
  expect_identical(class(result), DT_class)
})
test_that("dtjoin_anti with select data.table output class", {
  result <- dtjoin_anti(DT_A, DF_B, on=c("id_A == id_B"), select="c")
  expect_identical(class(result), DT_class)
})

# dtjoin_semi (1a)
test_that("dtjoin_semi (1a) data.frame output class", {
  result <- dtjoin_semi(DF_B, DT_A, on=c("id_B == id_A"))
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_semi (1a) data.table output class", {
  result <- dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A"))
  expect_identical(class(result), DT_class)
})
test_that("dtjoin_semi (1a) with select data.frame output class", {
  result <- dtjoin_semi(DF_B, DT_A, on=c("id_B == id_A"), select="c")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_semi (1a) with select data.table output class", {
  result <- dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A"), select="c")
  expect_identical(class(result), DT_class)
})

# dtjoin_semi (1b)
test_that("dtjoin_semi (1b) data.frame output class", {
  result <- dtjoin_semi(DF_B, DT_A, on=c("t_B == t_A"))
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_semi (1b) data.table output class", {
  result <- dtjoin_anti(DT_B, DF_A, on=c("t_B == t_A"))
  expect_identical(class(result), DT_class)
})
test_that("dtjoin_semi (1b) with select data.frame output class", {
  result <- dtjoin_semi(DF_B, DT_A, on=c("t_B == t_A"), select="c")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_semi (1b) with select data.table output class", {
  result <- dtjoin_anti(DT_B, DF_A, on=c("t_B == t_A"), select="c")
  expect_identical(class(result), DT_class)
})

# dtjoin_semi (2)
test_that("dtjoin_semi (2) data.frame output class", {
  result <- dtjoin_semi(DF_B, DT_A, on=c("id_B == id_A", "t_B < t_A"), mult = "first")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_semi (2) data.table output class", {
  result <- dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), mult = "first")
  expect_identical(class(result), DT_class)
})
test_that("dtjoin_semi (2) with select data.frame output class", {
  result <- dtjoin_semi(DF_B, DT_A, on=c("id_B == id_A", "t_B < t_A"), mult = "first", select="c")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_semi (2) with select data.table output class", {
  result <- dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), mult = "first", select="c")
  expect_identical(class(result), DT_class)
})

# dtjoin_semi (3)
test_that("dtjoin_semi (3) data.frame output class", {
  result <- dtjoin_semi(DF_B, DT_A, on=c("id_B == id_A", "t_B < t_A"), mult = "first")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_semi (3) data.table output class", {
  result <- dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), mult = "first")
  expect_identical(class(result), DT_class)
})
test_that("dtjoin_semi (3) with select data.frame output class", {
  result <- dtjoin_semi(DF_B, DT_A, on=c("id_B == id_A", "t_B < t_A"), mult = "first", select="c")
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_semi (3) with select data.table output class", {
  result <- dtjoin_anti(DT_B, DF_A, on=c("id_B == id_A", "t_B < t_A"), mult = "first", select="c")
  expect_identical(class(result), DT_class)
})

# dtjoin_cross
test_that("dtjoin_cross data.frame output class", {
  result <- dtjoin_cross(DF_B, DT_A)
  expect_identical(class(result), DF_class)
})
test_that("dtjoin_cross i.class=TRUE data.table output class", {
  result <- dtjoin_cross(DF_B, DT_A, i.class=TRUE)
  expect_identical(class(result), DT_class)
})
test_that("dtjoin_cross data.table output class", {
  result <- dtjoin_cross(DT_A, DF_B)
  expect_identical(class(result), DT_class)
})
test_that("dtjoin_cross i.class=TRUE data.frame output class", {
  result <- dtjoin_cross(DT_A, DF_B, i.class=TRUE)
  expect_identical(class(result), DF_class)
})

