DT_A[, c("k3", "k2", "k1") := list(1:9,rep(1:3,each=3),rep(1:3,times=3))]
data.table::setkey(DT_A,k1,k2,k3)

test_that("null output key, dtjoin() with keyed .i, nomatch.DT=NA", {
  expect_null(data.table::key(dtjoin(DT_B,DT_A,on="id_B==id_A",nomatch.DT=NA)))
})

test_that("null output key, dtjoin() with keyed .i, .DT not data.table", {
  expect_null(data.table::key(dtjoin(as.data.frame(DT_B),DT_A,on="id_B==id_A")))
})

test_that("null output key, dtjoin() with unkeyed .i", {
  expect_null(data.table::key(dtjoin(DT_A,DT_B,on="id_A==id_B")))
})

test_that("output key, dtjoin functions with keyed .i", {

  select <- NULL
  expected_key <- c("k1","k2","k3")

  res <- dtjoin(DT_B,DT_A,on="id_B==id_A",select=select)
  expect_equal(data.table::key(res), expected_key)
  expect_true(data.table:::is.sorted(res, by=expected_key))

  res <- dtjoin_semi(DT_A,DT_B,on="id_A==id_B",select=select)
  expect_equal(data.table::key(res), expected_key)
  expect_true(data.table:::is.sorted(res, by=expected_key))

  res <- dtjoin_anti(DT_A,DT_B,on="id_A==id_B",select=select)
  expect_equal(data.table::key(res), expected_key)
  expect_true(data.table:::is.sorted(res, by=expected_key))

  res <- dtjoin_cross(DT_B,DT_A,select=select)
  expect_equal(data.table::key(res), expected_key)
  expect_true(data.table:::is.sorted(res, by=expected_key))

  select <- c("k2","k1")
  expected_key <- c("k1","k2")
  expect_equal(data.table::key(dtjoin(DT_B,DT_A,on="id_B==id_A",select=select)), expected_key)
  expect_equal(data.table::key(dtjoin_semi(DT_A,DT_B,on="id_A==id_B",select=select)), expected_key)
  expect_equal(data.table::key(dtjoin_anti(DT_A,DT_B,on="id_A==id_B",select=select)), expected_key)
  expect_equal(data.table::key(dtjoin_cross(DT_B,DT_A,select=select)), expected_key)

  select <- c("k3","k1")
  expected_key <- "k1"
  expect_equal(data.table::key(dtjoin(DT_B,DT_A,on="id_B==id_A",select=select)), expected_key)
  expect_equal(data.table::key(dtjoin_semi(DT_A,DT_B,on="id_A==id_B",select=select)), expected_key)
  expect_equal(data.table::key(dtjoin_anti(DT_A,DT_B,on="id_A==id_B",select=select)), expected_key)
  expect_equal(data.table::key(dtjoin_cross(DT_B,DT_A,select=select)), expected_key)

  select <- c("k3","k2")
  expect_null(data.table::key(dtjoin(DT_B,DT_A,on="id_B==id_A",select=select)))
  expect_null(data.table::key(dtjoin_semi(DT_A,DT_B,on="id_A==id_B",select=select)))
  expect_null(data.table::key(dtjoin_anti(DT_A,DT_B,on="id_A==id_B",select=select)))
  expect_null(data.table::key(dtjoin_cross(DT_B,DT_A,select=select)))

  select <- "c"
  expect_null(data.table::key(dtjoin(DT_B,DT_A,on="id_B==id_A",select=select)))
  expect_null(data.table::key(dtjoin_semi(DT_A,DT_B,on="id_A==id_B",select=select)))
  expect_null(data.table::key(dtjoin_anti(DT_A,DT_B,on="id_A==id_B",select=select)))
  expect_null(data.table::key(dtjoin_cross(DT_B,DT_A,select=select)))
})

test_that("output key, exceptional case dtjoin_semi (2)", {
  # data.table join call where .DT is i
  # but no special handling needed since key preservation is separate
  select <- NULL
  expected_key <- c("k1","k2","k3")
  res <- dtjoin_semi(DT_A, DT_B, on="t_A > t_B",select=select)
  expect_equal(data.table::key(res), expected_key)
  expect_true(data.table:::is.sorted(res, by=expected_key))
})

test_that("output key, dtjoin functions with keyed .i, colliding names", {

  DT_B[, `:=`(k2 = NA)]

  expected_key <- c("k1","i.k2","k3")
  res <- dtjoin(DT_B,DT_A,on="id_B==id_A")
  expect_equal(data.table::key(res), expected_key)
  res <- dtjoin_cross(DT_B,DT_A)
  expect_equal(data.table::key(res), expected_key)

  expected_key <- c("k1","k2","k3")
  res <- dtjoin(DT_B,DT_A,on="id_B==id_A",i.home=TRUE)
  expect_equal(data.table::key(res), expected_key)
  res <- dtjoin_cross(DT_B,DT_A,i.home=TRUE)
  expect_equal(data.table::key(res), expected_key)
})

