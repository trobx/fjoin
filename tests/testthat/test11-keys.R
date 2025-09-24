# Keyed output when .i is a keyed data.table and output is a data.table
test_that("dtjoin functions with keyed .i", {
  DT_A[, c("k3", "k2", "k1") := list(1:9,rep(1:3,each=3),rep(1:3,times=3))]
  data.table::setkey(DT_A,k1,k2,k3)

  expect_null(data.table::key(dtjoin(DT_B,DT_A,on="id_B==id_A",nomatch.DT=NA)))

  expected_key <- c("k1","k2","k3")
  expect_equal(data.table::key(dtjoin(DT_B,DT_A,on="id_B==id_A")), expected_key)
  expect_equal(data.table::key(dtjoin_semi(DT_A,DT_B,on="id_A==id_B")), expected_key)
  expect_equal(data.table::key(dtjoin_anti(DT_A,DT_B,on="id_A==id_B")), expected_key)
  expect_equal(data.table::key(dtjoin_cross(DT_B,DT_A)), expected_key)

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
