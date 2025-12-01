# Test grouped tibble (grouped-df) groups

set.seed(1)
dat <- data.table::setDF(data.table::as.data.table(iris)[, head(.SD,3),by="Species"][order(sample(.N))][, id:=.I])

DF <- dat[c(1,4,7),c("id","Species","Petal.Length")]
GTBL <- dat[, c("id","Species","Sepal.Length")]
GTBL$grp <- rep(c(1L,2L), length.out=nrow(GTBL))
GTBL <- dplyr::group_by_at(GTBL, c("grp", "Species"))

test_that("grouped tibble groups attribute with fjoin", {
  expect_equal(
    dplyr::group_vars(fjoin_inner(GTBL,DF,on="id")),
    c("grp", "Species")
  )
  expect_equal(
    dplyr::group_vars(fjoin_inner(GTBL,DF,on="id",select.x="grp")),
    c("grp")
  )
  expect_equal(
    dplyr::group_vars(fjoin_inner(GTBL,DF,on="id",select.x="Species")),
    c("Species")
  )
  expect_equal(
    dplyr::group_vars(fjoin_inner(GTBL,DF,on="id",select.x="")),
    character(0)
  )
  expect_equal(
    class(fjoin_inner(GTBL,DF,on="id",select.x="")),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("grouped tibble groups attribute with fjoin_semi,_anti,_cross", {
  expect_equal(
    dplyr::group_vars(fjoin_semi(GTBL,DF,on="id",select="Species")),
    c("Species")
  )
  expect_equal(
    dplyr::group_vars(fjoin_semi(GTBL,DF,on="id",select="")),
    character(0)
  )
  expect_equal(
    dplyr::group_vars(fjoin_anti(GTBL,DF,on="id",select="Species")),
    c("Species")
  )
  expect_equal(
    dplyr::group_vars(fjoin_anti(GTBL,DF,on="id",select="")),
    character(0)
  )
  expect_equal(
    dplyr::group_vars(fjoin_cross(GTBL,DF,select="Species")),
    c("Species")
  )
  expect_equal(
    dplyr::group_vars(fjoin_cross(GTBL,DF,select="")),
    character(0)
  )
})

test_that("grouped tibble groups attribute, colliding group column names", {
  # already done above with "Species" vs "R.Species"
  # but test explicitly with dtjoin and include dtjoin_cross
  expect_equal(
    dplyr::group_vars(dtjoin(GTBL,DF,on="id")),
    c("grp", "Species")
  )
  expect_equal(
    dplyr::group_vars(dtjoin(DF,GTBL,on="id",i.class=TRUE)),
    c("grp", "i.Species")
  )
  expect_equal(
    dplyr::group_vars(dtjoin(GTBL,DF,on="id",i.home=TRUE,i.class=FALSE)),
    c("grp", "x.Species")
  )
  expect_equal(
    dplyr::group_vars(dtjoin(GTBL,DF,on="id",i.class=TRUE)),
    character(0)
  )
  expect_equal(
    dplyr::group_vars(dtjoin(DF,GTBL,on="id")),
    character(0)
  )
  expect_equal(
    dplyr::group_vars(dtjoin_cross(GTBL,DF)),
    c("grp", "Species")
  )
  expect_equal(
    dplyr::group_vars(dtjoin_cross(DF,GTBL,i.class=TRUE)),
    c("grp", "i.Species")
  )
  expect_equal(
    dplyr::group_vars(dtjoin_cross(GTBL,DF,i.home=TRUE,i.class=FALSE)),
    c("grp", "x.Species")
  )
  expect_equal(
    dplyr::group_vars(dtjoin_cross(GTBL,DF,i.class=TRUE)),
    character(0)
  )
  expect_equal(
    dplyr::group_vars(dtjoin_cross(DF,GTBL)),
    character(0)
  )
})

