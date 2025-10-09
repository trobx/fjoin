# Test sfc columns and active geometry with sf inputs/outputs

base <- matrix(
  c(0, 0,
    1, 0,
    1, 1,
    0, 1,
    0, 0),
  ncol=2,
  byrow=TRUE
)
SF2_A <- sf::st_sf(
  id=1:3,
  c=paste0("A",1:3),
  geom_active_A=sf::st_sfc(lapply(0:2, \(x) sf::st_polygon(list(base + x))), crs = 4326),
  geom_other_A=sf::st_sfc(lapply(2:4, \(x) sf::st_polygon(list(base + x))), crs = 4326),
  sf_column_name="geom_active_A"
)
SF2_A

SF2_B <- sf::st_sf(
  id=2:5,
  c=paste0("B",1:4),
  geom_active_B=sf::st_sfc(lapply(4:7, \(x) sf::st_polygon(list(base + x))), crs = 4326),
  sf_column_name="geom_active_B"
)
SF2_B

# ______________________________________________________________________________
# active geometry in sf output

test_that("sf active geometry", {
  result <- fjoin_inner(SF2_A, SF2_B, on="id")
  expect_identical(attr(result, "sf_column"), "geom_active_A")
})

test_that("sf active geometry order right", {
  # tests i.class
  result <- fjoin_inner(SF2_A, SF2_B, on="id", order="right")
  expect_identical(attr(result, "sf_column"), "geom_active_A")
})

test_that("df no active geometry selected", {
  result <- fjoin_inner(SF2_A, SF2_B, on="id", select="c")
  expect_identical(attr(result, "sf_column"), NULL)
  expect_identical(class(result), "data.frame")
})

# ______________________________________________________________________________
# bboxes updated for sfc columns

test_that("sfc bboxes with sf output", {
  result <- fjoin_inner(SF2_A, SF2_B, on="id")
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(as.numeric(sf::st_bbox(result)), c(1,1,3,3))
    expect_identical(as.numeric(attr(result$geom_active_A, "bbox")), c(1,1,3,3))
    expect_identical(as.numeric(attr(result$geom_other_A, "bbox")), c(3,3,5,5))
    expect_identical(as.numeric(attr(result$geom_active_B, "bbox")), c(4,4,6,6))
})

test_that("sfc bboxes with non-sf output", {
  result <- fjoin_inner(as.data.frame(SF2_A), as.data.frame(SF2_B), on="id")
    expect_identical(class(result), "data.frame")
    expect_identical(as.numeric(attr(result$geom_active_A, "bbox")), c(1,1,3,3))
    expect_identical(as.numeric(attr(result$geom_other_A, "bbox")), c(3,3,5,5))
    expect_identical(as.numeric(attr(result$geom_active_B, "bbox")), c(4,4,6,6))
})

# ______________________________________________________________________________
# sfc and select for semi and anti

desc <- "anti-join with sfc and select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_anti(SF2_A, SF2_B, on="id", select="geom_active_A")
  compare <-
    fjoin_left(SF2_A, SF2_B, on="id", select="geom_active_A", indicate=TRUE) |>
    subset(.join==1, select=c("id","geom_active_A")) |>
    unique()
  rownames(compare) <- NULL
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(result, compare)
})

desc <- "semi-join (1a) with sfc and select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_semi(SF2_A, SF2_B, on="id", select="geom_active_A")
  compare <-
    fjoin_left(SF2_A, SF2_B, on="id", select="geom_active_A", indicate=TRUE) |>
    subset(.join==3, select=c("id","geom_active_A")) |>
    unique()
  rownames(compare) <- NULL
  if (PRINT_TEST_OBJECTS) print(result)
    expect_identical(result, compare)
})

desc <- "semi-join (1b) with sfc and select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_semi(SF2_A, SF2_B, on="id", match.na=TRUE, select="geom_active_A")
  compare <-
    fjoin_left(SF2_A, SF2_B, on="id", match.na=TRUE, select="geom_active_A", indicate=TRUE) |>
    subset(.join==3, select=c("id","geom_active_A")) |>
    unique()
  rownames(compare) <- NULL
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(result, compare)
})

desc <- "semi-join (2) with sfc and select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_semi(SF2_A, SF2_B, on="id<id", select="geom_active_A")
  compare <-
    fjoin_left(SF2_A, SF2_B, on="id<id", select="geom_active_A", indicate=TRUE) |>
    subset(.join==3, select=c("id","geom_active_A")) |>
    unique()
  rownames(compare) <- NULL
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(result, compare)
})

desc <- "semi-join (3) with sfc and select"
if (PRINT_TEST_NAME) cat("\nTest:", desc, "\n")
test_that(desc, {
  result <-
    fjoin_semi(SF2_A, SF2_B, on="id<id", mult.y="first", select="geom_active_A")
  compare <-
    subset(SF2_A[1,],select=c("id","geom_active_A"))
  rownames(compare) <- NULL
  if (PRINT_TEST_OBJECTS) print(result)
  expect_identical(result, compare)
})

# ______________________________________________________________________________
# sfc name collisions for true and cross joins

SF3_A <- data.table::copy(SF2_A)
SF3_B <- data.table::copy(SF2_B)
data.table::setnames(SF3_A, "geom_active_A", "geom")
data.table::setnames(SF3_B, "geom_active_B", "geom")
sf::st_geometry(SF3_A) <- "geom"
sf::st_geometry(SF3_B) <- "geom"

test_that("dtjoin, sf_column with colliding name", {
  result <- dtjoin(SF3_A, SF3_B, on="id")
  expect_identical(attr(result, "sf_column"), "geom")
  expect_equal(SF3_A[SF3_A$id==2L, "geom", drop=TRUE], result[result$id==2L,"geom", drop=TRUE])
})

test_that("dtjoin, sf_column with colliding name, i.class=TRUE", {
  result <- dtjoin(SF3_A, SF3_B, on="id", i.class=TRUE)
  expect_identical(attr(result, "sf_column"), "i.geom")
  expect_equal(SF3_B[SF3_B$id==2L, "geom", drop=TRUE], result[result$id==2L,"i.geom", drop=TRUE])
})

test_that("dtjoin, sf_column with colliding name, i.home=TRUE", {
  result <- dtjoin(SF3_A, SF3_B, on="id", i.home=TRUE)
  expect_identical(attr(result, "sf_column"), "geom")
  expect_equal(SF3_B[SF3_B$id==2L, "geom", drop=TRUE], result[result$id==2L,"geom", drop=TRUE])
})

test_that("dtjoin, sf_column with colliding name, i.home=TRUE, i.class=FALSE", {
  result <- dtjoin(SF3_A, SF3_B, on="id", i.home=TRUE, i.class=FALSE)
  expect_identical(attr(result, "sf_column"), "x.geom")
  expect_equal(SF3_A[SF3_A$id==2L, "geom", drop=TRUE], result[result$id==2L,"x.geom", drop=TRUE])
})

test_that("dtjoin_cross, sf_column with colliding name", {
  result <- dtjoin_cross(SF3_A, SF3_B)
  expect_identical(attr(result, "sf_column"), "geom")
  expect_equal(SF3_A[SF3_A$id==2L, "geom", drop=TRUE], result[match(2L,result$id),"geom", drop=TRUE])
})

test_that("dtjoin_cross, sf_column with colliding name, i.class=TRUE", {
  result <- dtjoin_cross(SF3_A, SF3_B, i.class=TRUE)
  expect_identical(attr(result, "sf_column"), "i.geom")
  expect_equal(SF3_B[SF3_B$id==2L, "geom", drop=TRUE], result[match(2L,result$id),"i.geom", drop=TRUE])
})

test_that("dtjoin_cross, sf_column with colliding name, i.home=TRUE", {
  result <- dtjoin_cross(SF3_A, SF3_B, i.home=TRUE)
  expect_identical(attr(result, "sf_column"), "geom")
  expect_equal(SF3_B[SF3_B$id==2L, "geom", drop=TRUE], result[match(2L,result$id),"geom", drop=TRUE])
})

test_that("dtjoin_cross, sf_column with colliding name, i.home=TRUE, i.class=FALSE", {
  result <- dtjoin_cross(SF3_A, SF3_B, i.home=TRUE, i.class=FALSE)
  expect_identical(attr(result, "sf_column"), "x.geom")
  expect_equal(SF3_A[SF3_A$id==2L, "geom", drop=TRUE], result[match(2L,result$x.id),"x.geom", drop=TRUE])
})
