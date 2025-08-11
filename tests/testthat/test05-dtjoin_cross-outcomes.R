DF_A <- DF_A[1:3,]
DF_B <- DF_B[1:2,]

desc <- "dtjoin_cross"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_cross(DF_A,DF_B,show=SHOW)
  compare <-
    dplyr::cross_join(DF_B,DF_A) |> dplyr::select(id_A,t_A,c.y,v_A,id_B,t_B,c.x,v_B)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
  expect_identical(class(result), class(compare))
})

desc <- "dtjoin_cross (select)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_cross(DF_A,DF_B,select=c("c"),show=SHOW)
  compare <-
    dplyr::cross_join(DF_B,DF_A) |> dplyr::select(c.y, c.x)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
  expect_identical(class(result), class(compare))
})

desc <- "dtjoin_cross (select, i.home)"
if (PRINT_TEST_NAME) cat("\nTest: ", desc, "\n")
test_that(desc, {
  result <-
    dtjoin_cross(DF_A,DF_B,select=c("c"),i.home=TRUE,show=SHOW)
  compare <-
    dplyr::cross_join(DF_B,DF_A) |> dplyr::select(c.x, c.y)
  if (PRINT_TEST_OBJECTS) {print(result); print(compare)}
  expect_true(all.equal(result, compare, check.attributes = FALSE))
  expect_identical(class(result), class(compare))
})
