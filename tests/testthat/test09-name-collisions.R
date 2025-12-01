# ------------------------------------------------------------------------------
# check correct handling on name collisions incl. between join col and non-join col

test_that("dtjoin handles name collisions between join column and non-join column", {
  deetee <- data.table::data.table(id1=c(1L,9L), id2="yikes_DT", col_DT=0L, c="A")
  eye <- data.table::data.table(id2=1:2, id1="yikes_i", col_i=0L, c="B")
  # Cases 1 and 3
  result <- dtjoin(deetee, eye, on="id1==id2", on.first=TRUE, indicate=TRUE, nomatch.DT=NA)
  expect_equal(result[.join!=2L, id1], deetee$id1)
  expect_equal(result[.join!=1L, id1], eye$id2)
  expect_equal(result[.join!=2L, id2], deetee$id2)
  expect_equal(result[.join!=1L, i.id1], eye$id1)
  expect_null(result$i.id2)
  result <- dtjoin(deetee, eye, on="id1==id2", on.first=TRUE, indicate=TRUE, both=TRUE, nomatch.DT=NA)
  expect_equal(result[.join!=2L, id1], deetee$id1)
  expect_equal(result[.join!=1L, i.id2], eye$id2)
  expect_equal(result[.join!=2L, id2], deetee$id2)
  expect_equal(result[.join!=1L, i.id1], eye$id1)
  result <- dtjoin(deetee, eye, on="id1==id2", on.first=TRUE, indicate=TRUE, i.home=TRUE, nomatch.DT=NA)
  expect_equal(result[.join!=2L, id2], eye$id2)
  expect_equal(result[.join!=1L, id2], deetee$id1)
  expect_equal(result[.join!=2L, id1], eye$id1)
  expect_null(result$x.id1)
  expect_equal(result[.join!=1L, x.id2], deetee$id2)
  result <- dtjoin(deetee, eye, on="id1==id2", on.first=TRUE, indicate=TRUE, i.home=TRUE, both=TRUE, nomatch.DT=NA)
  expect_equal(result[.join!=2L, id2], eye$id2)
  expect_equal(result[.join!=1L, x.id1], deetee$id1)
  expect_equal(result[.join!=2L, id1], eye$id1)
  expect_equal(result[.join!=1L, x.id2], deetee$id2)
  # Cases 2 and 4 (same tests, calls have `mult.DT`)
  result <- dtjoin(deetee, eye, on="id1==id2", on.first=TRUE, indicate=TRUE, nomatch.DT=NA, mult.DT="first")
  expect_equal(result[.join!=2L, id1], deetee$id1)
  expect_equal(result[.join!=1L, id1], eye$id2)
  expect_equal(result[.join!=2L, id2], deetee$id2)
  expect_equal(result[.join!=1L, i.id1], eye$id1)
  expect_null(result$i.id2)
  result <- dtjoin(deetee, eye, on="id1==id2", on.first=TRUE, indicate=TRUE, both=TRUE, nomatch.DT=NA, mult.DT="first")
  expect_equal(result[.join!=2L, id1], deetee$id1)
  expect_equal(result[.join!=1L, i.id2], eye$id2)
  expect_equal(result[.join!=2L, id2], deetee$id2)
  expect_equal(result[.join!=1L, i.id1], eye$id1)
  result <- dtjoin(deetee, eye, on="id1==id2", on.first=TRUE, indicate=TRUE, i.home=TRUE, nomatch.DT=NA, mult.DT="first")
  expect_equal(result[.join!=2L, id2], eye$id2)
  expect_equal(result[.join!=1L, id2], deetee$id1)
  expect_equal(result[.join!=2L, id1], eye$id1)
  expect_null(result$x.id1)
  expect_equal(result[.join!=1L, x.id2], deetee$id2)
  result <- dtjoin(deetee, eye, on="id1==id2", on.first=TRUE, indicate=TRUE, i.home=TRUE, both=TRUE, nomatch.DT=NA, mult.DT="first")
  expect_equal(result[.join!=2L, id2], eye$id2)
  expect_equal(result[.join!=1L, x.id1], deetee$id1)
  expect_equal(result[.join!=2L, id1], eye$id1)
  expect_equal(result[.join!=1L, x.id2], deetee$id2)
})

# ------------------------------------------------------------------------------
# check correct handling on name collision between join col and non-corresponding join col

test_that("dtjoin handles name collisions between join columns in different predicates", {
  # Cases 1 and 3
  out <- capture.output(dtjoin(on=c("id==foo", "foo==id"), on.first=TRUE, nomatch.DT=NA))
  expect_true(grepl("data.frame\\(id = i.foo, foo = i.id,", out[3]))
  out <- capture.output(dtjoin(on=c("id==foo", "foo==id"), on.first=TRUE, nomatch.DT=NA, both=TRUE))
  expect_true(grepl("data.frame\\(id = x.id, foo = x.foo, i.foo, i.id,", out[3]))
  out <- capture.output(dtjoin(on=c("id==foo", "foo==id"), on.first=TRUE, nomatch.DT=NA, i.home=TRUE))
  expect_true(grepl("data.frame\\(foo = i.foo, id = i.id", out[3])) # could avoid i. reference here (unlike if collision with non-join col in .DT)
  out <- capture.output(dtjoin(on=c("id==foo", "foo==id"), on.first=TRUE, nomatch.DT=NA, i.home=TRUE, both=TRUE))
  expect_true(grepl("data.frame\\(foo = i.foo, id = i.id, x.id, x.foo,", out[3])) # ditto
  # Cases 2 and 4 (calls have `mult.DT`)
  out <- capture.output(dtjoin(on=c("id==foo", "foo==id"), on.first=TRUE, nomatch.DT=NA, mult.DT="first"))
  expect_true(grepl("data.frame\\(id = i.foo, foo = i.id,", out[3]))
  out <- capture.output(dtjoin(on=c("id==foo", "foo==id"), on.first=TRUE, nomatch.DT=NA, both=TRUE, mult.DT="first"))
  expect_true(grepl("data.frame\\(id, foo, i.foo, i.id,", out[3])) # NB different test from Cases 1 and 3 (since no garbling)
  out <- capture.output(dtjoin(on=c("id==foo", "foo==id"), on.first=TRUE, nomatch.DT=NA, i.home=TRUE, mult.DT="first"))
  expect_true(grepl("data.frame\\(foo = i.foo, id = i.id", out[3]))
  out <- capture.output(dtjoin(on=c("id==foo", "foo==id"), on.first=TRUE, nomatch.DT=NA, i.home=TRUE, both=TRUE, mult.DT="first"))
  expect_true(grepl("data.frame\\(foo = i.foo, id = i.id, x.id, x.foo,", out[3]))
})

# ------------------------------------------------------------------------------
# check with fjoin and also that non-standard prefix is correctly applied
test_that("fjoin handles name collisions and applies prefix arg", {
  # name collision between join col and non-corresponding join col
  out <- capture.output(fjoin_left(on=c("id==foo", "foo==id"), on.first=TRUE, prefix="PREFIX_"))
  expect_true(grepl("data.frame\\(id = i.id, foo = i.foo,", out[3]))
  out <- capture.output(fjoin_left(on=c("id==foo", "foo==id"), on.first=TRUE, prefix="PREFIX_", order="right"))
  expect_true(grepl("data.frame\\(id = i.foo, foo = i.id,", out[3]))
  out <- capture.output(fjoin_left(on=c("id==foo", "foo==id"), on.first=TRUE, both=TRUE, prefix="PREFIX_"))
  expect_true(grepl("data.frame\\(id = i.id, foo = i.foo, PREFIX_foo = x.foo, PREFIX_id = x.id,", out[3]))
  out <- capture.output(fjoin_left(on=c("id==foo", "foo==id"), on.first=TRUE, both=TRUE, prefix="PREFIX_", order="right"))
  expect_true(grepl("data.frame\\(id = x.id, foo = x.foo, PREFIX_foo = i.foo, PREFIX_id = i.id,", out[3]))
  # name collision between join col and non-join col
  out <- capture.output(fjoin_left(on=c("id1==col_DT", "col_i==id2"), on.first=TRUE, prefix="PREFIX_"))
  expect_true(grepl("data.frame\\(id1, col_i", out[3]))
  out <- capture.output(fjoin_left(on=c("id1==col_DT", "col_i==id2"), on.first=TRUE, prefix="PREFIX_", order="right"))
  expect_true(grepl("data.frame\\(id1, col_i = i.id2", out[3]))
  out <- capture.output(fjoin_left(on=c("id1==col_DT", "col_i==id2"), on.first=TRUE, both=TRUE, prefix="PREFIX_"))
  expect_true(grepl("data.frame\\(id1, col_i, col_DT = x.col_DT, id2 = x.id2", out[3]))
  out <- capture.output(fjoin_left(on=c("id1==col_DT", "col_i==id2"), on.first=TRUE, both=TRUE, prefix="PREFIX_", order="right"))
  expect_true(grepl("data.frame\\(id1 = x.id1, col_i = x.col_i, PREFIX_col_DT = i.col_DT, id2", out[3]))
})

# ------------------------------------------------------------------------------
# names with i.home=TRUE including outer.DT case
# These tests based on names only; tests of fjoin functions cover column values

test_that("dtjoin names with i.home", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", i.home=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","t_B","x.c","v_B"))
})

test_that("dtjoin names with i.home, outer.DT", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", nomatch.DT=NA, i.home=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","t_B","x.c","v_B"))
})

# rename_anti.DT (outer.DT, i.home), same name, preserve both
test_that("dtjoin names with i.home, outer.DT, preserve both", {
  data.table::setnames(DF_A,"id_A","id")
  data.table::setnames(DF_B,"id_B","id")
  result <- dtjoin(DF_B, DF_A, on="id", nomatch.DT=NA, i.home=TRUE, both=TRUE)
  expect_named(result, c("id","t_A","c","v_A","x.id","t_B","x.c","v_B"))
  data.table::setnames(DF_A,"id","id_A")
  data.table::setnames(DF_B,"id","id_B")
})

# ditto, different names
test_that("dtjoin names with i.home", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", nomatch.DT=NA, i.home=TRUE, both=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","id_B","t_B","x.c","v_B"))
})

# ------------------------------------------------------------------------------
# names with dtjoin special case mult.DT but no mult

# special case mult.DT but no mult, same name
test_that("dtjoin mult.DT but no mult, names with same name join col, outer.DT", {
  data.table::setnames(DF_A,"id_A","id")
  data.table::setnames(DF_B,"id_B","id")
  result <- dtjoin(DF_B, DF_A, on="id", mult.DT="first", nomatch.DT=NA)
  expect_named(result, c("id","t_B","c","v_B","t_A","i.c","v_A"))
  data.table::setnames(DF_A,"id","id_A")
  data.table::setnames(DF_B,"id","id_B")
})

# special case mult.DT but no mult, with i.home
test_that("dtjoin mult.DT but no mult, names with i.home, outer.DT, preserve both", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", mult.DT="first", nomatch.DT=NA, i.home=TRUE, both=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","id_B","t_B","x.c","v_B"))
})

# special case mult.DT but no mult, with i.home, same name
test_that("dtjoin mult.DT but no mult, names with i.home, outer.DT", {
  data.table::setnames(DF_A,"id_A","id")
  data.table::setnames(DF_B,"id_B","id")
  result <- dtjoin(DF_B, DF_A, on="id", mult.DT="first", nomatch.DT=NA, i.home=TRUE)
  expect_named(result, c("id","t_A","c","v_A","t_B","x.c","v_B"))
  data.table::setnames(DF_A,"id","id_A")
  data.table::setnames(DF_B,"id","id_B")
})

# special case mult.DT but no mult, with i.home, same name, preserve both
test_that("dtjoin mult.DT but no mult, names with i.home, outer.DT, preserve both", {
  data.table::setnames(DF_A,"id_A","id")
  data.table::setnames(DF_B,"id_B","id")
  result <- dtjoin(DF_B, DF_A, on="id", mult.DT="first", nomatch.DT=NA, i.home=TRUE, both=TRUE)
  expect_named(result, c("id","t_A","c","v_A","x.id","t_B","x.c","v_B"))
  data.table::setnames(DF_A,"id","id_A")
  data.table::setnames(DF_B,"id","id_B")
})

# special case mult.DT but no mult, with i.home, different names
test_that("dtjoin mult.DT but no mult, names with i.home", {
  result <- dtjoin(DF_B, DF_A, on="id_B==id_A", mult.DT="first", nomatch.DT=NA, i.home=TRUE)
  expect_named(result, c("id_A","t_A","c","v_A","t_B","x.c","v_B"))
})
