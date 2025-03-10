library(data.table)

# n_A <- 7L
# n_B <- 4L
# set.seed(2)
# A <- data.table::data.table(id_A=sample(letters[1:2], n_A, TRUE), t_A=sample(1:5, n_A, TRUE))
# B <- data.table::data.table(id_B=sample(letters[1:2], n_B, TRUE), t_B=sample(1:3, n_B, TRUE))
# A <- rbind(A, data.table::data.table(id_A=c(NA,NA)), fill=TRUE)
# B <- rbind(data.table::data.table(id_B=c(NA,NA)), B, fill=TRUE)
# A[, c := paste0("I'm row A", formatC(.I, width = log10(.N) + 1, format = "d", flag = "0"))]
# B[, c := paste0("I'm row B", formatC(.I, width = log10(.N) + 1, format = "d", flag = "0"))]
# A
# B


# rename antiDT (nomatch.DT and i.main) with
  # equality, diff names
  # non equi or preserve, same names

# general case:
  # not i.main, non-equi/preserve
  # i.main, non-equi or preserve

# special case (mult.DT but no mult)
  # not i.main
    # equality, same names
    # non equi or preserve, same names
  # i.main
    # equality, same names
    # non equi or preserve, same names


