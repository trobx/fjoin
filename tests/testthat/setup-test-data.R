PRINT_TEST_NAME <- FALSE
PRINT_TEST_OBJECTS <- FALSE
SHOW <- FALSE

# Need `[.data.table` for creating objects
.datatable.aware=TRUE

# data.table
n_A <- 7L
n_B <- 4L
set.seed(2) # 2
DT_A <- data.table::data.table(id_A=sample(letters[1:2], n_A, TRUE), t_A=sample(1:5, n_A, TRUE))
DT_B <- data.table::data.table(id_B=sample(letters[1:2], n_B, TRUE), t_B=sample(1:3, n_B, TRUE))
DT_A <- rbind(DT_A, data.table::data.table(id_A=c(NA,NA)), fill=TRUE)
DT_B <- rbind(data.table::data.table(id_B=c(NA,NA)), DT_B, fill=TRUE)
DT_A[, c := paste0("row A", formatC(.I, width = log10(.N) + 1, format = "d", flag = "0"))]
DT_B[, c := paste0("row B", formatC(.I, width = log10(.N) + 1, format = "d", flag = "0"))]
DT_A[, v_A := 0L]
DT_B[, v_B := 1L]

# Check gives different answers with two-way `mult` according to order of operations
tmp <- DT_B[DT_A, on=.(id_B == id_A, t_B < t_A), nomatch = NULL, .(id_A, id_B=x.id_B, t_A, t_B=x.t_B, c_A=i.c, c_B=c)]
tmp1 <- tmp[, first(.SD), by=c_A][, last(.SD), by=c_B][order(c_A, c_B), .(id_A, id_B, t_A, t_B, c_A, c_B)]
tmp2 <- tmp[, last(.SD), by=c_B][, first(.SD), by=c_A][order(c_A, c_B), .(id_A, id_B, t_A, t_B, c_A, c_B)]
stopifnot(!identical(tmp1,tmp2))
rm(tmp, tmp1, tmp2)

# data.frame
DF_A <- as.data.frame(DT_A)
DF_B <- as.data.frame(DT_B)

# tibble
TBL_A <- dplyr::as_tibble(DF_A)
TBL_B <- dplyr::as_tibble(DF_B)

# grouped tibble
GTBL_A <- dplyr::group_by(DF_A, v_A, t_A)
GTBL_B <- dplyr::group_by(DF_B, v_B, t_B)

# sf
SF_A <- data.table::copy(DT_A)
SF_A[, geom_active_A := sf::st_sfc(c(sapply(1:3, \(x) sf::st_sfc(sf::st_point(c(x,x)))), rep(NA,6)))]
SF_A[, geom_other_A := sf::st_sfc(c(sapply(4:6, \(x) sf::st_sfc(sf::st_point(c(x,x)))), rep(NA,6)))]
SF_A <- sf::st_as_sf(as.data.frame(SF_A, sf_column_name="geom_active_A", crs=4326))
SF_B <- data.table::copy(DT_B)
SF_B[, geom_active_B := sapply(7:12, \(x) sf::st_sfc(sf::st_point(c(x,x))))]
SF_B <- sf::st_as_sf(as.data.frame(SF_B, sf_column_name="geom_active_B", crs=4326))

# sf-tibble
SFTBL_A <- sf::st_as_sf(dplyr::as_tibble(SF_A), sf_column_name="geom_active_A", crs=4326)
SFTBL_B <- sf::st_as_sf(dplyr::as_tibble(SF_B), sf_column_name="geom_active_B", crs=4326)

# grouped sf-tibble
SFGTBL_A <- sf::st_as_sf(dplyr::group_by(SF_A, v_A, t_A), sf_column_name="geom_active_A", crs=4326)
SFGTBL_B <- sf::st_as_sf(dplyr::group_by(SF_B, v_B, t_B), sf_column_name="geom_active_B", crs=4326)



