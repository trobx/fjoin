## ----setup, include=FALSE-----------------------------------------------------
pkg <- function(x) {
  # style "pkgname" is defined in _pkgdown.yml 
  sprintf('<span class="pkgname">%s</span>', x)
}


## -----------------------------------------------------------------------------
library(fjoin)
read_df <- function(x) data.table::fread(x, quote = "'", data.table = FALSE)


## -----------------------------------------------------------------------------
dfP <- read_df("
id     item price other_cols
NA   apples    10        ...
 3  bananas    20        ...
 2 cherries    30        ...
 1    dates    40        ...
")

dfQ <- read_df("
id quantity       note  other_cols
 2        5         ''         ...
 1        6         ''         ...
 3        7         ''         ...
NA        8  'oranges'         ...
")


## -----------------------------------------------------------------------------
fjoin_full(dfQ, dfP, on = "id")


## -----------------------------------------------------------------------------
fjoin_full(dfQ, dfP, on = "id", select = c("item", "price", "quantity"))


## ----eval = FALSE-------------------------------------------------------------
# x <- dfQ |> select(id, quantity)
# y <- dfP |> select(id, item, price)
# full_join(x, y, join_by(id), na.matches = "never") |>
#   select(id, item, price, quantity)


## -----------------------------------------------------------------------------
fjoin_full(
  dfQ,
  dfP,
  on = "id",
  select = c("item", "price", "quantity"),
  indicate = TRUE
)


## -----------------------------------------------------------------------------
fjoin_full(
  dfQ,
  dfP,
  on = "id",
  select = c("item", "price", "quantity"),
  indicate = TRUE,
  order = "right"
)


## -----------------------------------------------------------------------------
fjoin_full(
  dfQ,
  dfP,
  on = "id",
  select = c("item", "price", "quantity"),
  indicate = TRUE,
  order = "right",
  do = FALSE
)


## -----------------------------------------------------------------------------
events <- read_df("
event_id event_ts
       1       10
       2       20
       3       40
")

reactions <- read_df("
reaction_id reaction_ts
          1          30
          2          50
          3          60
")


## -----------------------------------------------------------------------------
fjoin_left(
  events,
  reactions,
  on = c("event_ts < reaction_ts")
)


## -----------------------------------------------------------------------------
fjoin_left(
  events,
  reactions,
  on = c("event_ts < reaction_ts"),
  mult.x = "first"
)


## -----------------------------------------------------------------------------
fjoin_left(
  events,
  reactions,
  on = c("event_ts < reaction_ts"),
  mult.x = "first",
  mult.y = "last"
)


## -----------------------------------------------------------------------------
df_x <- data.frame(id_x = 1:3, row_x = paste0("x", 1:3))
df_y <- data.frame(id_y = rep(4:2, each = 2L), row_y = paste0("y", 1:6))


## -----------------------------------------------------------------------------
# (1) fjoin_left(df_x, df_y, on = "id_x == id_y", mult.x = "first")
dtjoin(
  df_y,
  df_x,
  on = "id_y == id_x",
  mult = "first",
  i.home = TRUE,
  prefix = "R.",
  show = TRUE
)

# (2) fjoin_left(df_x, df_y, on = "id_x == id_y", mult.x = "first", order = "right")
dtjoin(
  df_x,
  df_y,
  on = "id_x == id_y",
  mult.DT = "first",
  nomatch = NULL,
  nomatch.DT = NA,
  prefix = "R.",
  show = TRUE
)


## ----echo=FALSE---------------------------------------------------------------
pal <- c(
  "#B8860B",
  "grey60",
  "#66C2A5",
  "#1B9E77"
)

factor_in_order <- function(x, ...) factor(x, levels = unique(x), ...)

pl <- function(x) {
  library(ggplot2)
  ggplot(x, aes(x = soln, y = ifelse(is.na(median_secs), 0, median_secs), fill = soln)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = sprintf("%.1f", median_secs)), vjust = -0.3, size = 3) +
    facet_grid(factor_in_order(style) ~ factor_in_order(xyargs), switch = "y") +
    scale_fill_manual(values = pal) +
    scale_y_continuous(limits = c(0, k)) +
    #theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      #strip.placement.y = "left",
      strip.text.y.left = element_text(angle = 0),
    ) +
    labs(
      x = NULL,
      # title = x[, unique(description)],
      subtitle = sprintf("median time in secs, %s runs per join",x[, unique(na.omit(N))])
      )
}

dat <- structure(list(style = c("Inner join", "Inner join", "Inner join", 
"Inner join", "Inner join", "Inner join", "Inner join", "Inner join", 
"Left join", "Left join", "Left join", "Left join", "Left join", 
"Left join", "Left join", "Left join", "Right join (ordered by left)", 
"Right join (ordered by left)", "Right join (ordered by left)", 
"Right join (ordered by left)", "Right join (ordered by left)", 
"Right join (ordered by left)", "Right join (ordered by left)", 
"Right join (ordered by left)", "Right join (ordered by right)", 
"Right join (ordered by right)", "Right join (ordered by right)", 
"Right join (ordered by right)", "Right join (ordered by right)", 
"Right join (ordered by right)", "Right join (ordered by right)", 
"Right join (ordered by right)", "Full join", "Full join", "Full join", 
"Full join", "Full join", "Full join", "Full join", "Full join"
), xyargs = c("x small, y big", "x small, y big", "x small, y big", 
"x small, y big", "x big, y small", "x big, y small", "x big, y small", 
"x big, y small", "x small, y big", "x small, y big", "x small, y big", 
"x small, y big", "x big, y small", "x big, y small", "x big, y small", 
"x big, y small", "x small, y big", "x small, y big", "x small, y big", 
"x small, y big", "x big, y small", "x big, y small", "x big, y small", 
"x big, y small", "x small, y big", "x small, y big", "x small, y big", 
"x small, y big", "x big, y small", "x big, y small", "x big, y small", 
"x big, y small", "x small, y big", "x small, y big", "x small, y big", 
"x small, y big", "x big, y small", "x big, y small", "x big, y small", 
"x big, y small"), soln = structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 
4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 
4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 
4L), levels = c("fjoin", "merge", "dplyr", "collapse"), class = "factor"), 
    description = c("No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered", 
    "No missing values, tables unordered", "No missing values, tables unordered"
    ), N = c(5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
    5L, 5L, 5L, 5L, 5L, 5L, 5L, NA, 5L, 5L, 5L, NA, 5L, NA, NA, 
    5L, 5L, NA, NA, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L), median_secs = c(6.1902577, 
    6.1713217, 19.9907639, 19.7675571, 6.1000215, 6.1203305, 
    13.3964799, 7.0342887, 6.2597127, 6.2432609, 19.9681676, 
    20.1874484, 5.6448957, 5.6632873, 13.6564839, 5.5813086, 
    10.4640203, 10.8960488, 22.8774932, NA, 8.5460365, 12.2154671, 
    14.2776151, NA, 5.9763782, NA, NA, 5.6241829, 5.7766581, 
    NA, NA, 20.3630658, 10.6481448, 10.864006, 23.0229502, 23.5587724, 
    9.8157773, 13.6417876, 15.7693984, 9.0004465)), row.names = c(NA, 
-40L), class = c("data.frame", "data.table"))



## ----echo=FALSE---------------------------------------------------------------
library(data.table)
k <- 30 
setDT(dat)
pl(dat)


## ----message=FALSE------------------------------------------------------------
library(dplyr)
dfQ <- as_tibble(dfQ)

dfQ |>
  fjoin::fjoin_full(dfP,
    on = "id",
    select = c("item", "price", "quantity"),
    order = "right",
    indicate = TRUE
  ) |>
  mutate(
    quantity = if_else(.join == 2L, 0L, quantity),
    revenue  = price * quantity
  )


## -----------------------------------------------------------------------------
countries <- read_df("
country_id   country_name                          country_shape
         1     'Country A'  'POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))'     
         2     'Country B'  'POLYGON ((1 1, 2 1, 2 2, 1 2, 1 1))'    
         3     'Country C'  'POLYGON ((2 2, 3 2, 3 3, 2 3, 2 2))'    
") |> sf::st_as_sf(wkt = "country_shape", crs = 4326)

capitals <- read_df("
country_id  capital_name        capital_loc
         2       'City B'  'POINT (1.5 1.5)'    
         3       'City C'  'POINT (2.5 2.5)'    
         4       'City D'  'POINT (3.5 3.5)'    
") |> sf::st_as_sf(wkt = "capital_loc", crs = 4326)


## -----------------------------------------------------------------------------
fjoin_inner(countries, capitals, on = "country_id")


## ----message = TRUE-----------------------------------------------------------
try(dplyr::inner_join(countries, capitals, by = "country_id"))


## ----message=TRUE-------------------------------------------------------------
fjoin_left(as.data.frame(countries), as.data.frame(capitals), on = "country_id")$capital_loc


## -----------------------------------------------------------------------------
library(data.table)
dtQ <- as.data.table(dfQ)
dtP <- as.data.table(dfP)

dtP[, revenue := 
      price * fjoin_left(
        dtP,
        dtQ,
        on = "id",
        select = c("quantity"),
        indicate = TRUE
      )[.join == 1L, quantity := 0L]$quantity][]


## -----------------------------------------------------------------------------
dt1 <- data.table(t=c(5L,25L,45L))
dt2 <- data.table(t_start=c(1L,21L), t_end=c(10L,30L))


## -----------------------------------------------------------------------------
dtjoin(dt2, dt1, on=c("t_start <= t", "t_end >= t"), show = TRUE)


## ----message=FALSE------------------------------------------------------------
dt2[dt1, on=.(t_start <= t, t_end >= t)]


## ----message=FALSE------------------------------------------------------------
n <- 1e6L; ncol_dt <- 2L; ncol_df <- 10L
dt <- data.table(id = rep(1:n, each = 5L), matrix(runif(n * ncol_dt), ncol = ncol_dt))
df <- data.frame(id = 1:n, matrix(runif(n * ncol_df), ncol = ncol_df))

bench::mark(
  data.table = dt[df, on = .(id), .(id, V1, V2, X1, X3, X5, X7, X9)],
  fjoin      = dtjoin(dt, df, on = "id", select.i = c("X1", "X3", "X5", "X7", "X9")),
  iterations = 3,
  check      = TRUE
) |> summary() |> subset(select = c("expression", "n_itr", "median",  "mem_alloc"))

