# Right anti-join

The anti-join of `y` in a join of `x` and `y`, i.e. the rows of `y` that
do not join.

## Usage

``` r
fjoin_right_anti(
  x = NULL,
  y = NULL,
  on,
  match.na = FALSE,
  mult.x = "all",
  mult.y = "all",
  select = NULL,
  do = !(is.null(x) && is.null(y)),
  show = !do
)
```

## Arguments

- x, y:

  `data.frame`-like objects (plain, `data.table`, tibble, `sf`, `list`,
  etc.) or else both omitted for a mock join statement with no data. See
  Details.

- on:

  A character vector of join predicates, e.g.
  `c("id", "col_x == col_y", "date > date", "cost <= budget")`, or else
  `NA` for a natural join (an equality join on all same-named columns).

- match.na:

  Whether to allow equality matches between `NA`s or `NaN`s. Default
  `FALSE`.

- mult.x, mult.y:

  When a row of `x` (`y`) has multiple matching rows in `y` (`x`), which
  to accept: `"all"` (the default), `"first"`, or `"last"`. May be used
  in combination.

- select:

  Character vector of columns to be selected from `y`. `NULL` (the
  default) selects all columns. Join columns are always selected.

- do:

  Whether to execute the join. If `FALSE`, `show` is set to `TRUE` and
  the data.table code for the join is printed to the console instead.
  Default is `TRUE` unless `x` and `y` are both omitted/`NULL`, in which
  case a mock join statement is produced. See Details.

- show:

  Whether to print the data.table code for the join to the console.
  Default is the opposite of `do`. If `x` and `y` are both
  omitted/`NULL`, mock join code is displayed.

## Value

A `data.frame`, `data.table`, (grouped) tibble, `sf`, or `sf`-tibble, or
else `NULL` if `do` is `FALSE`. See Details.

## Details

Details are as for e.g.
[`fjoin_inner`](https://trobx.github.io/fjoin/reference/fjoin_inner.md)
except for arguments controlling the order and prefixing of output
columns, which do not apply. Output class is determined by `y`.

## See also

See the package-level documentation
[`fjoin`](https://trobx.github.io/fjoin/reference/fjoin-package.md) for
related functions.

## Examples

``` r
# ---------------------------------------------------------------------------
# Semi- and anti-joins: basic usage
# ---------------------------------------------------------------------------

# data frames
x <- data.table::fread(data.table = FALSE, input = "
country   pop_m
Australia  27.2
Brazil    212.0
Chad        3.0
")

y <- data.table::fread(data.table = FALSE, input = "
country forest_pc
Brazil       59.1
Chad          3.2
Denmark      15.8
")

# full join with `indicate = TRUE` for comparison
fjoin_full(x, y, on = "country", indicate = TRUE)
#>   .join   country pop_m forest_pc
#> 1     1 Australia  27.2        NA
#> 2     3    Brazil 212.0      59.1
#> 3     3      Chad   3.0       3.2
#> 4     2   Denmark    NA      15.8

fjoin_semi(x, y, on = "country")
#>   country pop_m
#> 1  Brazil   212
#> 2    Chad     3
fjoin_anti(x, y, on = "country")
#>     country pop_m
#> 1 Australia  27.2
fjoin_right_semi(x, y, on = "country")
#>   country forest_pc
#> 1  Brazil      59.1
#> 2    Chad       3.2
fjoin_right_anti(x, y, on = "country")
#>   country forest_pc
#> 1 Denmark      15.8

# ---------------------------------------------------------------------------
# `mult.x` and `mult.y` support
# ---------------------------------------------------------------------------

# data frames
events <- data.table::fread(data.table = FALSE, input = "
event_id event_ts
       1       10
       2       20
       3       40
")

reactions <- data.table::fread(data.table = FALSE, input = "
reaction_id reaction_ts
          1          30
          2          50
          3          60
")
# ---------------------------------------------------------------------------

# for each event, the next reaction, provided there was no intervening event (1:1)
fjoin_full(
  events,
  reactions,
  on = c("event_ts < reaction_ts"),
  mult.x = "first",
  mult.y = "last",
  indicate = TRUE
)
#>   .join event_id event_ts reaction_id reaction_ts
#> 1     1        1       10          NA          NA
#> 2     3        2       20           1          30
#> 3     3        3       40           2          50
#> 4     2       NA       NA           3          60

fjoin_semi(
  events,
  reactions,
  on = c("event_ts < reaction_ts"),
  mult.x = "first",
  mult.y = "last"
)
#>   event_id event_ts
#> 1        2       20
#> 2        3       40

fjoin_anti(
  events,
  reactions,
  on = c("event_ts < reaction_ts"),
  mult.x = "first",
  mult.y = "last"
)
#>   event_id event_ts
#> 1        1       10

# ---------------------------------------------------------------------------
# Natural join
# ---------------------------------------------------------------------------
fjoin_semi(x, y, on = NA)
#>   country pop_m
#> 1  Brazil   212
#> 2    Chad     3
fjoin_anti(x, y, on = NA)
#>     country pop_m
#> 1 Australia  27.2

# ---------------------------------------------------------------------------
# Mock join
# ---------------------------------------------------------------------------
fjoin_semi(on="id")
#> .DT : x (unnamed)
#> .i  : y (unnamed)
#> Join: na.omit(.DT, cols = "id")[id %in% .i$id]
#> 
fjoin_semi(on=c("id", "date"))
#> .DT : x (unnamed)
#> .i  : y (unnamed)
#> Join: setDT(.i[, na.omit(.SD), .SDcols = c("id", "date")][.DT, on = c("id", "date"), nomatch = NULL, mult = "first", data.frame(id = i.id, date = i.date, col_DT, col_c = i.col_c)])[]
#> 
fjoin_semi(on=c("id"), mult.y = "last")
#> .DT : x (unnamed)
#> .i  : y (unnamed)
#> Join: .DT[fsort(as.numeric(unique(.DT[.i[, na.omit(.SD), .SDcols = "id"], on = "id", nomatch = NULL, mult = "last", which = TRUE])))]
#> 
```
