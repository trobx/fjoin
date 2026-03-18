# Full join

Full join of `x` and `y`

## Usage

``` r
fjoin_full(
  x = NULL,
  y = NULL,
  on,
  match.na = FALSE,
  mult.x = "all",
  mult.y = "all",
  on.first = FALSE,
  order = "left",
  select = NULL,
  select.x = NULL,
  select.y = NULL,
  indicate = FALSE,
  prefix.y = "R.",
  both = FALSE,
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

- on.first:

  Whether to place the join columns first in the join result. Default
  `FALSE`.

- order:

  Whether the row order of the result should reflect `x` then `y`
  (`"left"`) or `y` then `x` (`"right"`). Default `"left"`.

- select, select.x, select.y:

  Character vectors of columns to be selected from either input if
  present (`select`) or specifically from one or other of them (e.g.
  `select.x`). `NULL` (the default) selects all columns. Use `""` or
  `NA` to select no columns. Join columns are always selected. See
  Details.

- indicate:

  Whether to add a column `".join"` at the front of the result, with
  values `1L` if from `x` only, `2L` if from `y` only, and `3L` if
  joined from both tables (c.f. `_merge` in Stata). Default `FALSE`.

- prefix.y:

  A prefix to attach to column names in `y` that are the same as a
  column name in `x`. Default `"R."`.

- both:

  Whether to include `y`'s equality join column(s) separately in the
  output, instead of combining them with `x`'s. Default `FALSE`. Note
  that non-equality join columns from `x` are always included
  separately.

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

### Input and output class

Each input can be any object with class `data.frame`, or a plain `list`
of same-length vectors.

The output class depends on `x` as follows:

- a `data.table` if `x` is a pure `data.table`

- a tibble if it is a tibble (and a grouped tibble if it has class
  `grouped_df`)

- an `sf` if it is an `sf` with its active geometry selected in the
  output

- a plain `data.frame` in all other cases

The following attributes are carried through and refreshed: `data.table`
key, tibble `groups`, `sf` `agr` (and `bbox` etc. of all individual
`sfc`-class columns regardless of output class). See below for
specifics.

### Specifying join conditions with `on`

`on` is a required argument. For a natural join (a join by equality on
all same-named column pairs), you must specify `on = NA`; you can't just
omit `on` as in other packages. This is to prevent a natural join being
specified by mistake, which may then go unnoticed.

### Using `select`, `select.x`, and `select.y`

Used on its own, `select` keeps the join columns plus the specified
non-join columns from both inputs if present.

If `select.x` is provided (and similarly for `select.y`) then:

- if `select` is also specified, non-join columns of `x` named in either
  `select` or `select.x` are included

- if `select` is not specified, only non-join columns named in
  `select.x` are included from `x`. Thus e.g. `select.x = ""` excludes
  all of `x`'s non-join columns.

Non-existent column names are ignored without warning.

### Column order

When `select` is specified but `select.x` and `select.y` are not, the
output consists of all join columns followed by the selected non-join
columns from either input in the order given in `select`.

In all other cases:

- columns from `x` come before columns from `y`

- within each group of columns, non-join columns are in the order given
  by `select.x`/`select.y`, or in their original data order if no
  selection is provided

- if `on.first` is `TRUE`, join columns from both inputs are moved to
  the front of the overall output.

### Using `mult.x` and `mult.y`

See the Examples for an application of using `mult.x` and `mult.y`
together. Note that `mult.y` is applied after `mult.x` except with
`order = "right"`.

### Displaying code and 'mock joins'

The option of displaying the join code with `show = TRUE` or by passing
null inputs is aimed at data.table users wanting to use the package as a
cookbook of recipes for adaptation. If `x` and `y` are both `NULL`,
template code is displayed based on join column names implied by `on`,
plus sample non-join column names. `select` arguments are ignored in
this case.

The code displayed is for the join operation after casting the inputs as
`data.table`s if necessary, and before casting the result as a tibble
and/or `sf` if applicable. Note that fjoin departs from the usual
`j = list()` idiom in order to avoid a deep copy of the output made by
`as.data.table.list`. (Likewise, internally it takes only shallow copies
of columns when casting inputs or outputs to different classes.)

### tibble `groups`

If `x` is a grouped tibble (class `grouped_df`), the output is grouped
by the grouping columns that are selected in the result.

### data.table `key`s

If the output is a `data.table`, it inherits a `key` as follows:

- `fjoin_inner` or `fjoin_left` with `order = "left"` (default): `x`'s
  `key` if present

- `fjoin_inner` or `fjoin_right` with `order = "right"`: `y`'s `key` if
  present

If not all of the key columns are selected in the result, the leading
subset is used.

### sf objects and `sfc`-class columns

Joins between two `sf` objects are supported. The active geometry and
relation-to-geometry attribute `agr` are determined by `x`. All
`sfc`-class columns in the output are refreshed after joining (using
[`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html) with
`recompute_bbox = TRUE`); this is true regardless of whether or not the
inputs and output are `sf`s.

## See also

See the package-level documentation
[`fjoin`](https://trobx.github.io/fjoin/reference/fjoin-package.md) for
related functions.

## Examples

``` r
# ---------------------------------------------------------------------------
# True joins (inner/left/right/full): basic usage
# ---------------------------------------------------------------------------

# data frames
x <- data.table::fread(data.table = FALSE, input = "
  country  pop_m
Australia   27.2
   Brazil  212.0
     Chad    3.0
")

y <- data.table::fread(data.table = FALSE, input = "
  country forest_pc
   Brazil      59.1
     Chad       3.2
  Denmark      15.8
")

# ---------------------------------------------------------------------------
# `indicate = TRUE` adds a front column ".join" indicating whether a row is
# from `x` only (1L), from `y` only (2L), or joined from both (3L)

fjoin_full(x, y, on = "country", indicate = TRUE)
#>   .join   country pop_m forest_pc
#> 1     1 Australia  27.2        NA
#> 2     3    Brazil 212.0      59.1
#> 3     3      Chad   3.0       3.2
#> 4     2   Denmark    NA      15.8
fjoin_left(x, y, on = "country", indicate = TRUE)
#>   .join   country pop_m forest_pc
#> 1     1 Australia  27.2        NA
#> 2     3    Brazil 212.0      59.1
#> 3     3      Chad   3.0       3.2
fjoin_right(x, y, on = "country", indicate = TRUE)
#>   .join country pop_m forest_pc
#> 1     3  Brazil   212      59.1
#> 2     3    Chad     3       3.2
#> 3     2 Denmark    NA      15.8
fjoin_inner(x, y, on = "country", indicate = TRUE)
#>   .join country pop_m forest_pc
#> 1     3  Brazil   212      59.1
#> 2     3    Chad     3       3.2

# ---------------------------------------------------------------------------
# Core options and arguments (in a 1:1 equality join with fjoin_full())
# ---------------------------------------------------------------------------

# data frames
dfQ <- data.table::fread(data.table = FALSE, quote ="'", input = "
id quantity                   notes other_cols
 2        5                      ''        ...
 1        6                      ''        ...
 3        7                      ''        ...
NA        8  'oranges (not listed)'        ...
")

dfP <- data.table::fread(data.table = FALSE, input = "
id     item price other_cols
NA   apples    10        ...
 3  bananas    20        ...
 2 cherries    30        ...
 1    dates    40        ...
 ")

# ---------------------------------------------------------------------------

# (1) basic syntax
# cf. dplyr: full_join(dfQ, dfP, join_by(id), na.matches = "never")
fjoin_full(dfQ, dfP, on = "id")
#>   id quantity                notes other_cols     item price R.other_cols
#> 1  2        5                             ... cherries    30          ...
#> 2  1        6                             ...    dates    40          ...
#> 3  3        7                             ...  bananas    20          ...
#> 4 NA        8 oranges (not listed)        ...     <NA>    NA         <NA>
#> 5 NA       NA                 <NA>       <NA>   apples    10          ...

# (an aside) equality matches on NA if you insist
fjoin_full(dfQ, dfP, on = "id", select = c("item", "price", "quantity", "notes"), match.na = TRUE)
#>   id     item price quantity                notes
#> 1  2 cherries    30        5                     
#> 2  1    dates    40        6                     
#> 3  3  bananas    20        7                     
#> 4 NA   apples    10        8 oranges (not listed)

# (2) join-select in one line
fjoin_full(dfQ, dfP, on = "id", select = c("item", "price", "quantity"))
#>   id     item price quantity
#> 1  2 cherries    30        5
#> 2  1    dates    40        6
#> 3  3  bananas    20        7
#> 4 NA     <NA>    NA        8
#> 5 NA   apples    10       NA

# equivalent operation in dplyr
# x <- dfQ |> select(id, quantity)
# y <- dfP |> select(id, item, price)
# full_join(x, y, join_by(id), na.matches = "never") |>
#   select(id, item, price, quantity)

# (3) indicator column (in Stata since 1984)
fjoin_full(
  dfQ,
  dfP,
  on = "id",
  select = c("item", "price", "quantity"),
  indicate = TRUE
)
#>   .join id     item price quantity
#> 1     3  2 cherries    30        5
#> 2     3  1    dates    40        6
#> 3     3  3  bananas    20        7
#> 4     1 NA     <NA>    NA        8
#> 5     2 NA   apples    10       NA

# (4) order rows by y then x
fjoin_full(
  dfQ,
  dfP,
  on = "id",
  select = c("item", "price", "quantity"),
  indicate = TRUE,
  order = "right"
)
#>   .join id     item price quantity
#> 1     2 NA   apples    10       NA
#> 2     3  3  bananas    20        7
#> 3     3  2 cherries    30        5
#> 4     3  1    dates    40        6
#> 5     1 NA     <NA>    NA        8

# (5) display code instead
fjoin_full(
  dfQ,
  dfP,
  on = "id",
  select = c("item", "price", "quantity"),
  indicate = TRUE,
  order = "right",
  do = FALSE
)
#> .DT : x = dfQ (cast as data.table)
#> .i  : y = dfP (cast as data.table)
#> Join: setDF(with(list(fjoin.temp = setDT(.DT[, fjoin.which.DT := .I][, na.omit(.SD, cols = "id"), .SDcols = c("id", "quantity", "fjoin.which.DT")][, fjoin.ind.DT := TRUE][.i, on = "id", data.frame(.join = fifelse(is.na(fjoin.ind.DT), 2L, 3L), id, item, price, quantity, fjoin.which.DT), allow.cartesian = TRUE])), rbind(fjoin.temp, setDT(.DT[!fjoin.temp$fjoin.which.DT, data.frame(id, quantity, fjoin.which.DT, .join = rep(1L, .N))]), fill = TRUE))[, fjoin.which.DT := NULL])[]
#> 

# ---------------------------------------------------------------------------
# M:M inequality join reduced to 1:1 using `mult.x` and `mult.y`
# ---------------------------------------------------------------------------

# data.table (`mult`) and dplyr (`multiple`) have options for reducing the
# cardinality on one side of the join from many ("all") to one ("first" or
# "last"). fjoin (`mult.x`, `mult.y`) permits this on either side of the
# join, or on both sides at once.

# This example (using `fjoin_left()`) shows an application to temporally
# ordered data frames of "events" and "reactions".

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

# (1) for each event, all subsequent reactions (M:M)
fjoin_left(
  events,
  reactions,
  on = c("event_ts < reaction_ts"),
)
#>   event_id event_ts reaction_id reaction_ts
#> 1        1       10           1          30
#> 2        1       10           2          50
#> 3        1       10           3          60
#> 4        2       20           1          30
#> 5        2       20           2          50
#> 6        2       20           3          60
#> 7        3       40           2          50
#> 8        3       40           3          60

# (2) for each event, the next reaction (1:M)
fjoin_left(
  events,
  reactions,
  on = c("event_ts < reaction_ts"),
  mult.x = "first"
)
#>   event_id event_ts reaction_id reaction_ts
#> 1        1       10           1          30
#> 2        2       20           1          30
#> 3        3       40           2          50

# (3) for each event, the next reaction, provided there was no intervening event (1:1)
fjoin_left(
  events,
  reactions,
  on = c("event_ts < reaction_ts"),
  mult.x = "first",
  mult.y = "last"
)
#>   event_id event_ts reaction_id reaction_ts
#> 1        1       10          NA          NA
#> 2        2       20           1          30
#> 3        3       40           2          50

# ---------------------------------------------------------------------------
# Natural join
# ---------------------------------------------------------------------------
fjoin_inner(x, y, on = NA) # note `NA` not `NULL`/omitted
#>   country pop_m forest_pc
#> 1  Brazil   212      59.1
#> 2    Chad     3       3.2
try(fjoin_inner(x, y)) # to prevent accidental natural joins
#> Error in fjoin_inner(x, y) : argument "on" is missing, with no default

# ---------------------------------------------------------------------------
# Mock join (code "ghostwriter" for data.table users)
# ---------------------------------------------------------------------------
fjoin_inner(on = c("id"))
#> .DT : y (unnamed)
#> .i  : x (unnamed)
#> Join: setDT(na.omit(.DT, cols = "id")[.i, on = "id", nomatch = NULL, data.frame(id = i.id, col_i, col_c = i.col_c, col_DT, R.col_c = col_c), allow.cartesian = TRUE])[]
#> 
```
