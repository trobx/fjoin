# Join data frame-like objects using an extended `DT[i]`-style interface to data.table

Write (and optionally run) data.table code for a join using a
generalisation of `DT[i]` syntax with extended arguments and enhanced
behaviour. Accepts any `data.frame`-like inputs (not only
`data.table`s), permits left, right, inner, and full joins, prevents
unwanted matches on `NA` and `NaN` by default, does not garble join
columns in non-equality joins, allows `mult` on both sides of the join,
creates an optional join indicator column, allows specifying which
columns to select from each input, and provides convenience options to
control column order and prefixing.

If run, the join returns a `data.frame`, `data.table`, tibble, `sf`, or
`sf`-tibble according to context. The generated data.table code can be
printed to the console instead of (or as well as) being executed. This
feature extends to *mock joins*, where no inputs are provided, and
template code is produced.

`dtjoin` is the workhorse function for
[`fjoin_inner`](https://trobx.github.io/fjoin/reference/fjoin_inner.md),
[`fjoin_left`](https://trobx.github.io/fjoin/reference/fjoin_left.md),
[`fjoin_right`](https://trobx.github.io/fjoin/reference/fjoin_right.md),
and
[`fjoin_full`](https://trobx.github.io/fjoin/reference/fjoin_full.md),
which are wrappers providing a more conventional interface for join
operations. These functions are recommended over `dtjoin` for most users
and cases.

## Usage

``` r
dtjoin(
  .DT = NULL,
  .i = NULL,
  on,
  match.na = FALSE,
  mult = "all",
  mult.DT = "all",
  nomatch = NA,
  nomatch.DT = NULL,
  indicate = FALSE,
  select = NULL,
  select.DT = NULL,
  select.i = NULL,
  both = FALSE,
  on.first = FALSE,
  i.home = FALSE,
  i.first = i.home,
  prefix = if (i.home) "x." else "i.",
  i.class = i.home,
  do = !(is.null(.DT) && is.null(.i)),
  show = !do,
  verbose = FALSE,
  ...
)
```

## Arguments

- .DT, .i:

  `data.frame`-like objects (plain, `data.table`, tibble, `sf`, `list`,
  etc.), or else both omitted for a mock join statement with no data.

- on:

  A character vector of join predicates, e.g.
  `c("id", "col_DT == col_i", "date < date", "cost <= budget")`, or else
  `NA` for a natural join (an equality join on all same-named columns).

- match.na:

  If `TRUE`, allow equality matches between `NA`s or `NaN`s. Default
  `FALSE`.

- mult:

  (as in `[.data.table`) When a row of `.i` has multiple matching rows
  in `.DT`, which to accept. One of `"all"` (the default), `"first"`, or
  `"last"`.

- mult.DT:

  Like `mult`, but with the roles of `.DT` and `.i` reversed, i.e. when
  a row of `.DT` has multiple matching rows in `.i`, which to accept
  (default `"all"`). Can be combined with `mult`. See Details.

- nomatch:

  (as in `[.data.table`) Either `NA` (the default) to retain rows of
  `.i` with no match in `.DT`, or `NULL` to exclude them.

- nomatch.DT:

  Like `nomatch` but with the roles of `.DT` and `.i` reversed, and a
  different default: either `NA` to append rows of `.DT` with no match
  in `.i`, or `NULL` (the default) to leave them out.

- indicate:

  Whether to add a column `".join"` at the front of the result, with
  values `1L` if from the "home" table only, `2L` if from the "foreign"
  table only, and `3L` if joined from both tables (c.f. `_merge` in
  Stata). Default `FALSE`.

- select, select.DT, select.i:

  Character vectors of columns to be selected from either input if
  present (`select`) or specifically from one or other (`select.DT`,
  `select.i`). `NULL` (the default) selects all columns. Use `""` or
  `NA` to select no columns. Join columns are always selected. See
  Details.

- both:

  Whether to include equality join columns from the "foreign" table
  separately in the output, instead of combining them with those from
  the "home" table. Default `FALSE`. Note that non-equality join columns
  from the foreign table are always included separately.

- on.first:

  Whether to place the join columns from both inputs first in the join
  result. Default `FALSE`.

- i.home:

  Whether to treat `.i` as the "home" table and `.DT` as the "foreign"
  table for column prefixing and `indicate`. Default `FALSE`, i.e. `.DT`
  is the "home" table, as in `[.data.table`.

- i.first:

  Whether to place `.i`'s columns before `.DT`'s in the join result. The
  default is to use the value of `i.home`, i.e. bring `.i`'s columns to
  the front if `.i` is the "home" table.

- prefix:

  A prefix to attach to column names in the "foreign" table that are the
  same as a column name in the "home" table. The default is `"i."` if
  the "foreign" table is `.i` (`i.home` is `FALSE`) and `"x."` if it is
  `.DT` (`i.home` is `TRUE`).

- i.class:

  Whether the `class` of the output should be based on `.i` instead of
  `.DT`. The default follows `i.home` (default `FALSE`). See Details for
  how output `class` and other attributes are set.

- do:

  Whether to execute the join. Default is `TRUE` unless `.DT` and `.i`
  are both omitted/`NULL`, in which case a mock join statement is
  produced.

- show:

  Whether to print the code for the join to the console. Default is the
  opposite of `do`. If `.DT` and `.i` are both omitted/`NULL`, mock join
  code is displayed.

- verbose:

  (passed to `[.data.table`) Whether data.table should print information
  to the console during execution. Default `FALSE`.

- ...:

  Further arguments (for internal use).

## Value

A `data.frame`, `data.table`, (grouped) tibble, `sf`, or `sf`-tibble, or
else `NULL` if `do` is `FALSE`. See Details.

## Details

### Input and output class

Each input can be any object with class `data.frame`, or a plain `list`
of same-length vectors.

The output class depends on `.DT` by default (but `.i` with
`i.class = TRUE`) and is as follows:

- a `data.table` if the input is a pure `data.table`

- a tibble if it is a tibble (and a grouped tibble if it has class
  `grouped_df`)

- an `sf` if it is an `sf` with its active geometry selected in the join

- a plain `data.frame` in all other cases

The following attributes are carried through and refreshed: `data.table`
key, tibble `groups`, `sf` `agr` (and `bbox` etc. of all individual
`sfc`-class columns regardless of output class). See below for
specifics. Other classes and attributes are not carried through.

### Specifying join conditions with `on`

`on` is a required argument. For a natural join (a join by equality on
all same-named column pairs), you must specify `on = NA`; you can't just
omit `on` as in other packages. This is to prevent a natural join being
specified by mistake, which may then go unnoticed.

### Using `select`, `select.DT`, and `select.i`

Used on its own, `select` keeps the join columns plus the specified
non-join columns from both inputs if present.

If `select.DT` is provided (and similarly for `select.i`) then:

- if `select` is also specified, non-join columns of `.DT` named in
  either `select` or `select.DT` are included

- if `select` is not specified, only non-join columns named in
  `select.DT` are included from `.DT`. Thus e.g. `select.DT = ""`
  excludes all of `.DT`'s non-join columns.

Non-existent column names are ignored without warning.

### Column order

When `select` is specified but `select.DT` and `select.i` are not, the
output consists of all join columns followed by the selected non-join
columns from either input in the order given in `select`.

In all other cases:

- columns from `.DT` come before columns from `.i` by default (but vice
  versa if `i.first` is `TRUE`)

- within each group of columns, non-join columns are in the order given
  by `select.DT`/`select.i`, or in their original data order if no
  selection is provided

- if `on.first` is `TRUE`, join columns from both inputs are moved to
  the front of the overall output.

### Using `mult` and `mult.DT`

If both of these arguments are not the default `"all"`, `mult` is
applied first (typically by passing directly to `[.data.table`) and
`mult.DT` is applied subsequently to eliminate all but the first or last
occurrence of each row of `.DT` from the inner part of the join,
producing a 1:1 result. This order of operations can affect the identity
of the rows in the inner join.

### Displaying code and 'mock joins'

The option of displaying the join code with `show = TRUE` or by passing
null inputs is aimed at data.table users wanting to use the package as a
cookbook of recipes for adaptation. If `.DT` and `.i` are both `NULL`,
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

If the relevant input is a grouped tibble (class `grouped_df`), the
output is grouped by the grouping columns that are selected in the
result.

### data.table `key`s

If `.i` is a `key`ed `data.table` and the output is also a `data.table`,
it inherits `.i`'s key provided `nomatch.DT` is `NULL` (i.e. the
non-matching rows of `.DT` are not included in the result). This differs
from a data.table `DT[i]` join, in which the output inherits the key of
`DT` provided it remains sorted on those columns. If not all of the key
columns are selected in the result, the leading subset is used.

### sf objects and `sfc`-class columns

Joins between two `sf` objects are supported. The relation-to-geometry
attribute `agr` is inherited from the input supplying the active
geometry. All `sfc`-class columns in the output are refreshed after
joining (using
[`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html) with
`recompute_bbox = TRUE`); this is true regardless of whether or not the
inputs and output are `sf`s.

## See also

See the package-level documentation
[`fjoin`](https://trobx.github.io/fjoin/reference/fjoin-package.md) for
related functions.

## Examples

``` r
# An illustration showing:
# - two calls to fjoin_left() (commented out), differing in the `order` argument
# - the resulting calls to dtjoin(), plus `show = TRUE`
# - the generated data.table code and output

# data frames
set.seed(1)
df_x <- data.frame(id_x = 1:3, col_x = paste0("x", 1:3), val = runif(3))
df_y <- data.frame(id_y = rep(4:2, each = 2), col_y = paste0("y", 1:6), val = runif(6))

# ---------------------------------------------------------------------------

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
#> .DT : df_y (cast as data.table)
#> .i  : df_x (cast as data.table)
#> Join: .DT[.i, on = "id_y == id_x", mult = "first", data.frame(id_x, col_x, val = i.val, col_y, R.val = val)]
#> 
#>   id_x col_x       val col_y     R.val
#> 1    1    x1 0.2655087  <NA>        NA
#> 2    2    x2 0.3721239    y5 0.6607978
#> 3    3    x3 0.5728534    y3 0.8983897

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
#> .DT : df_x (cast as data.table)
#> .i  : df_y (cast as data.table)
#> Join: setDF(with(list(fjoin.temp = setDT(setDT(.i[, fjoin.which.i := .I][.DT[, fjoin.which.DT := .I], on = "id_y == id_x", nomatch = NULL, mult = "first", data.frame(id_x = i.id_x, col_x = i.col_x, val = i.val, fjoin.which.DT = i.fjoin.which.DT, fjoin.which.i)])[.i, on = "fjoin.which.i", nomatch = NULL, data.frame(id_x = i.id_y, col_x, val, fjoin.which.DT, col_y, R.val = i.val)])), rbind(fjoin.temp, setDT(.DT[!fjoin.temp$fjoin.which.DT, data.frame(id_x, col_x, val, fjoin.which.DT)]), fill = TRUE))[, fjoin.which.DT := NULL])[]
#> 
#>   id_x col_x       val col_y     R.val
#> 1    3    x3 0.5728534    y3 0.8983897
#> 2    2    x2 0.3721239    y5 0.6607978
#> 3    1    x1 0.2655087  <NA>        NA
```
