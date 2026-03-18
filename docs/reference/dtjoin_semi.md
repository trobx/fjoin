# Semi-join of `DT` in a `DT[i]`-style join of data frame-like objects

Write (and optionally run) data.table code to return the semi-join of
`DT` (the rows of `DT` that join with `i`) using a generalisation of
`DT[i]` syntax.

The functions
[`fjoin_left_semi`](https://trobx.github.io/fjoin/reference/fjoin_left_semi.md)
and
[`fjoin_right_semi`](https://trobx.github.io/fjoin/reference/fjoin_right_semi.md)
provide a more conventional interface that is recommended over
`dtjoin_semi` for most users and cases.

## Usage

``` r
dtjoin_semi(
  .DT = NULL,
  .i = NULL,
  on,
  match.na = FALSE,
  mult = "all",
  mult.DT = "all",
  nomatch = NULL,
  nomatch.DT = NULL,
  select = NULL,
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

  Permitted for consistency with `dtjoin` but has no effect on the
  resulting semi-join.

- nomatch, nomatch.DT:

  Permitted for consistency with `dtjoin` but have no effect on the
  resulting semi-join.

- select:

  Character vector of columns of `.DT` to be selected. `NULL` (the
  default) selects all columns. Join columns are always selected.

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

Details are as for
[`dtjoin`](https://trobx.github.io/fjoin/reference/dtjoin.md) except for
arguments controlling the order and prefixing of output columns, which
do not apply.

## See also

See the package-level documentation
[`fjoin`](https://trobx.github.io/fjoin/reference/fjoin-package.md) for
related functions.

## Examples

``` r
# Mock joins

dtjoin_semi(on = "id")
#> .DT : (unnamed)
#> .i  : (unnamed)
#> Join: na.omit(.DT, cols = "id")[id %in% .i$id]
#> 
dtjoin_semi(on = c("id", "date <= date"))
#> .DT : (unnamed)
#> .i  : (unnamed)
#> Join: setDT(.i[, na.omit(.SD), .SDcols = c("id", "date")][.DT, on = c("id", "date >= date"), nomatch = NULL, mult = "first", data.frame(id = i.id, date = i.date, col_DT, col_c = i.col_c)])[]
#> 
dtjoin_semi(on = c("id", "date <= date"), mult = "last")
#> .DT : (unnamed)
#> .i  : (unnamed)
#> Join: .DT[fsort(as.numeric(unique(.DT[.i[, na.omit(.SD), .SDcols = c("id", "date")], on = c("id", "date <= date"), nomatch = NULL, mult = "last", which = TRUE])))]
#> 

```
