# Cross join

Cross join of `x` and `y`

## Usage

``` r
fjoin_cross(
  x = NULL,
  y = NULL,
  order = "left",
  select = NULL,
  select.x = NULL,
  select.y = NULL,
  prefix.y = "R.",
  do = !(is.null(x) && is.null(y)),
  show = !do
)
```

## Arguments

- x, y:

  `data.frame`-like objects (plain, `data.table`, tibble, `sf`, `list`,
  etc.) or else both omitted for a mock join statement with no data. See
  Details.

- order:

  Whether the row order of the result should reflect `x` then `y`
  (`"left"`) or `y` then `x` (`"right"`). Default `"left"`.

- select, select.x, select.y:

  Character vectors of columns to be selected from either input if
  present (`select`) or specifically from one or other of them (e.g.
  `select.x`). `NULL` (the default) selects all columns. Use `""` or
  `NA` to select no columns. Join columns are always selected. See
  Details.

- prefix.y:

  A prefix to attach to column names in `y` that are the same as a
  column name in `x`. Default `"R."`.

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
except for remarks about join columns and matching logic, which do not
apply.

## See also

See the package-level documentation
[`fjoin`](https://trobx.github.io/fjoin/reference/fjoin-package.md) for
related functions.

## Examples

``` r
# data frames
df1 <- data.table::fread(data.table = FALSE, input = "
bread    kcal
Brown     150
White     180
Baguette  250
")

df2 <- data.table::fread(data.table = FALSE, input = "
filling kcal
Cheese   200
Pâté     160
")

fjoin_cross(df1, df2)
#>      bread kcal filling R.kcal
#> 1    Brown  150  Cheese    200
#> 2    Brown  150    Pâté    160
#> 3    White  180  Cheese    200
#> 4    White  180    Pâté    160
#> 5 Baguette  250  Cheese    200
#> 6 Baguette  250    Pâté    160
fjoin_cross(df1, df2, order = "right")
#>      bread kcal filling R.kcal
#> 1    Brown  150  Cheese    200
#> 2    White  180  Cheese    200
#> 3 Baguette  250  Cheese    200
#> 4    Brown  150    Pâté    160
#> 5    White  180    Pâté    160
#> 6 Baguette  250    Pâté    160
```
