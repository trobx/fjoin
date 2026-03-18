# Cross join of data frame-like objects `DT` and `i` using a `DT[i]`-style interface to data.table

Write (and optionally run) `data.table` code to return the cross join of
two `data.frame`-like objects using a generalisation of `DT[i]` syntax.

The function
[`fjoin_cross`](https://trobx.github.io/fjoin/reference/fjoin_cross.md)
provides a more conventional interface that is recommended over
`dtjoin_cross` for most users and cases.

## Usage

``` r
dtjoin_cross(
  .DT = NULL,
  .i = NULL,
  select = NULL,
  select.DT = NULL,
  select.i = NULL,
  i.home = FALSE,
  i.first = i.home,
  prefix = if (i.home) "x." else "i.",
  i.class = i.home,
  do = !(is.null(.DT) && is.null(.i)),
  show = !do,
  ...
)
```

## Arguments

- .DT, .i:

  `data.frame`-like objects (plain, `data.table`, tibble, `sf`, `list`,
  etc.), or else both omitted for a mock join statement with no data.

- select, select.DT, select.i:

  Character vectors of columns to be selected from either input if
  present (`select`) or specifically from one or other (`select.DT`,
  `select.i`). `NULL` (the default) selects all columns. Use `""` or
  `NA` to select no columns. Join columns are always selected. See
  Details.

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

- ...:

  Further arguments (for internal use).

## Value

A `data.frame`, `data.table`, (grouped) tibble, `sf`, or `sf`-tibble, or
else `NULL` if `do` is `FALSE`. See Details.

## Details

Details are as for
[`dtjoin`](https://trobx.github.io/fjoin/reference/dtjoin.md) except for
remarks about join columns and matching logic, which do not apply.

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

dtjoin_cross(df1, df2)
#>      bread kcal filling i.kcal
#> 1    Brown  150  Cheese    200
#> 2    White  180  Cheese    200
#> 3 Baguette  250  Cheese    200
#> 4    Brown  150    Pâté    160
#> 5    White  180    Pâté    160
#> 6 Baguette  250    Pâté    160
```
