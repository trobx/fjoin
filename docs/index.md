# fjoin

**Data frame joins leveraging data.table**

*Please view this page on the [package
website](https://trobx.github.io/fjoin/) and head to the [Get
started](https://trobx.github.io/fjoin/articles/fjoin.html) guide next.*

## CRAN description

Extends data.table join functionality, lets it work with any data frame
class, and provides a familiar `x`/`y`-style interface, enabling broad
use across R. Offers NA-safe matching by default, on-the-fly column
selection, multiple match-handling on both sides, `x` or `y` row order,
and a row origin indicator. Performs inner, left, right, full, semi- and
anti-joins with equality and inequality conditions, plus cross joins.
Specific support for `data.table`, (grouped) tibble, and `sf`/`sfc`
objects and their attributes; returns a plain data frame otherwise.
Avoids data-copying of inputs and outputs. Allows displaying the
data.table code instead of (or as well as) executing it.

## Installation

Stable release (CRAN):

``` r
install.packages("fjoin")
```

Latest development version ([R-universe](https://trobx.r-universe.dev)):

``` r
install.packages("fjoin", repos = c("https://trobx.r-universe.dev"))
```

## More information

See [Get started](https://trobx.github.io/fjoin/articles/fjoin.html).
