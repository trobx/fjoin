# Package index

## The `fjoin_*` family

Data frame joins in a familiar `x`/`y` style

### True joins

- [`fjoin_inner()`](https://trobx.github.io/fjoin/reference/fjoin_inner.md)
  : Inner join
- [`fjoin_left()`](https://trobx.github.io/fjoin/reference/fjoin_left.md)
  : Left join
- [`fjoin_right()`](https://trobx.github.io/fjoin/reference/fjoin_right.md)
  : Right join
- [`fjoin_full()`](https://trobx.github.io/fjoin/reference/fjoin_full.md)
  : Full join

### Semi-joins

- [`fjoin_left_semi()`](https://trobx.github.io/fjoin/reference/fjoin_left_semi.md)
  [`fjoin_semi()`](https://trobx.github.io/fjoin/reference/fjoin_left_semi.md)
  : Left semi-join
- [`fjoin_right_semi()`](https://trobx.github.io/fjoin/reference/fjoin_right_semi.md)
  : Right semi-join

### Anti-joins

- [`fjoin_left_anti()`](https://trobx.github.io/fjoin/reference/fjoin_left_anti.md)
  [`fjoin_anti()`](https://trobx.github.io/fjoin/reference/fjoin_left_anti.md)
  : Left anti-join
- [`fjoin_right_anti()`](https://trobx.github.io/fjoin/reference/fjoin_right_anti.md)
  : Right anti-join

### Cross joins

- [`fjoin_cross()`](https://trobx.github.io/fjoin/reference/fjoin_cross.md)
  : Cross join

## The `dtjoin_*` family

An extended `DT[i]`-style interface for data frame joins

### True joins

- [`dtjoin()`](https://trobx.github.io/fjoin/reference/dtjoin.md) :

  Join data frame-like objects using an extended `DT[i]`-style interface
  to data.table

### Semi-joins

- [`dtjoin_semi()`](https://trobx.github.io/fjoin/reference/dtjoin_semi.md)
  :

  Semi-join of `DT` in a `DT[i]`-style join of data frame-like objects

### Anti-joins

- [`dtjoin_anti()`](https://trobx.github.io/fjoin/reference/dtjoin_anti.md)
  :

  Anti-join of `DT` in a `DT[i]`-style join of data frame-like objects

### Cross joins

- [`dtjoin_cross()`](https://trobx.github.io/fjoin/reference/dtjoin_cross.md)
  :

  Cross join of data frame-like objects `DT` and `i` using a
  `DT[i]`-style interface to data.table
