# fjoin

fjoin builds on data.table to provide fast, flexible joins on any data
frames. It slots into tidyverse pipelines and general workflows in a
single line, and provides NA-safe matching by default, on-the-fly column
selection, flexible row-order preservation, multiple-match handling on
both sides, and an indicator column for row origin.

## Vignette

See the [Get started](https://trobx.github.io/fjoin/articles/fjoin.html)
guide on the [package
website](https://trobx.github.io/fjoin/index.html).

## API

|  |  |
|----|----|
| **fjoin\_\* functions** | **dtjoin\_\* functions** |
| *`x`/`y` style* | *Extended `DT[i]` style* |
| [`fjoin_inner()`](https://trobx.github.io/fjoin/reference/fjoin_inner.md), [`fjoin_left()`](https://trobx.github.io/fjoin/reference/fjoin_left.md), [`fjoin_right()`](https://trobx.github.io/fjoin/reference/fjoin_right.md), [`fjoin_full()`](https://trobx.github.io/fjoin/reference/fjoin_full.md) | [`dtjoin()`](https://trobx.github.io/fjoin/reference/dtjoin.md) |
| [`fjoin_left_semi()`](https://trobx.github.io/fjoin/reference/fjoin_left_semi.md) (alias [`fjoin_semi()`](https://trobx.github.io/fjoin/reference/fjoin_left_semi.md)), [`fjoin_right_semi()`](https://trobx.github.io/fjoin/reference/fjoin_right_semi.md) | [`dtjoin_semi()`](https://trobx.github.io/fjoin/reference/dtjoin_semi.md) |
| [`fjoin_left_anti()`](https://trobx.github.io/fjoin/reference/fjoin_left_anti.md) (alias [`fjoin_anti()`](https://trobx.github.io/fjoin/reference/fjoin_left_anti.md)), [`fjoin_right_anti()`](https://trobx.github.io/fjoin/reference/fjoin_right_anti.md) | [`dtjoin_anti()`](https://trobx.github.io/fjoin/reference/dtjoin_anti.md) |
| [`fjoin_cross()`](https://trobx.github.io/fjoin/reference/fjoin_cross.md) | [`dtjoin_cross()`](https://trobx.github.io/fjoin/reference/dtjoin_cross.md) |

## See also

Useful links:

- <https://trobx.github.io/fjoin/>

- Report bugs at <https://github.com/trobx/fjoin/issues>

## Author

**Maintainer**: Toby Robertson <trobx@proton.me>
