
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- <a href="https://CRAN.R-project.org/package=fjoin"><img src="https://www.r-pkg.org/badges/version/fjoin" alt="CRAN status"></a> -->

<a href="https://trobx.r-universe.dev"><img src="https://trobx.r-universe.dev/badges/fjoin" alt="R-universe version"></a>
<a href="https://github.com/trobx/fjoin/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/trobx/fjoin/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check"></a>
<a href="https://app.codecov.io/gh/trobx/fjoin"><img src="https://codecov.io/gh/trobx/fjoin/branch/main/graph/badge.svg?token=CMINLAO40Y" alt="codecov"></a>
<!-- <a href="https://tinyverse.netlify.app/badge/fjoin"><img src="https://tinyverse.netlify.app/badge/fjoin" alt="Dependencies (tinyverse)"></a> -->
<a><img src="https://img.shields.io/badge/dependencies-1/1-green" alt="Dependencies (placeholder)"></a>
<a href="https://lifecycle.r-lib.org/articles/stages.html#experimental"><img src="https://img.shields.io/badge/lifecycle-experimental-orange.svg" alt="Lifecycle: experimental"></a>
<a href="https://trobx.github.io/fjoin/"><img src="https://img.shields.io/badge/docs-homepage-blue.svg" alt="Documentation"></a>
<a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/license-MIT-blue.svg" alt="License: MIT"></a>
<!-- badges: end -->

# fjoin <img src="man/figures/logo.png" align="right" width="120" alt="fjoin logo" />

**Universal data frame joins leveraging
<span class="pkgname">data.table</span>**

*Please view this on the [package
website](https://trobx.github.io/fjoin/) and head to the [Get
started](https://trobx.github.io/fjoin/articles/fjoin.html) guide next.*

## Description

Extends <span class="pkgname">data.table</span> join functionality, lets
it work with any data frame class, and provides a familiar `x`/`y`-style
interface, enabling broad use across R. Offers NA-safe matching by
default, on-the-fly column selection, multiple match-handling on both
sides, `x` or `y` row order, and a row origin indicator. Performs inner,
left, right, full, semi- and anti-joins with equality and inequality
conditions, plus cross joins. Specific support for `data.table`,
(grouped) tibble, and `sf`/`sfc` objects and their attributes; returns a
plain data frame otherwise. Avoids data-copying of inputs and outputs.
Allows displaying the <span class="pkgname">data.table</span> code
instead of (or as well as) executing it.

## Installation

Stable release (CRAN):

``` r
install.packages("fjoin")
```

Latest development version ([R-universe](https://trobx.r-universe.dev)):

``` r
install.packages("fjoin",
  repos = c("https://trobx.r-universe.dev", "https://cloud.r-project.org"))
```

## More information

See [Get started](https://trobx.github.io/fjoin/articles/fjoin.html).
