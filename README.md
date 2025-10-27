
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- <a href="https://CRAN.R-project.org/package=fjoin"><img src="https://www.r-pkg.org/badges/version/fjoin" alt="CRAN status"></a> -->

<a href="https://trobx.r-universe.dev"><img src="https://trobx.r-universe.dev/badges/fjoin" alt="R-universe version"></a>
<a href="https://github.com/trobx/fjoin/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/trobx/fjoin/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check"></a>
<a href="https://codecov.io/gh/trobx/fjoin"><img src="https://codecov.io/gh/trobx/fjoin/branch/main/graph/badge.svg?token=CMINLAO40Y" alt="codecov"></a>
<!-- <a href="https://tinyverse.netlify.app/badge/fjoin"><img src="https://tinyverse.netlify.app/badge/fjoin" alt="Dependencies (tinyverse)"></a> -->
<a><img src="https://img.shields.io/badge/dependencies-1/1-green" alt="Dependencies (placeholder)"></a>
<a href="https://lifecycle.r-lib.org/articles/stages.html#experimental"><img src="https://img.shields.io/badge/lifecycle-experimental-orange.svg" alt="Lifecycle: experimental"></a>
<a href="https://trobx.github.io/fjoin/"><img src="https://img.shields.io/badge/docs-homepage-blue.svg" alt="Documentation"></a>
<a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/license-MIT-blue.svg" alt="License: MIT"></a>
<!-- badges: end -->

# fjoin <img src="man/figures/logo.png" align="right" width="120" alt="fjoin logo" />

Fast and friendly data frame joins leveraging
<span class="pkgname">data.table</span>

## Description

Extends <span class="pkgname">data.table</span> join functionality and
provides a familiar `x`/`y`-style interface that works directly on any
data frames. Provides NA-safe matching by default, on-the-fly column
selection, flexible row-order preservation, multiple-match handling on
both sides, and an indicator column for row origin. Supports inner,
left, right, full, semi- and anti-joins with equality and inequality
conditions, plus cross joins. Specific support for `data.table`,
(grouped) tibble, and `sf`/`sfc` objects and their attributes; prudently
returns a plain data frame otherwise. Avoids data-copying of inputs and
outputs. Allows displaying the <span class="pkgname">data.table</span>
code instead of (or as well as) executing it. Experimental but heavily
tested; feedback and FRs welcome.

## Installation

<!-- 
Stable release from CRAN:
```r
install.packages("fjoin")
```
-->

Latest version from [R-universe](https://trobx.r-universe.dev):

``` r
install.packages("fjoin",
  repos = c("https://trobx.r-universe.dev", "https://cloud.r-project.org"))
```

## More information

See [Get started](https://trobx.github.io/fjoin/articles/fjoin.html).
