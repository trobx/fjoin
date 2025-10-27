#' fjoin
#'
#' \pkg{fjoin} builds on \pkg{data.table} to provide fast, flexible joins on any
#' data frames. It slots into tidyverse pipelines and general workflows in a
#' single line, and provides NA-safe matching by default, on-the-fly column
#' selection, flexible row-order preservation, multiple-match handling on both
#' sides, and an indicator column for row origin.
#'
#' @section Vignette:
#' View the \href{https://https://trobx.github.io/fjoin/articles/fjoin.html}{Get started}
#' guide on the package website or access locally in R with \code{vignette("fjoin")}.
#'
#' @section API:
#' \tabular{ll}{
#' \strong{fjoin_* functions} \tab \strong{dtjoin_* functions} \cr
#' \emph{\code{x}/\code{y} style} \tab \emph{Extended \code{DT[i]} style} \cr
#' \code{\link{fjoin_inner}()}, \code{\link{fjoin_left}()}, \code{\link{fjoin_right}()}, \code{\link{fjoin_full}()} \tab \code{\link{dtjoin}()} \cr
#' \code{\link{fjoin_left_semi}()} (alias \code{\link{fjoin_semi}()}), \code{\link{fjoin_right_semi}()} \tab \code{\link{dtjoin_semi}()} \cr
#' \code{\link{fjoin_left_anti}()} (alias \code{\link{fjoin_anti}()}), \code{\link{fjoin_right_anti}()} \tab \code{\link{dtjoin_anti}()} \cr
#' \code{\link{fjoin_cross}()} \tab \code{\link{dtjoin_cross}()} \cr
#' }
#'
#' @keywords internal
"_PACKAGE"

# zzz---------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
.datatable.aware <- TRUE
utils::globalVariables(c(".SD", ":="))
