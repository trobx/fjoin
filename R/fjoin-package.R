#' fjoin
#'
#' \pkg{fjoin} is a general-purpose data frame join package that runs on
#' \pkg{data.table} and works seamlessly in general workflows and tidyverse
#' pipelines. It is lightweight, fast, and has short and clear syntax with
#' options not found in other frameworks.
#'
#' @section Vignettes:
#' Visit the package website \url{https://trobx.github.io/fjoin} or access
#' locally in R:
#' \itemize{
#'   \item Guide to fjoin: \code{vignette("fjoin")}
#'   \item Performance overview: \code{vignette("fjoin-performance")}
#' }
#'
#' @section API:
#' \tabular{ll}{
#' \strong{fjoin_* functions} \tab \strong{dtjoin* functions} \cr
#' \emph{\code{x}/\code{y} style} \tab \emph{Extended \code{DT[i]}-like interface} \cr
#' \code{\link{fjoin_inner}()}, \code{\link{fjoin_left}()}, \code{\link{fjoin_right}()}, \code{\link{fjoin_full}()} \tab \code{\link{dtjoin}()} \cr
#' \code{\link{fjoin_left_semi}()} (alias \code{\link{fjoin_semi}()}), \code{\link{fjoin_right_semi}()} \tab \code{\link{dtjoin_semi}()} \cr
#' \code{\link{fjoin_left_anti}()} (alias \code{\link{fjoin_anti}()}), \code{\link{fjoin_right_anti}()} \tab \code{\link{dtjoin_anti}()} \cr
#' \code{\link{fjoin_cross}()} \tab \code{\link{dtjoin_cross}()} \cr
#' }
#'
#' @keywords internal
"_PACKAGE"

# zzz

# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
.datatable.aware <- TRUE
# For R CMD check Windows note/Linux and macOS error: "no visible binding for global variable '.SD'".
utils::globalVariables(c(".SD"))
if (FALSE) .SD  # dummy reference

## usethis namespace: start
## usethis namespace: end
