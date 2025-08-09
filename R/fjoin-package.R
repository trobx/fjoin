#' fjoin
#'
#' \pkg{fjoin} is a data frame join package that runs on \pkg{data.table},
#' providing fast equi and non-equi joins of all styles. It plugs seamlessly
#' into tidyverse pipelines and general workflows, and has features not found in
#' other frameworks, including NA-safety by default, on-the-fly column
#' selection, flexible row-order preservation, multiple-match handling on both
#' sides, and an indicator column for row origin.
#'
#' @section Vignette: Visit the package website
#'   \url{https://trobx.github.io/fjoin} or access locally in R with \code{vignette("fjoin")}.
#'
#' @section API:
#' \tabular{ll}{
#' \strong{fjoin_* functions} \tab \strong{dtjoin* functions} \cr
#' \emph{\code{x}/\code{y} style} \tab \emph{Extended \code{DT[i]} style} \cr
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
