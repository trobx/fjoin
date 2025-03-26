#' fjoin
#'
#' fjoin is a comprehensive new join package in R, resting on data.table but
#' accessible to all users. It provides high-performance equi and non-equi joins
#' with convenience features and advanced options. For an easy entry to fjoin,
#' please see the ' \href{../doc/fjoin-quickstart.html}{Quickstart Guide} and
#' further links.
#'
#' The \code{fjoin_*()} family of functions provides fast and option-rich
#' implementations of all main join styles via a simple interface:
#' \itemize{
#' \item True joins: \code{\link{fjoin_inner}()}, \code{\link{fjoin_left}()},
#' \code{\link{fjoin_right}()}, \code{\link{fjoin_full}()} \item Semi-joins:
#' \code{\link{fjoin_left_semi}()} (aka \code{\link{fjoin_semi}()}),
#' \code{\link{fjoin_right_semi}()} \item Anti-joins:
#' \code{\link{fjoin_left_anti}()} (aka \code{\link{fjoin_anti}()}),
#' \code{\link{fjoin_right_anti}()}
#' \item Cross-joins: \code{\link{fjoin_cross}()}
#' }
#' These functions are wrappers around a greatly extended data.table-like
#' interface, which is also exported: \code{\link{dtjoin}()},
#' \code{\link{dtjoin_semi_i}()}, \code{\link{dtjoin_anti_DT}()},
#' \code{\link{dtjoin_cross}()}

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
