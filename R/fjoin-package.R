#' @keywords internal
"_PACKAGE"

# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
.datatable.aware <- TRUE
# For R CMD check Windows note/Linux and macOS error: "no visible binding for global variable '.SD'".
utils::globalVariables(c(".SD"))
if (FALSE) .SD  # dummy reference

#' ## Vignettes
#' - \link{vignette("fjoin-quickstart")}: A hands-on introduction


## usethis namespace: start
## usethis namespace: end
NULL
