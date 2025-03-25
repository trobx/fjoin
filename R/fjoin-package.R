#' @keywords internal
"_PACKAGE"

# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
.datatable.aware <- TRUE
# For R CMD check Windows note/Linux and macOS error: "no visible binding for global variable '.SD'".
utils::globalVariables(c(".SD"))
if (FALSE) .SD  # dummy reference

## usethis namespace: start
## usethis namespace: end
NULL
