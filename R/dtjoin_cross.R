#' Return the cross-join of two \code{data.table}s
#'
#' @description
#' Write (and optionally run) \code{data.table} code to return the cross-join of
#'     two \code{data.table}s. The arguments for controlling column order and prefixing
#'     are as for \code{dtjoin}.
#'
#' @inheritParams dtjoin
#'
#' @returns A \code{data.table} (the resulting cross-join), or \code{NULL} if \code{do}
#'     is \code{FALSE}. The data.table code is always printed to the console.
#'
#' @details
#' # TODO
#'
#' @examples
#' # TODO
#'
#' @export
dtjoin_cross <- function(
    .DT            = NULL,
    .i             = NULL,
    i.first        = i.main,
    i.main         = FALSE,
    prefix         = if (i.main) "x." else "i.",
    do             = TRUE
) {

  check_TF(do)
  check_TF(i.first)
  check_TF(i.main)

  check_setup(do, .DT, .i)

  # --------------------------------------------------------------------------

  jvars_DT <- as.character(names(.DT))
  jvars_i  <- as.character(names(.i))

  if (!i.main) {
    jvars_i  <- ifelse(jvars_i %in% names(.DT), sprintf("%s%s = i.%s",prefix,jvars_i,jvars_i), jvars_i)
  } else {
    jvars_i  <- ifelse(jvars_i %in% names(.DT), sprintf("%s = i.%s",jvars_i,jvars_i), jvars_i)
    jvars_DT <- ifelse(jvars_DT %in% names(.i), sprintf("%s%s = %s",prefix,jvars_DT,jvars_DT), jvars_DT)
  }

  jvars <- if (!i.first) c(jvars_DT, jvars_i) else c(jvars_i, jvars_DT)
  jtext <- sprintf("list(%s)", paste(jvars, collapse = ", "))
  jointext <- sprintf("%s[, fjoin.ind := TRUE][%s[, fjoin.ind := TRUE], on = \"fjoin.ind\", allow.cartesian = TRUE, %s]", ".DT", ".i", jtext)

  # --------------------------------------------------------------------------

  cat(".DT :", deparse(substitute(.DT)), "\n")
  cat(".i  :", deparse(substitute(.i)), "\n")
  cat("Join:", jointext, "\n")

  if (do) {
    if (as.numeric(nrow(.DT)) * as.numeric(nrow(.i)) > 2L^31L)
      stop("Cross join would exceed 2^31 rows")
    on.exit(clean_up(.DT, .i), add = TRUE)
    eval(parse(text = jointext))
  }
}
