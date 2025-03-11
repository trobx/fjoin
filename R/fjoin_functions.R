#' Inner join
#'
#' @description
#' Inner join of \code{x} and \code{y}
#'
#' @param x,y \code{data.table}s
#' @param on A character vector of join predicates, e.g. \code{c("id", "col_x ==
#'   col_y", "date < date")}, passed to the \code{on} argument of
#'   \code{[.data.table}.
#' @param match.na If \code{TRUE}, allow equality matches between \code{NA}s or
#'   \code{NaN}s. The default is \code{FALSE}, i.e. such matches are not
#'   allowed, as in most real-world applications.
#' @param mult.x When a row of \code{x} has multiple matching rows in \code{y},
#'   which to accept: \code{"all"} (the default), \code{"first"}, or
#'   \code{"last"}.
#' @param mult.y When a row of \code{y} has multiple matching rows in \code{x},
#'   which to accept: \code{"all"} (the default), \code{"first"}, or
#'   \code{"last"}. Can be combined with \code{mult.x}.
#' @param order Whether the row order of the result should reflect \code{x} then
#'   \code{y} (\code{"x"}) or \code{y} then \code{x} (\code{"y"}). Default is
#'   \code{"x"} for left, inner, full and cross joins, \code{"y"} for right
#'   joins.
#' @param indicate  Whether to add a column \code{".join"} with values \code{1L}
#'   if from \code{x} only, \code{2L} if from \code{y} only, and \code{3L} if
#'   joined from both tables. C.f. the _merge option in Stata. Default
#'   \code{FALSE}.
#' @param on.first Whether to place the join columns first in the join result.
#'   Default \code{FALSE}.
#' @param prefix A prefix to attach to column names in \code{y} that are the
#'   same as a column name in \code{x}. Default \code{"R"}.
#' @param preserve (rarely used) Whether to include \code{y}'s equality join
#'   column(s) in addition to \code{x}'s (equivalent to "keep" in dplyr).
#'   Default \code{FALSE}. Note that non-equality join columns from \code{x} are
#'   always included separately.
#' @param do Whether to execute the join. Default is \code{TRUE} unless \code{x}
#'   and \code{y} are both omitted/\code{NULL}, in which case a mock join
#'   statement is produced. The join statement is always printed to the console
#'   regardless of \code{do}.
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @export
fjoin_inner <- function(
    x,
    y,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    order     = "x",
    indicate  = FALSE,
    prefix    = "R.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = TRUE
) {

  check_arg_order(order)
  order.x <- order == "x"

  dtjoin(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    on         = if (order.x) flip_on(on) else on,
    mult       = mult.x,
    mult.DT    = mult.y,
    nomatch    = NULL,
    nomatch.DT = NULL,
    i.main     = order.x,
    match.na   = match.na,
    on.first   = on.first,
    preserve   = preserve,
    indicate   = indicate,
    prefix     = prefix,
    do         = do
  )

}

# ------------------------------------------------------------------------------
#' Left join
#'
#' @description
#' Left join of \code{x} and \code{y}
#'
#' @inheritParams fjoin_inner
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @export
fjoin_left <- function(
    x,
    y,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    order     = "x",
    indicate  = FALSE,
    prefix    = "R.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = TRUE
) {

  check_arg_order(order)
  order.x <- order == "x"

  dtjoin(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    on         = if (order.x) flip_on(on) else on,
    mult       = mult.x,
    mult.DT    = mult.y,
    nomatch    = if (order.x) NA else NULL,
    nomatch.DT = if (order.x) NULL else NA,
    i.main     = order.x,
    match.na   = match.na,
    on.first   = on.first,
    preserve   = preserve,
    indicate   = indicate,
    prefix     = prefix,
    do         = do
  )
}

# ------------------------------------------------------------------------------
#' Right join
#'
#' @description
#' Right join of \code{x} and \code{y}
#'
#' @inheritParams fjoin_inner
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @export
fjoin_right <- function(
    x,
    y,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    indicate  = FALSE,
    order     = "y",
    prefix    = "R.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = TRUE
) {

  check_arg_order(order)
  order.y <- order == "y"

  dtjoin(
    .DT        = if (order.y) x else y,
    .i         = if (order.y) y else x,
    on         = if (order.y) on else flip_on(on),
    mult       = mult.y,
    mult.DT    = mult.x,
    nomatch    = if (order.y) NA else NULL,
    nomatch.DT = if (order.y) NULL else NA,
    i.main     = !order.y,
    match.na   = match.na,
    on.first   = on.first,
    preserve   = preserve,
    indicate   = indicate,
    prefix     = prefix,
    do         = do
  )
}

# ------------------------------------------------------------------------------
#' Full join
#'
#' @description
#' Full join of \code{x} and \code{y}
#'
#' @inheritParams fjoin_inner
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @export
fjoin_full <- function(
    x,
    y,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    on.first  = FALSE,
    order     = "x",
    indicate  = FALSE,
    prefix    = "R.",
    preserve  = FALSE,
    do        = TRUE
) {

  check_arg_order(order)
  order.x <- order == "x"

  dtjoin(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    on         = if (order.x) flip_on(on) else on,
    mult       = if (order.x) mult.x else mult.y,
    mult.DT    = if (order.x) mult.y else mult.x,
    nomatch    = NA,
    nomatch.DT = NA,
    i.main     = order.x,
    match.na   = match.na,
    on.first   = on.first,
    preserve   = preserve,
    indicate   = indicate,
    prefix     = prefix,
    do         = do
  )
}

# ------------------------------------------------------------------------------
#' Left semi-join
#'
#' @description
#' The semi-join of \code{x} in a join of \code{x} and \code{y}. The alias
#'   \code{semi_join} can be used instead.
#'
#' @inheritParams fjoin_inner
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @aliases fjoin_semi
#' @export
fjoin_left_semi <- function(
    x,
    y,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    do        = TRUE
) {
  dtjoin_semi_i(
    .DT       = y,
    .i        = x,
    on        = flip_on(on),
    match.na  = match.na,
    mult      = mult.x,
    mult.DT   = mult.y,
    do        = do
  )
}

# ------------------------------------------------------------------------------
fjoin_semi <- fjoin_left_semi

# ------------------------------------------------------------------------------
#' Right semi-join
#'
#' @description
#' The semi-join of \code{y} in a join of \code{x} and \code{y}
#'
#' @inheritParams fjoin_inner
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @export
fjoin_right_semi <- function(
    x,
    y,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    do        = TRUE
) {
  dtjoin_semi_i(
    .DT       = x,
    .i        = y,
    on        = on,
    match.na  = match.na,
    mult      = mult.y,
    mult.DT   = mult.x,
    do        = do
  )
}

# ------------------------------------------------------------------------------
#' Left anti-join
#'
#' @description
#' The anti-join of \code{x} in a join of \code{x} and \code{y}.  The alias
#'   \code{anti_join} can be used instead.
#'
#' @inheritParams fjoin_inner
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @aliases fjoin_anti
#' @export
fjoin_left_anti <- function(
    x,
    y,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    preserve  = FALSE,
    indicate  = FALSE,
    prefix    = "R.",
    do        = TRUE
) {
  dtjoin_anti_DT(
    .DT       = x,
    .i        = y,
    on        = on,
    match.na  = match.na,
    mult      = mult.y,
    mult.DT   = mult.x,
    do        = TRUE
  )
}

# ------------------------------------------------------------------------------
fjoin_anti <- fjoin_left_anti

# ------------------------------------------------------------------------------
#' Right anti-join
#'
#' @description
#' The anti-join of \code{y} in a join of \code{x} and \code{y}
#'
#' @inheritParams fjoin_inner
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @export
fjoin_right_anti <- function(
    x,
    y,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    preserve  = FALSE,
    indicate  = FALSE,
    prefix    = "R.",
    do        = TRUE
) {
  dtjoin_anti_DT(
    .DT       = y,
    .i        = x,
    on        = flip_on(on),
    match.na  = match.na,
    mult      = mult.x,
    mult.DT   = mult.y,
    do        = do
  )
}

# ------------------------------------------------------------------------------
#' Cross-join
#'
#' @description
#' Cross-join of \code{x} and \code{y}
#'
#' @inheritParams fjoin_inner
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TO DO
#'
#' @export
fjoin_cross <- function(
    x,
    y,
    order     = "x",
    prefix    = "R.",
    do        = TRUE
) {

  check_arg_order(order)
  order.x <- order == "x"

  dtjoin_cross(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    i.main     = order.x,
    prefix     = prefix,
    do         = do
  )
}
