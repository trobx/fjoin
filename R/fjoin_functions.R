#' Inner join
#'
#' @description
#' Inner join of \code{x} and \code{y}
#'
#' @param x,y \code{data.frame}-like objects (plain, \code{tibble},
#'   \code{data.table} etc.), or else both omitted (\code{NULL}) for a mock join
#'   statement with no data.
#' @param on A character vector of join predicates, e.g. \code{c("id", "col_x ==
#'   col_y", "date < date")}.
#' @param match.na If \code{TRUE}, allow equality matches between \code{NA}s or
#'   \code{NaN}s. The default is \code{FALSE}, i.e. such matches are not
#'   allowed, as in most real-world applications (but unlike other join
#'   frameworks in R)
#' @param mult.x When a row of \code{x} has multiple matching rows in \code{y},
#'   which to accept: \code{"all"} (the default), \code{"first"}, or
#'   \code{"last"}.
#' @param mult.y When a row of \code{y} has multiple matching rows in \code{x},
#'   which to accept: \code{"all"} (the default), \code{"first"}, or
#'   \code{"last"}. Can be combined with \code{mult.x}.
#' @param select,select.x,select.y Character vectors of columns to be selected
#'   from either input if present (\code{select}) or specifically from one or
#'   other of them (e.g. \code{select.x}). \code{NULL} (the default) selects
#'   all columns. Use \code{""} (or \code{NA}) to select no columns. Join
#'   columns are always selected.
#' @param order Whether the row order of the result should reflect \code{x} then
#'   \code{y} (\code{"left"}) or \code{y} then \code{x} (\code{"right"}).
#'   Default is \code{"left"} for left, inner, full and cross joins,
#'   \code{"right"} for right joins.
#' @param indicate  Whether to add a column \code{".join"}  at the front of the
#'   result, with values \code{1L} if from \code{x} only, \code{2L} if from
#'   \code{y} only, and \code{3L} if joined from both tables (c.f. \code{_merge}
#'   in Stata). Default \code{FALSE}.
#' @param on.first Whether to place the join columns first in the join result.
#'   Default \code{FALSE}.
#' @param prefix A prefix to attach to column names in \code{y} that are the
#'   same as a column name in \code{x}. Default \code{"R."}.
#' @param preserve (rarely used) Whether to include \code{y}'s equality join
#'   column(s) in addition to \code{x}'s (equivalent to \code{keep} in dplyr).
#'   Default \code{FALSE}. Note that non-equality join columns from \code{x} are
#'   always included separately.
#' @param do Whether to execute the join. If FALSE, the data.table code for the
#'   join is printed to the console instead. Default is \code{TRUE} unless \code{x}
#'   and \code{y} are both omitted/\code{NULL}, in which case a mock join
#'   statement is produced. See details.
#'
#' @returns A \code{data.frame}, \code{tibble} or \code{data.table}, or else
#'  \code{NULL} if \code{do} is \code{FALSE}. See details.
#'
#' @examples
#' # TO DO
#'
#' @export
fjoin_inner <- function(
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    order     = "left",
    select    = NULL,
    select.x  = NULL,
    select.y  = NULL,
    indicate  = FALSE,
    prefix    = "R.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = !(is.null(x) && is.null(y))
) {

  check_arg_order(order)
  order.x <- order == "left"
  xylabels <- c(make_label_fjoin(x, substitute(x)), make_label_fjoin(y, substitute(y)))

  dtjoin(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    on         = if (order.x) flip_on(on) else on,
    mult       = if (order.x) mult.x else mult.y,
    mult.DT    = if (order.x) mult.y else mult.x,
    nomatch    = NULL,
    nomatch.DT = NULL,
    select     = select,
    select.DT  = if (order.x) select.y else select.x,
    select.i   = if (order.x) select.x else select.y,
    i.main     = order.x,
    .labels    = if (order.x) rev(xylabels) else xylabels,
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
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    order     = "left",
    select    = NULL,
    select.x  = NULL,
    select.y  = NULL,
    indicate  = FALSE,
    prefix    = "R.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = !(is.null(x) && is.null(y))
) {

  check_arg_order(order)
  order.x <- order == "left"
  xylabels <- c(make_label_fjoin(x, substitute(x)), make_label_fjoin(y, substitute(y)))

  dtjoin(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    on         = if (order.x) flip_on(on) else on,
    mult       = if (order.x) mult.x else mult.y,
    mult.DT    = if (order.x) mult.y else mult.x,
    nomatch    = if (order.x) NA else NULL,
    nomatch.DT = if (order.x) NULL else NA,
    select     = select,
    select.DT  = if (order.x) select.y else select.x,
    select.i   = if (order.x) select.x else select.y,
    i.main     = order.x,
    .labels    = if (order.x) rev(xylabels) else xylabels,
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
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    indicate  = FALSE,
    order     = "right",
    select    = NULL,
    select.x  = NULL,
    select.y  = NULL,
    prefix    = "R.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = !(is.null(x) && is.null(y))
) {

  check_arg_order(order)
  order.y <- order == "right"
  xylabels <- c(make_label_fjoin(x, substitute(x)), make_label_fjoin(y, substitute(y)))

  dtjoin(
    .DT        = if (order.y) x else y,
    .i         = if (order.y) y else x,
    on         = if (order.y) on else flip_on(on),
    mult       = if (order.y) mult.y else mult.x,
    mult.DT    = if (order.y) mult.x else mult.y,
    nomatch    = if (order.y) NA else NULL,
    nomatch.DT = if (order.y) NULL else NA,
    select     = select,
    select.DT  = if (order.y) select.x else select.y,
    select.i   = if (order.y) select.y else select.x,
    i.main     = !order.y,
    .labels    = if (order.y) xylabels else rev(xylabels),
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
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    on.first  = FALSE,
    order     = "left",
    select    = NULL,
    select.x  = NULL,
    select.y  = NULL,
    indicate  = FALSE,
    prefix    = "R.",
    preserve  = FALSE,
    do        = !(is.null(x) && is.null(y))
) {

  check_arg_order(order)
  order.x <- order == "left"
  xylabels <- c(make_label_fjoin(x, substitute(x)), make_label_fjoin(y, substitute(y)))

  dtjoin(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    on         = if (order.x) flip_on(on) else on,
    mult       = if (order.x) mult.x else mult.y,
    mult.DT    = if (order.x) mult.y else mult.x,
    nomatch    = NA,
    nomatch.DT = NA,
    select     = select,
    select.DT  = if (order.x) select.y else select.x,
    select.i   = if (order.x) select.x else select.y,
    i.main     = order.x,
    .labels    = if (order.x) rev(xylabels) else xylabels,
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
#'   \code{fjoin_semi} can be used instead.
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
fjoin_left_semi <- function(
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    do        = !(is.null(x) && is.null(y))
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
# See SO 57770755
#' @rdname fjoin_left_semi
#' @export
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
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    do        = !(is.null(x) && is.null(y))
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
#'   \code{fjoin_anti} can be used instead.
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
fjoin_left_anti <- function(
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    preserve  = FALSE,
    indicate  = FALSE,
    prefix    = "R.",
    do        = !(is.null(x) && is.null(y))
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
# See SO 57770755
#' @rdname fjoin_left_anti
#' @export
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
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    preserve  = FALSE,
    indicate  = FALSE,
    prefix    = "R.",
    do        = !(is.null(x) && is.null(y))
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
    order     = "left",
    prefix    = "R.",
    do        = TRUE
) {

  check_arg_order(order)
  order.x <- order == "left"

  dtjoin_cross(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    i.main     = order.x,
    prefix     = prefix,
    do         = do
  )
}
