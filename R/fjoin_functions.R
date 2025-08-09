# Thin wrappers around `dtjoin()`, `dtjoin_semi()`, `dtjoin_anti()`,
# `dtjoin_cross()` that translate from extended `DT[i]` to conventional `x`/`y`.
#
# The four true join functions differ in:
# - the values of `nomatch` and `nomatch.DT` passed to `dtjoin()`
# - the default value of `order` ("right" for `fjoin_right()`, "left" otherwise).
#
# The four semi- and anti-join functions differ in:
# - whether they are "left" or "right" (order of inputs and `on`)
# - whether they delegate to `dtjoin_semi()` or `dtjoin_anti()`
#
# Currently these are distinct functions with documentation populated for
# `fjoin_inner()` and inherited by the others. Would prefer to avoid repetitive
# function bodies by using e.g. two unexported intermediate wrappers with public
# functions like
# `fjoin_inner <- function(...) .fjoin_true(style = "inner", ...)`
# `fjoin_left_semi <- function(...) .fjoin_semi_anti(style1 = "left", style2 = "semi", ...)`
# with single documentation for each group, but leads to R CMD check warnings
# re. documented args not in being in \usage and dots in \usage not being a
# documented arg, plus need to repeat param definitions across the two groups.
# Replacing the dots with named args reintroduces code repetition and means the
# user has to scroll past four full function signatures in \usage (c.80 lines
# total) to get to the important information. TODO: research a way out.
#
#' Inner join
#'
#' @description
#' Inner join of \code{x} and \code{y}
#'
#' @param x,y \code{data.frame}-like objects (plain, \code{data.table},
#'   tibble, \code{sf}, \code{list}, etc.) or else both omitted
#'   (\code{NULL}) for a mock join statement with no data. See Details.
#' @param on A character vector of join predicates, e.g. \code{c("id", "col_x ==
#'   col_y", "date > date", "cost <= budget")}.
#' @param match.na Whether to allow equality matches between \code{NA}s or
#'   \code{NaN}s. The default is \code{FALSE}, as in most real-world
#'   applications (but unlike other join frameworks in R).
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
#'   columns are always selected. See Details.
#' @param order Whether the row order of the result should reflect \code{x} then
#'   \code{y} (\code{"left"}) or \code{y} then \code{x} (\code{"right"}).
#'   Default is \code{"left"} for left, inner, full, and cross joins,
#'   \code{"right"} for right joins.
#' @param indicate  Whether to add a column \code{".join"}  at the front of the
#'   result, with values \code{1L} if from \code{x} only, \code{2L} if from
#'   \code{y} only, and \code{3L} if joined from both tables (c.f. \code{_merge}
#'   in Stata). Default \code{FALSE}.
#' @param on.first Whether to place the join columns first in the join result.
#'   Default \code{FALSE}.
#' @param prefix.y A prefix to attach to column names in \code{y} that are the
#'   same as a column name in \code{x}. Default \code{"y."}.
#' @param preserve Whether to include \code{y}'s equality join column(s) in
#'   addition to \code{x}'s (equivalent to \code{keep} in dplyr). Default
#'   \code{FALSE}. Note that non-equality join columns from \code{x} are always
#'   included separately.
#' @param do Whether to execute the join. If \code{FALSE}, \code{show} is set to
#'   \code{TRUE} and the \pkg{data.table} code for the join is printed to the
#'   console instead. Default is \code{TRUE} unless \code{x} and \code{y} are
#'   both omitted/\code{NULL}, in which case a mock join statement is produced.
#'   See Details.
#' @param show Whether to print the \pkg{data.table} code for the join to the
#'   console. Default is the opposite of \code{do}. If \code{x} and \code{y} are
#'   both omitted/\code{NULL}, mock join code is displayed.
#'
#' @returns A \code{data.frame}, \code{data.table}, tibble, \code{sf}, or
#' \code{sf}-tibble, or else \code{NULL} if \code{do} is \code{FALSE}.
#' See Details.
#'
#' @details
#' \subsection{Input and output class}{
#' Each input can be any object with class \code{data.frame}, or a plain
#' \code{list} of same-length vectors.
#'
#' The output class follows these rules:
#' \itemize{
#'   \item if either input is an \code{sf} with its active geometry selected in
#'   the join, create an \code{sf}
#'   \item otherwise, return a \code{data.table} if either input is a
#'   \code{data.table}, or else create a plain \code{data.frame}
#'   \item finally, if either input is a tibble and the result is not a
#'   \code{data.table}, add tibble class \code{"tbl-df"}.
#' }
#' }
#'
#' \subsection{Using \code{select}, \code{select.x}, and \code{select.y}}{
#' Used on its own, \code{select} keeps the join columns plus the
#' specified non-join columns from both inputs if present.
#'
#' If \code{select.x} is provided (and similarly for \code{select.y}) then:
#' \itemize{
#'  \item if \code{select} is also specified, non-join columns of \code{x}
#'  named in either \code{select} or \code{select.x} are included
#'  \item if \code{select} is not specified, only non-join columns named in
#'  \code{select.x} are included from \code{x}. Thus e.g. \code{select.x = ""}
#'  excludes all of \code{x}'s non-join columns.
#' }
#' Non-existent column names are ignored without warning.
#' }
#'
#' \subsection{Column order}{
#' When \code{select} is specified but \code{select.x} and \code{select.y} are
#' not, the output consists of all join columns followed by the selected
#' non-join columns from either input in the order given in \code{select}.
#'
#' In all other cases:
#' \itemize{
#'   \item columns from \code{x} come before columns from \code{y}
#'   \item within each group of columns, non-join columns are in the order
#'   given by \code{select.x}/\code{select.y}, or in their original data order
#'   if no selection is provided
#'   \item if \code{on.first} is \code{TRUE}, join columns from both inputs are
#'   moved to the front of the overall output.
#' }
#' }
#'
#' \subsection{Using \code{mult.x} and \code{mult.y}}{
#' See the Examples for an application of using \code{mult.x} and \code{mult.y}
#' together. Note that \code{mult.y} is applied after \code{mult.x} except in a
#' right join.
#' }
#'
#' \subsection{Displaying code and 'mock joins'}{
#' The option of displaying the join code with \code{show = TRUE} or by passing
#' null inputs is aimed at \pkg{data.table} users wanting to use the package as
#' a cookbook of recipes for adaptation. If \code{x} and \code{y} are both
#' \code{NULL}, template code is displayed based on join column names implied by
#' \code{on}, plus sample non-join column names. \code{select} arguments are
#' ignored in this case.
#'
#' The code displayed is for the join operation after casting the inputs as
#' \code{data.table}s if necessary, and before casting the result as a tibble
#' and/or \code{sf} if applicable. Note that \pkg{fjoin} departs from the usual
#' \code{j = list()} idiom in order to avoid a deep copy of the output made by
#' \code{as.data.table.list}. (Likewise, internally it takes only shallow copies
#' of columns when casting inputs or outputs to different classes.)
#' }
#'
#' \subsection{Additional notes for \pkg{sf} users}{
#' Joins (non-spatial) between two \code{sf} objects are supported. If both
#' active geometries are selected in the result, the result sets \code{x}'s
#' or \code{y}'s geometry as active according to the value of \code{order}.
#'
#' All \code{sfc}-class columns in the join result are refreshed (using
#' \code{sf::st_sfc()} with \code{recompute_bbox = TRUE}), whether or not the
#' output is \code{sf}.
#' }
#'
#' @seealso
#'  See the package-level documentation \code{\link{fjoin}} for related
#'  functions.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # True joins (inner/left/right/full): basic usage
#' # ---------------------------------------------------------------------------
#'
#' # data frames
#' x <- data.table::fread(data.table = FALSE, input = "
#'   country  pop_m
#' Australia   27.2
#'    Brazil  212.0
#'      Chad    3.0
#' ")
#'
#' y <- data.table::fread(data.table = FALSE, input = "
#'   country forest_pc
#'    Brazil      59.1
#'      Chad       3.2
#'   Denmark      15.8
#' ")
#'
#' NULL # section break
#'
#' fjoin_full(x, y, on = "country", indicate = TRUE)
#' fjoin_left(x, y, on = "country", indicate = TRUE)
#' fjoin_right(x, y, on = "country", indicate = TRUE)
#' fjoin_inner(x, y, on = "country", indicate = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Core options and arguments (in a 1:1 equality join with fjoin_full())
#' # ---------------------------------------------------------------------------
#'
#' # data frames
#' dfQ <- data.table::fread(data.table = FALSE, quote ="'", input = "
#' id quantity                   notes other_cols
#'  2        5                      ''        ...
#'  1        6                      ''        ...
#'  3        7                      ''        ...
#' NA        8  'oranges (not listed)'        ...
#' ")
#'
#' dfP <- data.table::fread(data.table = FALSE, input = "
#' id     item price other_cols
#' NA   apples    10        ...
#'  3  bananas    20        ...
#'  2 cherries    30        ...
#'  1    dates    40        ...
#'  ")
#'
#' NULL # section break
#'
#' # (1) basic syntax
#' # cf. dplyr: full_join(dfQ, dfP, join_by(id), na.matches = "never")
#' fjoin_full(dfQ, dfP, on = "id")
#'
#' # (2) join-select in one line
#' fjoin_full(dfQ, dfP, on = "id", select = c("item", "price", "quantity"))
#'
#' # equivalent operation in dplyr
#' # x <- dfQ |> select(id, quantity)
#' # y <- dfP |> select(id, item, price)
#' # full_join(x, y, join_by(id), na.matches = "never") |>
#' #   select(id, item, price, quantity)
#' NULL # section break
#'
#' # (an aside) equality matches on NA if you insist
#' fjoin_full(dfQ, dfP, on = "id", select = c("item", "price", "quantity", "notes"), match.na = TRUE)
#'
#' # (3) indicator column (in Stata since 1985)
#' fjoin_full(
#'   dfQ,
#'   dfP,
#'   on = "id",
#'   select = c("item", "price", "quantity"),
#'   indicate = TRUE
#' )
#'
#' # (4) order rows by y then x
#' fjoin_full(
#'   dfQ,
#'   dfP,
#'   on = "id",
#'   select = c("item", "price", "quantity"),
#'   indicate = TRUE,
#'   order = "right"
#' )
#'
#' # (5) display code instead
#' fjoin_full(
#'   dfQ,
#'   dfP,
#'   on = "id",
#'   select = c("item", "price", "quantity"),
#'   indicate = TRUE,
#'   order = "right",
#'   do = FALSE
#' )
#'
#' # ---------------------------------------------------------------------------
#' # M:M inequality join reduced to 1:1 using `mult.x` and `mult.y`
#' # ---------------------------------------------------------------------------
#'
#' # data.table (`mult`) and dplyr (`multiple`) have options for reducing the
#' # cardinality on one side of the join from many ("all") to one ("first" or
#' # "last"). fjoin (`mult.x`, `mult.y`) permits this on either side of the
#' # join, or on both sides at once.
#'
#' # This example (using `fjoin_left()`) shows an application to temporally
#' # ordered data frames of "events" and "reactions".
#'
#' # data frames
#' events <- data.table::fread(data.table = FALSE, input = "
#' event_id event_ts
#'        1       10
#'        2       20
#'        3       40
#' ")
#'
#' reactions <- data.table::fread(data.table = FALSE, input = "
#' reaction_id reaction_ts
#'           1          30
#'           2          50
#'           3          60
#' ")
#' NULL # section break
#'
#' # (1) for each event, all subsequent reactions (M:M)
#' fjoin_left(
#'   events,
#'   reactions,
#'   on = c("event_ts < reaction_ts"),
#' )
#'
#' # (2) for each event, the next reaction (1:M)
#' fjoin_left(
#'   events,
#'   reactions,
#'   on = c("event_ts < reaction_ts"),
#'   mult.x = "first"
#' )
#'
#' # (3) for each event, the next reaction, provided there was no intervening event (1:1)
#' fjoin_left(
#'   events,
#'   reactions,
#'   on = c("event_ts < reaction_ts"),
#'   mult.x = "first",
#'   mult.y = "last"
#' )
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
    prefix.y  = "y.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_on(on)
  check_arg_order(order)
  order.x <- order == "left"
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
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
    .labels    = if (order.x) c(label.y, label.x) else c(label.x, label.y),
    match.na   = match.na,
    on.first   = on.first,
    preserve   = preserve,
    indicate   = indicate,
    prefix     = prefix.y,
    do         = do,
    show       = show
  )
}

# ------------------------------------------------------------------------------
#' Left join
#'
#' @description
#' Left join of \code{x} and \code{y}
#'
#' @inherit fjoin_inner params details return seealso examples
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
    prefix.y  = "y.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_on(on)
  check_arg_order(order)
  order.x <- order == "left"
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
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
    .labels    = if (order.x) c(label.y, label.x) else c(label.x, label.y),
    match.na   = match.na,
    on.first   = on.first,
    preserve   = preserve,
    indicate   = indicate,
    prefix     = prefix.y,
    do         = do,
    show       = show
  )
}

# ------------------------------------------------------------------------------
#' Right join
#'
#' @description
#' Right join of \code{x} and \code{y}
#'
#' @inherit fjoin_inner params details return seealso examples
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
    prefix.y  = "y.",
    on.first  = FALSE,
    preserve  = FALSE,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_on(on)
  check_arg_order(order)
  order.x <- order == "left"
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
  dtjoin(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    on         = if (order.x) flip_on(on) else on,
    mult       = if (order.x) mult.x else mult.y,
    mult.DT    = if (order.x) mult.y else mult.x,
    nomatch    = if (order.x) NULL else NA,
    nomatch.DT = if (order.x) NA else NULL,
    select     = select,
    select.DT  = if (order.x) select.y else select.x,
    select.i   = if (order.x) select.x else select.y,
    i.main     = order.x,
    .labels    = if (order.x) c(label.y, label.x) else c(label.x, label.y),
    match.na   = match.na,
    on.first   = on.first,
    preserve   = preserve,
    indicate   = indicate,
    prefix     = prefix.y,
    do         = do,
    show       = show
  )
}

# ------------------------------------------------------------------------------
#' Full join
#'
#' @description
#' Full join of \code{x} and \code{y}
#'
#' @inherit fjoin_inner params details return seealso examples
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
    prefix.y  = "y.",
    preserve  = FALSE,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_on(on)
  check_arg_order(order)
  order.x <- order == "left"
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
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
    .labels    = if (order.x) c(label.y, label.x) else c(label.x, label.y),
    match.na   = match.na,
    on.first   = on.first,
    preserve   = preserve,
    indicate   = indicate,
    prefix     = prefix.y,
    do         = do,
    show       = show
  )
}

# ------------------------------------------------------------------------------
#' Left semi-join
#'
#' @description
#' The semi-join of \code{x} in a join of \code{x} and \code{y}, i.e. the rows
#'   of \code{x} that join at least once. The alias \code{fjoin_semi} can be
#'   used instead.
#'
#' @inherit fjoin_inner params return seealso
#'
#' @param select Character vector of non-join columns to be selected from
#'   \code{x}. \code{NULL} (the default) selects all columns. Join columns are
#'   always selected.
#'
#' @details
#' Details are as for e.g. \code{\link{fjoin_inner}} except for arguments
#' controlling the order and prefixing of output columns, which do not apply.
#' Output class is determined by \code{x}.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Semi- and anti-joins: basic usage
#' # ---------------------------------------------------------------------------
#'
#' # data frames
#' x <- data.table::fread(data.table = FALSE, input = "
#' country   pop_m
#' Australia  27.2
#' Brazil    212.0
#' Chad        3.0
#' ")
#'
#' y <- data.table::fread(data.table = FALSE, input = "
#' country forest_pc
#' Brazil       59.1
#' Chad          3.2
#' Denmark      15.8
#' ")
#'
#' # full join with `indicate = TRUE` for comparison
#' fjoin_full(x, y, on = "country", indicate = TRUE)
#'
#' fjoin_semi(x, y, on = "country")
#' fjoin_anti(x, y, on = "country")
#' fjoin_right_semi(x, y, on = "country")
#' fjoin_right_anti(x, y, on = "country")
#'
#' # ---------------------------------------------------------------------------
#' # `mult.x` and `mult.y` support
#' # ---------------------------------------------------------------------------
#'
#' # data frames
#' events <- data.table::fread(data.table = FALSE, input = "
#' event_id event_ts
#'        1       10
#'        2       20
#'        3       40
#' ")
#'
#' reactions <- data.table::fread(data.table = FALSE, input = "
#' reaction_id reaction_ts
#'           1          30
#'           2          50
#'           3          60
#' ")
#' NULL # section break
#'
#' # for each event, the next reaction, provided there was no intervening event (1:1)
#' fjoin_full(
#'   events,
#'   reactions,
#'   on = c("event_ts < reaction_ts"),
#'   mult.x = "first",
#'   mult.y = "last",
#'   indicate = TRUE
#' )
#'
#' fjoin_semi(
#'   events,
#'   reactions,
#'   on = c("event_ts < reaction_ts"),
#'   mult.x = "first",
#'   mult.y = "last"
#' )
#'
#' fjoin_anti(
#'   events,
#'   reactions,
#'   on = c("event_ts < reaction_ts"),
#'   mult.x = "first",
#'   mult.y = "last"
#' )
#'
#' @export
fjoin_left_semi <- function(
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    select    = NULL,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_on(on)
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
  dtjoin_semi (
    .DT       = x,
    .i        = y,
    on        = on,
    .labels   = c(label.x, label.y),
    match.na  = match.na,
    mult      = mult.y,
    mult.DT   = mult.x,
    select    = select,
    do        = do,
    show      = show
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
#' The semi-join of \code{y} in a join of \code{x} and \code{y}, i.e. the rows
#'   of \code{y} that join at least once.
#'
#' @inherit fjoin_left_semi params return seealso examples
#'
#' @param select Character vector of columns to be selected from \code{y}.
#' \code{NULL} (the default) selects all columns. Join columns are always
#' selected.
#'
#' @details
#' Details are as for e.g. \code{\link{fjoin_inner}} except for arguments
#' controlling the order and prefixing of output columns, which do not apply.
#' Output class is determined by \code{y}.
#'
#' @export
fjoin_right_semi <- function(
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    select    = NULL,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_on(on)
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
  dtjoin_semi(
    .DT       = y,
    .i        = x,
    on        = flip_on(on),
    .labels   = c(label.y, label.x),
    match.na  = match.na,
    mult      = mult.x,
    mult.DT   = mult.y,
    select    = select,
    do         = do,
    show       = show
  )
}
#'
# ------------------------------------------------------------------------------
#' Left anti-join
#'
#' @description
#' The anti-join of \code{x} in a join of \code{x} and \code{y}, i.e. the rows
#'   of \code{x} that do not join. The alias \code{fjoin_anti} can be used
#'   instead.
#'
#' @inherit fjoin_left_semi params return seealso examples
#'
#' @details
#' Details are as for \code{\link{fjoin_inner}} except for arguments controlling
#' the order and prefixing of output columns, which do not apply. Output class
#' is determined by \code{x}.
#'
#' @export
fjoin_left_anti <- function(
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    select    = NULL,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_on(on)
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
  dtjoin_anti(
    .DT       = x,
    .i        = y,
    on        = on,
    .labels   = c(label.x, label.y),
    match.na  = match.na,
    mult      = mult.y,
    mult.DT   = mult.x,
    select    = select,
    do        = do,
    show      = show
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
#' The anti-join of \code{y} in a join of \code{x} and \code{y}, i.e. the rows
#'   of \code{y} that do not join.
#'
#' @inherit fjoin_right_semi params return seealso details examples
#'
#' @export
fjoin_right_anti <- function(
    x         = NULL,
    y         = NULL,
    on,
    match.na  = FALSE,
    mult.x    = "all",
    mult.y    = "all",
    select    = NULL,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_on(on)
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
  dtjoin_anti(
    .DT       = y,
    .i        = x,
    on        = flip_on(on),
    .labels   = c(label.y, label.x),
    match.na  = match.na,
    mult      = mult.x,
    mult.DT   = mult.y,
    select    = select,
    do        = do,
    show      = show
  )
}

# ------------------------------------------------------------------------------
#' Cross join
#'
#' @description
#' Cross join of \code{x} and \code{y}
#'
#' @inherit fjoin_inner params return seealso
#'
#' @details
#' Details are as for e.g. \code{\link{fjoin_inner}} except for remarks
#' about join columns and matching logic, which do not apply.
#'
#' @examples
#' # data frames
#' df1 <- data.table::fread(data.table = FALSE, input = "
#' bread    kcal
#' Brown     150
#' White     180
#' Baguette  250
#' ")
#'
#' df2 <- data.table::fread(data.table = FALSE, input = "
#' filling kcal
#' Cheese   200
#' Pâté     160
#' ")
#'
#' fjoin_cross(df1, df2)
#' fjoin_cross(df1, df2, order = "right")
#'
#' @export
fjoin_cross <- function(
    x         = NULL,
    y         = NULL,
    order     = "left",
    prefix.y  = "y.",
    select    = NULL,
    select.x  = NULL,
    select.y  = NULL,
    do        = !(is.null(x) && is.null(y)),
    show      = !do
) {
  check_arg_order(order)
  order.x <- order == "left"
  label.x <- make_label_fjoin(x, substitute(x))
  label.y <- make_label_fjoin(y, substitute(y))
  dtjoin_cross(
    .DT        = if (order.x) y else x,
    .i         = if (order.x) x else y,
    .labels    = if (order.x) c(label.y, label.x) else c(label.x, label.y),
    i.main     = order.x,
    prefix     = prefix.y,
    select     = select,
    select.DT  = if (order.x) select.y else select.x,
    select.i   = if (order.x) select.x else select.y,
    do         = do,
    show       = show
  )
}
