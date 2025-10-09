#' Join data frame-like objects using an extended \code{DT[i]}-style interface
#' to data.table
#'
#' @description Write (and optionally run) \pkg{data.table} code for a join
#' using a generalisation of \code{DT[i]} syntax with extended arguments and
#' enhanced behaviour. Accepts any \code{data.frame}-like inputs (not only
#' \code{data.table}s), permits left, right, inner, and full joins, prevents
#' unwanted matches on \code{NA} and \code{NaN} by default, does not garble join
#' columns in non-equality joins, allows \code{mult} on both sides of the join,
#' creates an optional join indicator column, allows specifying which columns to
#' select from each input, and provides convenience options to control column
#' order and prefixing.
#'
#' If run, the join returns a \code{data.frame}, \code{data.table}, tibble,
#' \code{sf}, or \code{sf}-tibble according to context. The generated
#' \pkg{data.table} code can be printed to the console instead of (or as well
#' as) being executed. This feature extends to \emph{mock joins}, where no
#' inputs are provided, and template code is produced.
#'
#' \code{dtjoin} is the workhorse function for \code{\link{fjoin_inner}},
#' \code{\link{fjoin_left}}, \code{\link{fjoin_right}}, and
#' \code{\link{fjoin_full}}, which are wrappers providing a more conventional
#' interface for join operations. These functions are recommended over
#' \code{dtjoin} for most users and cases.
#'
#' @param .DT,.i \code{data.frame}-like objects (plain, \code{data.table}, tibble,
#'   \code{sf}, \code{list}, etc.), or else both omitted (\code{NULL}) for a mock
#'   join statement with no data.
#' @param on A character vector of join predicates, e.g. \code{c("id", "col_DT
#'   == col_i", "date < date")}.
#' @param match.na If \code{TRUE}, allow equality matches between \code{NA}s or
#'   \code{NaN}s. Default \code{FALSE}.
#' @param mult (as in \code{[.data.table}) When a row of \code{.i} has multiple
#'   matching rows in \code{.DT}, which to accept. One of \code{"all"} (the
#'   default), \code{"first"}, or \code{"last"}.
#' @param mult.DT Like \code{mult}, but with the roles of \code{.DT} and
#'   \code{.i} reversed, i.e. when a row of \code{.DT} has multiple matching
#'   rows in \code{.i}, which to accept (default \code{"all"}). Can be combined
#'   with \code{mult}. See Details.
#' @param nomatch (as in \code{[.data.table}) Either \code{NA} (the default) to
#'   retain rows of \code{.i} with no match in \code{.DT}, or \code{NULL} to
#'   exclude them.
#' @param nomatch.DT Like \code{nomatch} but with the roles of \code{.DT} and
#'   \code{.i} reversed, and a different default: either \code{NA} to append
#'   rows of \code{.DT} with no match in \code{.i}, or \code{NULL} (the default)
#'   to leave them out.
#' @param indicate  Whether to add a column \code{".join"} at the front of the
#'   result, with values \code{1L} if from the "home" table only, \code{2L} if
#'   from the "foreign" table only, and \code{3L} if joined from both tables
#'   (c.f. \code{_merge} in Stata). Default \code{FALSE}.
#' @param select,select.DT,select.i Character vectors of columns to be selected
#'   from either input if present (\code{select}) or specifically from one or
#'   other (\code{select.DT}, \code{select.i}). \code{NULL} (the default)
#'   selects all columns. Use \code{""} or \code{NA} to select no columns. Join
#'   columns are always selected. See Details.
#' @param on.first Whether to place the join columns from both inputs first in
#'   the join result. Default \code{FALSE}.
#' @param i.home Whether to treat \code{.i} as the "home" table and \code{.DT}
#'   as the "foreign" table for column prefixing and \code{indicate}. Default
#'   \code{FALSE}, i.e. \code{.DT} is the "home" table, as in
#'   \code{[.data.table}.
#' @param i.first Whether to place \code{.i}'s columns before \code{.DT}'s in
#'   the join result. The default is to use the value of \code{i.home}, i.e.
#'   bring \code{.i}'s columns to the front if \code{.i} is the "home" table.
#' @param prefix A prefix to attach to column names in the "foreign" table that
#'   are the same as a column name in the "home" table. The default is
#'   \code{"i."} if the "foreign" table is \code{.i} (\code{i.home} is
#'   \code{FALSE}) and \code{"x."} if it is \code{.DT} (\code{i.home} is
#'   \code{TRUE}).
#' @param preserve Whether to include equality join columns from the "foreign"
#'   table separately in the output, instead of combining them with those from
#'   the "home" table. Default \code{FALSE}. Note that non-equality join columns
#'   from the foreign table are always included separately.
#' @param i.class Whether the \code{class} of the output should be based on
#'   \code{.i} instead of \code{.DT}. The default follows \code{i.home} (default
#'   \code{FALSE}). See Details for how output \code{class} and other attributes
#'   are set.
#' @param do Whether to execute the join. Default is \code{TRUE} unless
#'   \code{.DT} and \code{.i} are both omitted/\code{NULL}, in which case a mock
#'   join statement is produced.
#' @param show Whether to print the code for the join to the console. Default is
#'   the opposite of \code{do}. If \code{.DT} and \code{.i} are both
#'   omitted/\code{NULL}, mock join code is displayed.
#' @param verbose (passed to \code{[.data.table}) Whether data.table should
#'   print information to the console during execution. Default \code{FALSE}.
#' @param ... Further arguments (for internal use).
#'
#' @returns A \code{data.frame}, \code{data.table}, (grouped) tibble, \code{sf},
#' or \code{sf}-tibble, or else \code{NULL} if \code{do} is \code{FALSE}. See
#' Details.
#'
#' @details
#' \subsection{Input and output class}{
#' Each input can be any object with class \code{data.frame}, or a plain
#' \code{list} of same-length vectors.
#'
#' The output class depends on \code{.DT} by default (but \code{.i} with
#' \code{i.class = TRUE}) and is as follows:
#' \itemize{
#'   \item a \code{data.table} if the input is a pure \code{data.table}
#'   \item a tibble if it is a tibble (and a grouped tibble if it has class
#'   \code{grouped_df})
#'   \item an \code{sf} if it is an \code{sf} with its active geometry selected
#'   in the join
#'   \item a plain \code{data.frame} in all other cases.
#' }
#' The following attributes are carried through and refreshed: \code{data.table}
#' key, grouped tibble \code{groups}, \code{sf} bounding box, etc. See below for
#' specifics. Other classes and attributes are not carried through.
#' }
#'
#' \subsection{Using \code{select}, \code{select.DT}, and \code{select.i}}{
#' Used on its own, \code{select} keeps the join columns plus the specified
#' non-join columns from both inputs if present.
#'
#' If \code{select.DT} is provided (and similarly for \code{select.i}) then:
#' \itemize{
#'  \item if \code{select} is also specified, non-join columns of \code{.DT}
#'  named in either \code{select} or \code{select.DT} are included
#'  \item if \code{select} is not specified, only non-join columns named in
#'  \code{select.DT} are included from \code{.DT}. Thus e.g. \code{select.DT = ""}
#'  excludes all of \code{.DT}'s non-join columns.
#' }
#' Non-existent column names are ignored without warning.
#' }
#'
#' \subsection{Column order}{
#' When \code{select} is specified but \code{select.DT} and \code{select.i} are
#' not, the output consists of all join columns followed by the selected
#' non-join columns from either input in the order given in \code{select}.
#'
#' In all other cases:
#' \itemize{
#'   \item columns from \code{.DT} come before columns from \code{.i} by default
#'   (but vice versa if \code{i.first} is \code{TRUE})
#'   \item within each group of columns, non-join columns are in the order
#'   given by \code{select.DT}/\code{select.i}, or in their original data order
#'   if no selection is provided
#'   \item if \code{on.first} is \code{TRUE}, join columns from both inputs are
#'   moved to the front of the overall output.
#' }
#' }
#'
#' \subsection{Using \code{mult} and \code{mult.DT}}{
#' If both of these arguments are not the default \code{"all"}, \code{mult} is
#' applied first (typically by passing directly to \code{[.data.table}) and
#' \code{mult.DT} is applied subsequently to eliminate all but the first or last
#' occurrence of each row of \code{.DT} from the inner part of the join,
#' producing a 1:1 result. This order of operations can affect the identity of
#' the rows in the inner join.
#' }
#'
#' \subsection{Displaying code and 'mock joins'}{
#' The option of displaying the join code with \code{show = TRUE} or by passing
#' null inputs is aimed at \pkg{data.table} users wanting to use the package as
#' a cookbook of recipes for adaptation. If \code{.DT} and \code{.i} are both
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
#' \subsection{tibble \code{groups}}{
#' If the relevant input is a grouped tibble (class \code{grouped_df}), the
#' output is grouped by the grouping columns that are selected in the result.
#' }
#'
#' \subsection{\pkg{data.table} \code{key}s}{
#' If \code{.i} is a \code{key}ed \code{data.table} and the output is also a
#' \code{data.table}, it inherits \code{.i}'s key provided
#' \code{nomatch.DT} is \code{NULL} (i.e. the non-matching rows of \code{.DT}
#' are not included in the result). This differs from a \pkg{data.table}
#' \code{DT[i]} join, in which the output inherits the key of \code{DT}
#' provided it remains sorted on those columns. If not all of the key columns
#' are selected in the result, the leading subset is used.
#' }
#'
#' \subsection{\pkg{sf} objects and \code{sfc}-class columns}{
#' Joins between two \code{sf} objects are supported. All \code{sfc}-class
#' columns in the output are refreshed after joining (using \code{sf::st_sfc()}
#' with \code{recompute_bbox = TRUE}); this is true regardless of whether or not
#' the inputs and output are \code{sf}s.
#' }
#'
#' @seealso
#'  See the package-level documentation \code{\link{fjoin}} for related
#'  functions.
#'
#' @examples
#' # An illustration showing:
#' # - two calls to fjoin_left() (commented out), differing in the `order` argument
#' # - the resulting calls to dtjoin(), plus `show = TRUE`
#' # - the generated data.table code and output
#'
#' # data frames
#' set.seed(1)
#' df_x <- data.frame(id_x = 1:3, col_x = paste0("x", 1:3), val = runif(3))
#' df_y <- data.frame(id_y = rep(4:2, each = 2), col_y = paste0("y", 1:6), val = runif(6))
#'
#' NULL # section break
#'
#' # (1) fjoin_left(df_x, df_y, on = "id_x == id_y", mult.x = "first")
#' dtjoin(
#'   df_y,
#'   df_x,
#'   on = "id_y == id_x",
#'   mult = "first",
#'   i.home = TRUE,
#'   prefix = "R.",
#'   show = TRUE
#' )
#'
#' # (2) fjoin_left(df_x, df_y, on = "id_x == id_y", mult.x = "first", order = "right")
#' dtjoin(
#'   df_x,
#'   df_y,
#'   on = "id_x == id_y",
#'   mult.DT = "first",
#'   nomatch = NULL,
#'   nomatch.DT = NA,
#'   prefix = "R.",
#'   show = TRUE
#' )
#'
#' @export
dtjoin <- function(
  # inputs
  .DT        = NULL,
  .i         = NULL,
  # matching logic
  on,
  match.na   = FALSE,
  mult       = "all",
  mult.DT    = "all",
  # output rows
  nomatch    = NA,
  nomatch.DT = NULL,
  # output columns
  indicate   = FALSE,
  select     = NULL,
  select.DT  = NULL,
  select.i   = NULL,
  preserve   = FALSE,
  on.first   = FALSE,
  i.home     = FALSE,
  i.first    = i.home,
  prefix     = if (i.home) "x." else "i.",
  # output class
  i.class    = i.home,
  # execution options
  do         = !(is.null(.DT) && is.null(.i)),
  show       = !do,
  verbose    = FALSE,
  # passed from fjoin_* (labels)
  ...
) {

  # inputs----------------------------------------------------------------------

  check_names(.DT)
  check_names(.i)
  check_arg_prefix(prefix)
  check_arg_on(on)
  check_arg_TF(match.na)
  check_arg_mult(mult)
  check_arg_mult(mult.DT)
  check_arg_nomatch(nomatch)
  check_arg_nomatch(nomatch.DT)
  check_arg_select(select)
  check_arg_select(select.DT)
  check_arg_select(select.i)
  check_arg_TF(do)
  check_arg_TF(show)
  check_arg_TF(indicate)
  check_arg_TF(on.first)
  check_arg_TF(i.home)
  check_arg_TF(i.first)
  check_arg_TF(i.class)
  check_arg_TF(preserve)
  check_arg_TF(verbose)

  dots <- list(...)
  check_dots_names(dots)

  cols.on <- on_vec_to_df(on)

  mock <- is.null(.DT) && is.null(.i)
  if (mock) do <- FALSE
  if (!do) show <- TRUE

  if (show) {
    .labels <-
      if (".labels" %in% names(dots)) {
        dots$.labels
      } else {
        c(make_label_dtjoin(.DT, substitute(.DT)), make_label_dtjoin(.i, substitute(.i)))
      }
  }

  if (mock) {
    tmp <- make_mock_tables(cols.on)
    .DT <- tmp[[1]]
    .i  <- tmp[[2]]
    check_names(.DT)
    check_names(.i)
    asis.DT <- TRUE
    asis.i  <- TRUE
  } else {
    check_input_class(.DT)
    check_input_class(.i)
    orig.DT <- .DT
    orig.i  <- .i
    asis.DT <- identical(class(.DT), c("data.table", "data.frame"))
    asis.i  <- identical(class(.i), c("data.table", "data.frame"))
    if (!asis.DT) {
      .DT <- shallow_DT(.DT)
      if (show) .labels[[1]] <- paste(.labels[[1]], "(cast as data.table)")
    }
    if (!asis.i) {
      .i <- shallow_DT(.i)
      if (show) .labels[[2]] <- paste(.labels[[2]], "(cast as data.table)")
    }
  }

  has_select    <- !is.null(select)
  has_select.DT <- !is.null(select.DT)
  has_select.i  <- !is.null(select.i)
  if (has_select) {
    select    <- unique(select)
    select.DT <- if (has_select.DT) c(select, unique(select.DT[!select.DT %in% select])) else select
    select.i  <- if (has_select.i) c(select, unique(select.i[!select.i %in% select])) else select
  } else {
    if (has_select.DT) select.DT <- unique(select.DT)
    if (has_select.i)  select.i <- unique(select.i)
  }

  has_mult    <- mult != "all"
  has_mult.DT <- mult.DT != "all"
  outer.i     <- !(is.null(nomatch) || nomatch %in% 0L)
  outer.DT    <- !(is.null(nomatch.DT) || nomatch.DT %in% 0L)

  case <-
    if (!has_mult.DT) {
      1L # no mult.DT
    } else if (!has_mult) {
      2L # mult.DT but no mult
    } else if (!outer.i) {
      3L # mult.DT and mult, inner wrt .i
    } else {
      4L # mult.DT and mult, outer wrt .i
    }

  # cols.on, cols.DT, cols.i ---------------------------------------------------

  cols.DT <- data.table::setDT(list(name = unique(names(.DT))))
  cols.i  <- data.table::setDT(list(name = unique(names(.i))))

  cols.on$idx.DT <- match(cols.on$joincol.DT, cols.DT$name)
  cols.on$idx.i  <- match(cols.on$joincol.i, cols.i$name)

  if (anyNA(cols.on$idx.DT)) stop(
    paste("Join column(s) not found in `.DT`:",
          paste(cols.on[is.na(cols.on$idx.DT),"joincol.DT"], collapse = ", "))
  )
  if (anyNA(cols.on$idx.i)) stop(
    paste("Join column(s) not found in `.i`:",
          paste(cols.on[is.na(cols.on$idx.i),"joincol.i"], collapse = ", "))
  )

  cols.DT$is_joincol <- FALSE
  cols.i$is_joincol  <- FALSE
  data.table::set(cols.DT, cols.on$idx.DT, "is_joincol", TRUE)
  data.table::set(cols.i, cols.on$idx.i, "is_joincol", TRUE)

  cols.DT$is_nonjoincol <- if (is.null(select.DT)) !cols.DT$is_joincol else !cols.DT$is_joincol & (cols.DT$name %in% select.DT)
  cols.i$is_nonjoincol  <- if (is.null(select.i)) !cols.i$is_joincol else !cols.i$is_joincol & (cols.i$name %in% select.i)

  ### join column jvars ###

  if (case %in% c(1L, 3L)) {
    # Cases 1 and 3: jvars used at stage 1 select-on-join

    if (!i.home) {
      # .DT home table

      # equality and not preserve both:
      #   (id, id)   -> (id, NULL)  (id garbles to id=i.id)
      #   (id1, id2) -> (id1, NULL) (id1 garbles to id1=id2)
      # otherwise preserve both:
      #   (id, id)   -> (id=x.id, PREF.id=id)
      #   (id1, id2) -> (id1=x.id1, id2)

      cols.on$jvar.DT <-
        data.table::fifelse(
          cols.on$op == "==" & !preserve,
          cols.on$joincol.DT,
          sprintf("%s = x.%s", cols.on$joincol.DT, cols.on$joincol.DT))

      cols.on$jvar.i  <-
        data.table::fifelse(
          cols.on$op == "==" & !preserve,
          NA_character_,
          data.table::fifelse(
            cols.on$joincol.i == cols.on$joincol.DT,
            sprintf("%s%s = %s", prefix, cols.on$joincol.i, cols.on$joincol.i),
            cols.on$joincol.i))

    } else {
      # .i home table

      # equality and not preserve both:
      #   (id, id)   -> (NULL, id)  (id garbles to id=i.id)
      #   (id1, id2) -> (NULL, id2) (no garbling)
      # otherwise preserve both:
      #   (id, id)   -> (PREF.id=x.id, id) (id garbles to id=i.id)
      #   (id1, id2) -> (id1=x.id1, id2)   (avoid id1 garbling)

      cols.on$jvar.DT <-
        data.table::fifelse(
          cols.on$op == "==" & !preserve,
          NA_character_,
          data.table::fifelse(
            cols.on$joincol.DT == cols.on$joincol.i,
            sprintf("%s%s = x.%s", prefix, cols.on$joincol.DT, cols.on$joincol.DT),
            sprintf("%s = x.%s", cols.on$joincol.DT, cols.on$joincol.DT)))

      cols.on$jvar.i  <- cols.on$joincol.i
    }

  } else {
    # Cases 2 and 4: jvars used at stage 2 join onto .i on fjoin.which.i (not the on arg)

    if (!i.home) {
      # .DT home table

      # equality and not preserve both:
      #   (id, id)   -> (id=i.id, NULL) (manually garble)
      #   (id1, id2) -> (id1=id2, NULL) (manually garble)
      # otherwise preserve both:
      #   (id, id)   -> (id, PREF.id=i.id) (do not garble)
      #   (id1, id2) -> (id1, id2)         (do not garble)

      cols.on$jvar.DT <-
        data.table::fifelse(
          cols.on$op == "==" & !preserve,
          data.table::fifelse(
            cols.on$joincol.DT == cols.on$joincol.i,
            sprintf("%s = i.%s", cols.on$joincol.DT, cols.on$joincol.DT),
            sprintf("%s = %s", cols.on$joincol.DT, cols.on$joincol.i)),
          cols.on$joincol.DT)

      cols.on$jvar.i  <-
        data.table::fifelse(
          cols.on$op == "==" & !preserve,
          NA_character_,
          data.table::fifelse(
            cols.on$joincol.i == cols.on$joincol.DT,
            sprintf("%s%s = i.%s", prefix, cols.on$joincol.i, cols.on$joincol.i),
            cols.on$joincol.i))

    } else {
      # .i home table

      # equality and not preserve both:
      #   (id, id)   -> (NULL, id=i.id) (manually garble)
      #   (id1, id2) -> (NULL, id2)     (no garbling)
      # otherwise preserve both:
      #   (id, id)   -> (PREF.id=id, id=i.id) (do not garble)
      #   (id1, id2) -> (id1, id2)            (do not garble)

      cols.on$jvar.DT <-
        data.table::fifelse(
          cols.on$op == "==" & !preserve,
          NA_character_,
          data.table::fifelse(
            cols.on$joincol.DT == cols.on$joincol.i,
            sprintf("%s%s = %s", prefix, cols.on$joincol.DT, cols.on$joincol.DT),
            cols.on$joincol.DT))

      cols.on$jvar.i  <-
        data.table::fifelse(
          cols.on$joincol.i == cols.on$joincol.DT,
          sprintf("%s = i.%s", cols.on$joincol.i, cols.on$joincol.i),
          cols.on$joincol.i)

    }
  }

  data.table::set(cols.DT, cols.on$idx.DT, "jvar", cols.on$jvar.DT)
  data.table::set(cols.i, cols.on$idx.i, "jvar", cols.on$jvar.i)

  ### non-join column jvars ###

  if (!i.home) {
    # (c,c) -> (c,PREF.c=i.c)
    cols.DT$jvar <-
      data.table::fifelse(cols.DT$is_nonjoincol, cols.DT$name, cols.DT$jvar)
    cols.i$jvar  <-
      data.table::fifelse(
        cols.i$is_nonjoincol,
        data.table::fifelse(
          cols.i$name %in% cols.DT$name,
          sprintf("%s%s = i.%s",prefix,cols.i$name,cols.i$name), cols.i$name),
        cols.i$jvar)
  } else {
    # (c,c) -> (PREF.c=c,c=i.c)
    cols.DT$jvar <-
      data.table::fifelse(
        cols.DT$is_nonjoincol,
        data.table::fifelse(
          cols.DT$name %in% cols.i$name,
          sprintf("%s%s = %s",prefix,cols.DT$name,cols.DT$name),
          cols.DT$name),
        cols.DT$jvar)
    cols.i$jvar  <-
      data.table::fifelse(
        cols.i$is_nonjoincol,
        data.table::fifelse(
          cols.i$name %in% cols.DT$name,
          sprintf("%s = i.%s",cols.i$name,cols.i$name),
          cols.i$name),
        cols.i$jvar)
  }

  cols.DT$has_jvar <- !is.na(cols.DT$jvar)
  cols.i$has_jvar  <- !is.na(cols.i$jvar)

  # handle outer.DT-------------------------------------------------------------

  if (outer.DT) {

    cols.DT <- rbind(
      cols.DT,
      list(name          = "fjoin.which.DT",
           is_joincol    = FALSE,
           is_nonjoincol = TRUE,
           jvar          = "fjoin.which.DT",
           has_jvar      = TRUE),
      use.names = TRUE,
      fill = TRUE)
    if (!is.null(select)) select <- c(select, "fjoin.which.DT")
    if (!is.null(select.DT)) select.DT <- c(select.DT, "fjoin.which.DT")

    if (i.home) {
      # apply garbling and prefixing to .DT's columns in anti-join

      # non-join cols: copy from jvar i.e. if name collision, v -> x.v
      cols.DT$jvar_anti <- data.table::fifelse(cols.DT$is_nonjoincol, cols.DT$jvar, NA_character_)

      # join cols:
      cols.on$jvar_anti.DT <-
        data.table::fcase(
          cols.on$op == "==" & !preserve & cols.on$joincol.DT != cols.on$joincol.i,
            # equality and not preserve, and different names: id1 -> id2
            sprintf("%s = %s", cols.on$joincol.i, cols.on$joincol.DT),
          (cols.on$op != "==" | preserve) & cols.on$joincol.DT == cols.on$joincol.i,
            # preserve, and same names: id -> PREF.id=id
            sprintf("%s%s = %s", prefix, cols.on$joincol.DT, cols.on$joincol.DT),
          default = cols.on$joincol.DT
      )
      data.table::set(cols.DT, cols.on$idx.DT, "jvar_anti", cols.on$jvar_anti.DT)
    }
  }

  # screen_NAs, equi_names_, sdcols_--------------------------------------------

  if (match.na) {
    screen_NAs <- FALSE
  } else {
    allows_equi <- cols.on$op %in% c("==",">=","<=")
    if (any(allows_equi)) {
      equi_names.DT <- cols.on$joincol.DT[allows_equi]
      equi_names.i  <- cols.on$joincol.i[allows_equi]
      screen_NAs <-
        .DT[, anyNA(.SD), .SDcols=equi_names.DT] &&
        .i[, anyNA(.SD), .SDcols=equi_names.i]
    } else {
      screen_NAs <- FALSE
    }
  }

  if (screen_NAs) {
    sdcols.DT <- if (is.null(select.DT)) cols.DT$name else cols.DT$name[cols.DT$is_joincol | cols.DT$is_nonjoincol]
    sdcols.i  <- if (is.null(select.i)) cols.i$name else cols.i$name[cols.i$is_joincol | cols.i$is_nonjoincol]
  }

  # output class----------------------------------------------------------------

  as_DT <- if (i.class) asis.i else asis.DT

  if (do) {

    if (as_DT) {
      # key from .i always
      set_key <- asis.i && data.table::haskey(.i) && !outer.DT
      if (set_key) {
        kcols <- subset_while_in(data.table::key(orig.i), cols.i$name[cols.i$is_joincol | cols.i$is_nonjoincol])
        if (is.null(kcols)) {
          set_key <- FALSE
        } else {
          if (i.home) {
            key <- kcols
          } else {
            kidx <- match(kcols, cols.i$name)
            has_jvar <- cols.i$has_jvar[kidx]
            key <- rep(NA_character_, length(kcols))
            key[has_jvar]  <- substr_until(cols.i$jvar[kidx[has_jvar]], " = ")
            key[!has_jvar] <- cols.on[match(kidx[!has_jvar],cols.on$idx.i), "jvar.DT"]
          }
        }
      }

    } else {
      as_tbl_df <- as_grouped_df <- as_sf <- FALSE
      whose_class <- if (i.class) orig.i else orig.DT
      whose_cols  <- if (i.class) cols.i else cols.DT
      # (grouped) tibble
      if (requireNamespace("dplyr", quietly = TRUE)) {
        if (inherits(whose_class, "grouped_df")) {
          groups <- names(attr(whose_class,"groups"))[-length(names(attr(whose_class,"groups")))]
          groups <- substr_until(fast_na.omit(whose_cols$jvar[match(groups, whose_cols$name)]), " = ")
          as_grouped_df <- length(groups) > 0L
        }
        if (!as_grouped_df) as_tbl_df <- (inherits(whose_class, "tbl_df"))
      }
      # sf data frame
      if (inherits(whose_class, "sf") && requireNamespace("sf", quietly = TRUE)) {
        sf_col_idx <- match(attr(whose_class, "sf_column"), whose_cols$name)
        if (whose_cols$has_jvar[sf_col_idx]) {
          as_sf <- TRUE
          sf_col <- substr_until(whose_cols$jvar[sf_col_idx], until=" = ")
        }
      }
    }
  }

  has_sfc <-
    requireNamespace("sf", quietly = TRUE) &&
    (any_inherits(.DT, "sfc", mask=cols.DT$is_nonjoincol) || any_inherits(.i, "sfc", mask=cols.i$is_nonjoincol))

  # add_ind.DT, jvars, jtext----------------------------------------------------

  if (!on.first && !(has_select || has_select.DT || has_select.i)) {
  # all columns, order as is

    jvars.DT <- cols.DT$jvar[cols.DT$has_jvar]
    jvars.i  <- cols.i$jvar[cols.i$has_jvar]

    jvars <- if (i.first) c(jvars.i, jvars.DT) else c(jvars.DT, jvars.i)

  } else {

    joincol_jvars.DT    <- cols.DT$jvar[cols.DT$is_joincol & cols.DT$has_jvar]
    nonjoincol_jvars.DT <- cols.DT$jvar[cols.DT$is_nonjoincol]

    joincol_jvars.i     <- cols.i$jvar[cols.i$is_joincol & cols.i$has_jvar]
    nonjoincol_jvars.i  <- cols.i$jvar[cols.i$is_nonjoincol]

    if (has_select && !(has_select.DT || has_select.i)) {
    # select-only case (always as if on.first, then selected in order)

      # for each selected name, jvar or NA
      nonjoincol_jvars.DT <- nonjoincol_jvars.DT[match(select, cols.DT$name[cols.DT$is_nonjoincol])]
      nonjoincol_jvars.i  <- nonjoincol_jvars.i[match(select, cols.i$name[cols.i$is_nonjoincol])]

      # interleave, dropping NAs (rbind then fast_na.omit which also flattens)
      jvars <-
        if (i.first) {
          c(joincol_jvars.i, joincol_jvars.DT, fast_na.omit(rbind(nonjoincol_jvars.i, nonjoincol_jvars.DT)))
        } else {
          c(joincol_jvars.DT, joincol_jvars.i, fast_na.omit(rbind(nonjoincol_jvars.DT, nonjoincol_jvars.i)))
        }

    } else {
    # all other cases

      jvars <-
        if (i.first) {
          if (on.first) {
            c(joincol_jvars.i, joincol_jvars.DT, nonjoincol_jvars.i, nonjoincol_jvars.DT)
          } else {
            c(joincol_jvars.i, nonjoincol_jvars.i, joincol_jvars.DT, nonjoincol_jvars.DT)
          }
        } else {
          if (on.first) {
            c(joincol_jvars.DT, joincol_jvars.i, nonjoincol_jvars.DT, nonjoincol_jvars.i)
          } else {
            c(joincol_jvars.DT, nonjoincol_jvars.DT, joincol_jvars.i, nonjoincol_jvars.i)
          }
        }
    }
  }

  add_ind.DT <- FALSE
  if (indicate) {
    if (!outer.i) {
      jvars <- c(".join = rep(3L, .N)", jvars)
    } else {
      add_ind.DT <- TRUE
      jvars <- c(sprintf(".join = fifelse(is.na(fjoin.ind.DT), %s, 3L)", if (!i.home) "2L" else "1L"), jvars)
    }
  }

  # Unnamed "x" to "x=x" for setDF(list()), used when sfc(s) selected to avoid renaming to "geometry"
  if (has_sfc) jvars <- data.table::fifelse(grepl("=", jvars), jvars, sprintf("%s = %s", jvars, jvars))

  jtext <- sprintf(if (has_sfc) "setDF(list(%s))" else "data.frame(%s)", paste(jvars, collapse=", "))

  # jointext--------------------------------------------------------------------

  argtext_nomatch   <- if (!outer.i) "nomatch = NULL, " else ""
  argtext_mult      <- if (mult != "all") sprintf("mult = %s, ", deparse(mult)) else ""
  argtext_verbose   <- if (verbose) ", verbose = TRUE" else ""
  argtext_indicate  <- if (add_ind.DT) "[, fjoin.ind.DT := TRUE]" else ""

  if (case == 1L) {
    # (1) no mult.DT

    .DTtext <- if (outer.DT) ".DT[, fjoin.which.DT := .I]" else ".DT"
    .itext  <- ".i"
    if (screen_NAs) {
      if (!outer.i && na_omit_cost_rc(nrow(.DT), length(sdcols.DT)) > na_omit_cost_rc(nrow(.i), length(sdcols.i))) {
        .itext <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=if (is.null(select.i)) NULL else sdcols.i)
      } else {
        # one-sided or .i smaller
        .DTtext <- na_omit_text(.DTtext, na_cols=equi_names.DT, sd_cols=if (is.null(select.DT)) NULL else sdcols.DT)
      }
    }
    jointext <-
      sprintf("%s%s[%s, on = %s, %s%s%s%s%s]",
              .DTtext,
              argtext_indicate,
              .itext,
              deparse(on_df_to_vec(cols.on)),
              argtext_nomatch,
              argtext_mult,
              jtext,
              if (!has_mult && all(cols.on$op == "==")) ", allow.cartesian = TRUE" else "",
              argtext_verbose)
    if (outer.DT) {
      jointext <- sprintf("setDT(%s)", jointext)
    } else if (as_DT) {
      jointext <- sprintf("setDT(%s)[]", jointext)
    }

  } else if (case == 2L) {
    # (2) mult.DT but not mult

    .DTtext <- if (outer.DT) ".DT[, fjoin.which.DT := .I]" else ".DT"
    .itext  <- ".i[, fjoin.which.i := .I]"
    if (screen_NAs) {
      if (na_omit_cost_rc(nrow(.DT), length(sdcols.DT)) > na_omit_cost_rc(nrow(.i), length(sdcols.i))) {
        .itext <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=if (is.null(select.i)) NULL else sdcols.i)
      } else {
        .DTtext <- na_omit_text(.DTtext, na_cols=equi_names.DT, sd_cols=if (is.null(select.DT)) NULL else sdcols.DT)
      }
    }
    jointext <-
      sprintf("setDT(%s[%s, on = %s, nomatch = NULL, mult = %s, %s%s])[%s, on = \"fjoin.which.i\", %s%s%s]",
              .itext,
              .DTtext,
              deparse(on_df_to_vec(cols.on, flip = TRUE)),
              deparse(mult.DT),
              sprintf(if (has_sfc) "setDF(list(%s%s, fjoin.which.i = fjoin.which.i))" else "data.frame(%s%s, fjoin.which.i)",
                      with(list(x=cols.DT$name[cols.DT$has_jvar]), paste(sprintf("%s = i.%s",x,x), collapse=", ")),
                      if (add_ind.DT) ", fjoin.ind.DT = TRUE" else ""
              ),
              argtext_verbose,
              ".i",                # TODO: make variable
              argtext_nomatch,
              jtext,
              argtext_verbose)
    if (outer.DT) {
      jointext <- sprintf("setDT(%s)", jointext)
    } else if (as_DT) {
      jointext <- sprintf("setDT(%s)[]", jointext)
    }

  } else {
    # both mult.DT and mult
    # need fjoin.which.DT (add if not already present for outer.DT)

    if (case == 3L) {
      # (3) mult.DT and mult, inner wrt .i

      .DTtext <- ".DT[, fjoin.which.DT := .I]"
      .itext  <- ".i"
      if (screen_NAs) {
        if (na_omit_cost_rc(nrow(.DT), 1L + length(sdcols.DT)) > na_omit_cost_rc(nrow(.i), length(sdcols.i))) {
          .itext <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=if (is.null(select.i)) NULL else sdcols.i)
        } else {
          .DTtext <- na_omit_text(.DTtext, na_cols=equi_names.DT, sd_cols=if (is.null(select.DT)) NULL else c(sdcols.DT, "fjoin.which.DT"))
        }
      }
      jointext <-
        sprintf("setDT(%s[%s, on = %s, nomatch = NULL, %s%s%s])[%s%s]%s",
                .DTtext,
                .itext,
                deparse(on_df_to_vec(cols.on)),
                argtext_mult,
                sprintf(if (has_sfc) "setDF(list(%s%s%s))" else "data.frame(%s%s%s)",
                        paste(jvars, collapse=", "),
                        if (outer.DT) "" else if (has_sfc) ", fjoin.which.DT = fjoin.which.DT" else ", fjoin.which.DT",
                        if (add_ind.DT) ", fjoin.ind.DT = TRUE" else ""
                ),
                argtext_verbose,
                if (mult.DT=="first") {
                  ", first(.SD), by = \"fjoin.which.DT\""
                } else {
                  "!duplicated(fjoin.which.DT, fromLast=TRUE)"
                },
                argtext_verbose,
                if (outer.DT) "" else "[, fjoin.which.DT := NULL][]")
      if (!(outer.DT || as_DT)) jointext <- sprintf("setDF(%s)[]", jointext)

    } else {
      # (4) mult.DT and mult, outer wrt .i

      .DTtext <- ".DT[, fjoin.which.DT := .I]"
      .itext  <- ".i[, fjoin.which.i := .I]"
      if (screen_NAs) .DTtext <- na_omit_text(.DTtext, na_cols=equi_names.DT, sd_cols=if (is.null(select.DT)) NULL else c(sdcols.DT, "fjoin.which.DT"))
      jointext <-
        sprintf("setDT(%s[%s, on = %s, nomatch = NULL, %s%s%s])[%s%s][.i, on = \"fjoin.which.i\", %s%s]",
                .DTtext,
                .itext,
                deparse(on_df_to_vec(cols.on)),
                argtext_mult,
                sprintf(if (has_sfc) "setDF(list(%s%s%s%s))" else "data.frame(%s%s%s%s)",
                        with(list(x=cols.DT$name[cols.DT$has_jvar]), paste(sprintf("%s = x.%s",x,x), collapse=", ")),
                        if (has_sfc) ", fjoin.which.i = fjoin.which.i" else ", fjoin.which.i",
                        if (outer.DT) "" else if (has_sfc) ", fjoin.which.DT = fjoin.which.DT" else ", fjoin.which.DT",
                        if (add_ind.DT) ", fjoin.ind.DT = TRUE" else ""
                ),
                argtext_verbose,
                if (mult.DT=="first") {
                  ", first(.SD), by = \"fjoin.which.DT\""
                } else {
                  "!duplicated(fjoin.which.DT, fromLast=TRUE)"
                },
                argtext_verbose,
                jtext,
                argtext_verbose)
      if (outer.DT) {
        jointext <- sprintf("setDT(%s)", jointext)
      } else if (as_DT) {
        jointext <- sprintf("setDT(%s)[]", jointext)
      }
    }
  }

  # append .DT's anti-join
  if (outer.DT) {
    jvars_anti.DT <- if (i.home) fast_na.omit(cols.DT$jvar_anti) else cols.DT$name[cols.DT$is_joincol | cols.DT$is_nonjoincol]
    .DTantitext <-
      sprintf("setDT(.DT[!fjoin.temp$fjoin.which.DT, %s])",
              if (has_sfc) {
                sprintf("setDF(list(%s%s))",
                        paste(data.table::fifelse(grepl("=", jvars_anti.DT), jvars_anti.DT, sprintf("%s = %s", jvars_anti.DT, jvars_anti.DT)), collapse=", "),
                        if (indicate) sprintf(", .join = rep(%s, .N)", if (!i.home) "1L" else "2L") else "")
              } else {
                sprintf("data.frame(%s%s)",
                        paste(jvars_anti.DT, collapse=", "),
                        if (indicate) sprintf(", .join = rep(%s, .N)", if (!i.home) "1L" else "2L") else "")
              }
      )
    jointext <- sprintf("with(list(fjoin.temp = %s), rbind(fjoin.temp, %s, fill = TRUE))[, fjoin.which.DT := NULL]", jointext, .DTantitext)
    jointext <- if (as_DT) sprintf("%s[]", jointext) else sprintf("setDF(%s)[]", jointext)
  }

  # outputs---------------------------------------------------------------------

  if (show) {
    cat(".DT : ", .labels[[1]], "\n", ".i  : ", .labels[[2]], "\n", "Join: ", jointext, "\n\n", sep="")
  }

  if (do) {
    if (asis.DT) on.exit(drop_temp_cols(.DT), add=TRUE)
    if (asis.i) on.exit(drop_temp_cols(.i), add=TRUE)
    ans <- eval(parse(text=jointext), envir=list2env(list(.DT=.DT, .i=.i), parent=getNamespace("data.table")))
    if (as_DT) {
      if (set_key) attr(ans, "sorted") <- key
    } else{
      if (as_grouped_df) {
        ans <- dplyr::group_by(ans, !!!dplyr::syms(groups))
      } else {
        if (as_tbl_df) ans <- dplyr::as_tibble(ans)
      }
      if (as_sf) ans <- sf::st_as_sf(ans, sf_column_name=sf_col, sfc_last=FALSE)
    }
    if (has_sfc) ans <- refresh_sfc_cols(ans)
    ans
  }
}
