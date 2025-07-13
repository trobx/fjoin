#' Join data.frame-like objects using an extended \code{DT[i]}-style interface
#' to data.table
#'
#' @description Write (and optionally run) \pkg{data.table} code for a join
#' using a generalisation of \code{DT[i]} syntax with extended arguments and
#' enhanced behaviour. Accepts any \code{data.frame}-like inputs (not only
#' \code{data.table}s), permits left, right, inner, and full joins, prevents
#' unwanted matches on \code{NA} and \code{NaN} by default, does not garble
#' join columns in non-equality joins, allows \code{mult} on both sides of the
#' join, creates an optional join indicator column, allows specifying which
#' columns to select from each input, and provides convenience options to
#' control column order and prefixing.
#'
#' If run, the join returns a \code{data.frame}, \code{data.table},
#' tibble, \code{sf}, or \code{sf}-tibble according to context.
#' The generated \code{data.table} code can be printed to the console instead of
#' (or as well as) being executed. This feature extends to \emph{mock joins},
#' where no inputs are provided, and template code is produced.
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
#'   \code{NaN}s. The default is \code{FALSE}, i.e. such matches are not
#'   allowed, as in most real-world applications (but unlike other join
#'   frameworks in R).
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
#'   selects all columns. Use \code{NA} or \code{""} to select no columns. Join
#'   columns are always selected. See Details.
#' @param on.first Whether to place the join columns from both inputs first in
#'   the join result. Default \code{FALSE}.
#' @param i.main Whether to treat \code{.i} as the "home" table and \code{.DT}
#'   as the "foreign" table for column prefixing and \code{indicate}. Default
#'   \code{FALSE}, i.e. \code{.DT} is the "home" table, as in \code{[.data.table}.
#' @param i.first Whether to place \code{.i}'s columns before \code{.DT}'s in
#'   the join result. The default is to use the value of \code{i.main}, i.e.
#'   bring \code{.i}'s columns to the front if \code{.i} is the "home" table.
#' @param prefix A prefix to attach to column names in the "foreign" table that
#'   are the same as a column name in the "home" table. The default is
#'   \code{"i."} if the "foreign" table is \code{.i} (\code{i.main} is
#'   \code{FALSE}) and \code{"x."} if it is \code{.DT} (\code{i.main} is
#'   \code{TRUE}).
#' @param preserve (rarely used) Whether to include the "foreign" table's
#'   equality join column(s) in addition to the "home" table's (equivalent to
#'   \code{keep} in dplyr). Default \code{FALSE}. Note that non-equality join
#'   columns from the foreign table are always included separately.
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
#' \subsection{Using \code{select}, \code{select.DT}, and \code{select.i}}{
#' Used on its own, \code{select} retains the join columns plus the specified
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
#' \subsection{Additional notes for \pkg{sf} users}{
#' If \code{.DT} and \code{.i} are both \code{sf} objects whose active geometries
#' are selected in the result, the result sets \code{.i}'s geometry.
#'
#' Regardless of whether or not the inputs and output are \code{sf}, all
#' \code{sfc}-class columns in the join result are refreshed after joining (using
#' \code{sf::st_sfc()} with \code{recompute_bbox = TRUE}).
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
#'   i.main = TRUE,
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
  on.first   = FALSE,
  i.main     = FALSE,
  i.first    = i.main,
  prefix     = if (i.main) "x." else "i.",
  preserve   = FALSE,
  # execution options
  do         = !(is.null(.DT) && is.null(.i)),
  show       = !do,
  verbose    = FALSE,
  ...
) {

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
  check_arg_TF(i.first)
  check_arg_TF(i.main)
  check_arg_TF(preserve)
  check_arg_TF(verbose)

  dot_args <- list(...)

  on   <- clean_on(on)
  mock <- is.null(.DT) && is.null(.i)
  do   <- !mock && do
  show <- show || !do

  if (show) {
    .labels <-
      if (".labels" %in% names(dot_args)) {
        dot_args$.labels
      } else {
        c(make_label_dtjoin(.DT, substitute(.DT)), make_label_dtjoin(.i, substitute(.i)))
      }
  }

  if (mock) {
    tmp   <- make_mock_tables(on)
    .DT   <- tmp[[1]]
    .i    <- tmp[[2]]
    asis.DT <- TRUE
    asis.i  <- TRUE
  }
  else {
    check_input_class(.DT)
    check_input_class(.i)
    orig.DT           <- .DT
    orig.i            <- .i
    asis.DT           <- identical(class(.DT), c("data.table", "data.frame"))
    asis.i            <- identical(class(.i), c("data.table", "data.frame"))
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
  if (has_select) select <- unique(select)
  select.DT <- if (has_select) union(select, select.DT) else unique(select.DT)
  select.i  <- if (has_select) union(select, select.i)  else unique(select.i)

  has_mult       <- mult != "all"
  has_mult.DT    <- mult.DT != "all"
  outer.i        <- !(is.null(nomatch) || nomatch %in% 0L)
  outer.DT       <- !(is.null(nomatch.DT) || nomatch.DT %in% 0L)
  rename_anti.DT <- outer.DT && i.main

  # ----------------------------------------------------------------------------
  # is_joincol_, equi_names_, is_selected_, jvars_, oldnames_anti.DT, newnames_anti.DT

  names.DT      <- unique(names(.DT))
  is_joincol.DT <- rep(FALSE, length(names.DT))
  jvars.DT      <- rep(NA_character_, length(names.DT))

  names.i       <- unique(names(.i))
  is_joincol.i  <- rep(FALSE, length(names.i))
  jvars.i       <- rep(NA_character_, length(names.i))

  if (!match.na) {
    equi_names.DT <- rep(NA_character_, length(on))
    equi_names.i  <- rep(NA_character_, length(on))
  }

  if (rename_anti.DT) {
    oldnames_anti.DT <- rep(NA_character_, length(on))
    newnames_anti.DT <- rep(NA_character_, length(on))
  }

  for (i in seq_along(on)) {

    s <- strsplit_predicate(on[i])

    idx.DT <- match(s[1], names.DT)
    if (is.na(idx.DT)) stop(sprintf("No column named \"%s\" found in `.DT`", s[1]))

    idx.i  <- match(s[3], names.i)
    if (is.na(idx.i)) stop(sprintf("No column named \"%s\" found in `.i`", s[3]))

    is_joincol.DT[idx.DT] <- TRUE
    is_joincol.i[idx.i]   <- TRUE

    if (!match.na && allows_equi(s[2])) {
      equi_names.DT[[i]] <- s[1]
      equi_names.i[[i]]  <- s[3]
    }

    if (rename_anti.DT) {
      if (s[2] == "==" && !preserve) {
        if (s[1] != s[3]) {
          # id1 -> id2
          oldnames_anti.DT[[i]] <- s[1]
          newnames_anti.DT[[i]] <- s[3]
        }
      } else {
        if (s[1] == s[3]) {
          # id -> PREF.id=id
          oldnames_anti.DT[[i]] <- s[1]
          newnames_anti.DT[[i]] <- sprintf("%s%s", prefix, s[3])
        }
      }
    }

    if (!(has_mult.DT && !has_mult)) {
      # typical case
      if (!i.main) {
        # .DT home table
        if (s[2] == "==" && !preserve) {
          # (id, id)   -> (id, NULL)  (id garbles to id=i.id)
          # (id1, id2) -> (id1, NULL) (id1 garbles to id1=id2)
          jvars.DT[idx.DT] <- s[1]
        } else {
          # (id, id)   -> (id=x.id, PREF.id=id)
          # (id1, id2) -> (id1=x.id1, id2)
          jvars.DT[idx.DT] <- sprintf("%s = x.%s", s[1], s[1])
          jvars.i[idx.i]   <- if (s[1] == s[3]) sprintf("%s%s = %s", prefix, s[3], s[3]) else s[3]
        }
      } else {
        # .i home table
        if (s[2] == "==" && !preserve) {
          # (id, id)   -> (NULL, id)  (id garbles to id=i.id)
          # (id1, id2) -> (NULL, id2) (no garbling)
          jvars.i[idx.i] <- s[3]
        } else {
          # (id, id)   -> (PREF.id=x.id, id) (id garbles to id=i.id)
          # (id1, id2) -> (id1=x.id1, id2)   (avoid id1 garbling)
          if (s[1] == s[3]) {
            jvars.DT[idx.DT] <- sprintf("%s%s = x.%s", prefix, s[1], s[1])
          } else {
            jvars.DT[idx.DT] <- sprintf("%s = x.%s", s[1], s[1])
          }
          jvars.i[idx.i] <- s[3]
        }
      }
    } else {
      # special case mult.DT but no mult: join is onto .i on row num (not the on arg)
      if (!i.main) {
        # .DT home table
        if (s[2] == "==" && !preserve) {
          # (id, id)   -> (id=i.id, NULL) (manually garble)
          # (id1, id2) -> (id1=id2, NULL) (manually garble)
          if (s[1] == s[3]) {
            jvars.DT[idx.DT] <- sprintf("%s = i.%s", s[1], s[1])
          } else {
            jvars.DT[idx.DT] <- sprintf("%s = %s", s[1], s[3])
          }
        } else {
          # (id, id)   -> (id, PREF.id=i.id) (do not garble)
          # (id1, id2) -> (id1, id2)         (do not garble)
          jvars.DT[idx.DT] <- s[1]
          jvars.i[idx.i]   <- if (s[1] == s[3]) sprintf("%s%s = i.%s", prefix, s[3], s[3]) else s[3]
        }
      } else {
        # .i home table
        if (s[2] == "==" && !preserve) {
          # (id, id)   -> (NULL, id=i.id) (manually garble)
          # (id1, id2) -> (NULL, id2)     (no garbling)
          jvars.i[idx.i] <- if (s[1] == s[3]) sprintf("%s = i.%s", s[3], s[3]) else s[3]
        } else {
          # (id, id)   -> (PREF.id=id, id=i.id) (do not garble)
          # (id1, id2) -> (id1, id2)            (do not garble)
          if (s[1] == s[3]) {
            jvars.DT[idx.DT] <- sprintf("%s%s = %s", prefix, s[1], s[1])
            jvars.i[idx.i]   <- sprintf("%s = i.%s", s[3], s[3])
          } else {
            jvars.DT[idx.DT] <- s[1]
            jvars.i[idx.i]   <- s[3]
          }
        }
      }
    }
  }

  if (!match.na) {
    equi_names.DT <- equi_names.DT[!is.na(equi_names.DT)]
    equi_names.i  <- equi_names.i[!is.na(equi_names.i)]
  }

  if (rename_anti.DT) {
    oldnames_anti.DT <- oldnames_anti.DT[!is.na(oldnames_anti.DT)]
    newnames_anti.DT <- newnames_anti.DT[!is.na(newnames_anti.DT)]
  }

  # selected (non-join) columns
  is_selected.DT <- if (is.null(select.DT)) !is_joincol.DT else !is_joincol.DT & (names.DT %in% select.DT)
  is_selected.i  <- if (is.null(select.i)) !is_joincol.i else !is_joincol.i & (names.i  %in% select.i)
  jvars.DT[is_selected.DT] <- names.DT[is_selected.DT]
  jvars.i[is_selected.i]   <- names.i[is_selected.i]

  if (!i.main) {
    # (c,c) -> (c,PREF.c=i.c)
    jvars.i <- ifelse(is_selected.i & jvars.i %in% names.DT, sprintf("%s%s = i.%s",prefix,jvars.i,jvars.i), jvars.i)
  } else {
    # (c,c) -> (PREF.c=c,c=i.c)
    jvars.DT <- ifelse(is_selected.DT & jvars.DT %in% names.i, sprintf("%s%s = %s",prefix,jvars.DT,jvars.DT), jvars.DT)
    jvars.i  <- ifelse(is_selected.i & jvars.i %in% names.DT, sprintf("%s = i.%s",jvars.i,jvars.i), jvars.i)
  }

  include.DT <- !is.na(jvars.DT)
  include.i  <- !is.na(jvars.i)

  # ----------------------------------------------------------------------------
  # handle outer.DT, rename_anti.DT

  if (outer.DT) {
    include_anti.DT <- is_joincol.DT | include.DT
    if (rename_anti.DT) {
      # renames for non-join columns x.v <- v
      tmp <- names.DT[is_selected.DT & names.DT %in% names.i]
      oldnames_anti.DT <- c(oldnames_anti.DT, tmp)
      newnames_anti.DT <- c(newnames_anti.DT, sprintf("%s%s", prefix, tmp))
      rename_anti.DT <- length(newnames_anti.DT) != 0L
    }
    names.DT       <- c(names.DT, "fjoin.DT.rn")
    jvars.DT       <- c(jvars.DT, "fjoin.DT.rn")
    is_joincol.DT  <- c(is_joincol.DT, FALSE)
    include.DT     <- c(include.DT, TRUE)
    is_selected.DT <- c(is_selected.DT, TRUE)
    if (!is.null(select)) select <- c(select, "fjoin.DT.rn")
    if (!is.null(select.DT)) select.DT <- c(select.DT, "fjoin.DT.rn")
  }

  # ----------------------------------------------------------------------------
  # screen_NAs

  screen_NAs <-
    !match.na &&
    length(equi_names.DT) &&
    .DT[, anyNA(.SD), .SDcols=equi_names.DT] &&
    .i[, anyNA(.SD), .SDcols=equi_names.i]

  # ----------------------------------------------------------------------------
  # sdcols_

  if (screen_NAs) {
    sdcols.DT <- if (is.null(select.DT)) names.DT else names.DT[is_joincol.DT | is_selected.DT]
    sdcols.i  <- if (is.null(select.i)) names.i else names.i[is_joincol.i | is_selected.i]
  }

  # ----------------------------------------------------------------------------
  # sfc_present
  sfc_present <- any_inherits(.DT, "sfc", mask=is_selected.DT) || any_inherits(.i, "sfc", mask=is_selected.i)

  # ----------------------------------------------------------------------------
  # output class

  if (!do) {
    as_DT <- asis.DT || asis.i
  } else {
    as_sf <- as_tbl_df <- FALSE
    as_tibble_ok <- requireNamespace("tibble", quietly = TRUE)
    # sf/sf-tibble iff sfc col(s) present, sf installed, and .DT or .i is sf whose active geometry is selected
    if (sfc_present && requireNamespace("sf", quietly = TRUE)) {
      # .i ahead of .DT
      if (inherits(orig.i, "sf")) {
        sf_col <- attr(orig.i, "sf_column")
        if (include.i[match(sf_col, names.i)]) {
          as_sf <- TRUE
          if (!i.main && sf_col %in% names.DT) sf_col <- sprintf("%s%s", prefix, sf_col)
          as_tbl_df <- inherits(orig.i, "tbl_df") && as_tibble_ok
        }
      }
      if (!as_sf && inherits(orig.DT, "sf")) {
        sf_col <- attr(orig.DT, "sf_column")
        if (include.DT[match(sf_col, names.DT)]) {
          as_sf <- TRUE
          if (i.main && sf_col %in% names.i) sf_col <- sprintf("%s%s", prefix, sf_col)
          as_tbl_df <- inherits(orig.DT, "tbl_df") && as_tibble_ok
        }
      }
    }
    as_DT <- !as_sf && (asis.DT || asis.i)
    if (!as_DT) as_tbl_df <- (inherits(orig.DT, "tbl_df") || inherits(orig.i, "tbl_df")) && as_tibble_ok
  }

  # ----------------------------------------------------------------------------
  # add_dummy_col.DT, jvars, jtext

  jvars.DT   <- jvars.DT[include.DT]
  jvars.i    <- jvars.i[include.i]

  if (!on.first && !(has_select || has_select.DT || has_select.i)) {
  # all columns, order as is

    jvars <- if (i.first) c(jvars.i, jvars.DT) else c(jvars.DT, jvars.i)

  } else {

    is_joincol.DT     <- is_joincol.DT[include.DT]
    joincol_jvars.DT  <- jvars.DT[is_joincol.DT]
    other_jvars.DT    <- jvars.DT[!is_joincol.DT]
    selected_names.DT <- names.DT[is_selected.DT]

    is_joincol.i      <- is_joincol.i[include.i]
    joincol_jvars.i   <- jvars.i[is_joincol.i]
    other_jvars.i     <- jvars.i[!is_joincol.i]
    selected_names.i  <- names.i[is_selected.i]

    if (has_select && !(has_select.DT || has_select.i)) {
    # select-only case (always as if on.first, then selected in order)

      jvars <-
        if (i.first) {
          c(joincol_jvars.i,
            joincol_jvars.DT,
            stats::na.omit(unlist(lapply(select, function(x) c(other_jvars.i[match(x,selected_names.i)], other_jvars.DT[match(x,selected_names.DT)])))))
        } else {
          c(joincol_jvars.DT,
            joincol_jvars.i,
            stats::na.omit(unlist(lapply(select, function(x) c(other_jvars.DT[match(x,selected_names.DT)], other_jvars.i[match(x,selected_names.i)])))))
        }

    } else {
    # all other cases

      if (!is.null(select.DT)) {
        other_jvars.DT <- c(stats::na.omit(other_jvars.DT[match(select.DT,selected_names.DT)], if (outer.DT) "fjoin.DT.rn" else NULL))
      }
      if (!is.null(select.i)) {
        other_jvars.i  <- stats::na.omit(other_jvars.i[match(select.i,selected_names.i)])
      }
      jvars <-
        if (i.first) {
          if (on.first) c(joincol_jvars.i, joincol_jvars.DT, other_jvars.i, other_jvars.DT) else c(joincol_jvars.i, other_jvars.i, joincol_jvars.DT, other_jvars.DT)
        } else {
          if (on.first) c(joincol_jvars.DT, joincol_jvars.i, other_jvars.DT, other_jvars.i) else c(joincol_jvars.DT, other_jvars.DT, joincol_jvars.i, other_jvars.i)
        }
    }
  }

  add_dummy_col.DT <- FALSE
  if (indicate) {
    if (!outer.i) {
      jvars <- c(list(".join = rep(3L, .N)"), jvars)
    } else {
      add_dummy_col.DT <- TRUE
      jvars <- c(list(sprintf(".join = fifelse(is.na(fjoin.ind), %s, 3L)", if (!i.main) "2L" else "1L")), jvars)
    }
  }

  # unnamed "x" to "x=x" for setDF(list())
  # TODO improve this by dealing with it earlier
  if (sfc_present) jvars <- ifelse(grepl("=", jvars), jvars, sprintf("%s = %s", jvars, jvars))

  jtext <- sprintf(if (sfc_present) "setDF(list(%s))" else "data.frame(%s)", paste(jvars, collapse=", "))

  # ----------------------------------------------------------------------------
  # argtext_

  argtext_nomatch   <- if (!outer.i) "nomatch = NULL, " else ""
  argtext_mult      <- if (mult != "all") sprintf("mult = %s, ", deparse(mult)) else ""
  argtext_verbose   <- if (verbose) ", verbose = TRUE" else ""
  argtext_indicate  <- if (add_dummy_col.DT) "[, fjoin.ind := TRUE]" else ""

  # ----------------------------------------------------------------------------
  # jointext

  if (!has_mult.DT) {
    # (1) no mult.DT

    .DTtext <- if (outer.DT) ".DT[, fjoin.DT.rn := .I]" else ".DT"
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
              deparse(on),
              argtext_nomatch,
              argtext_mult,
              jtext,
              if (!has_mult) ", allow.cartesian = TRUE" else "",
              argtext_verbose)
    if (outer.DT) {
      jointext <- sprintf("setDT(%s)", jointext)
    } else if (as_DT) {
      jointext <- sprintf("setDT(%s)[]", jointext)
    }


  } else if (mult == "all") {
    # (2) mult.DT but not mult

    .DTtext <- if (outer.DT) ".DT[, fjoin.DT.rn := .I]" else ".DT"
    .itext  <- ".i[, fjoin.i.rn := .I]"
    if (screen_NAs) {
      if (na_omit_cost_rc(nrow(.DT), length(sdcols.DT)) > na_omit_cost_rc(nrow(.i), length(sdcols.i))) {
        .itext <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=if (is.null(select.i)) NULL else sdcols.i)
      } else {
        .DTtext <- na_omit_text(.DTtext, na_cols=equi_names.DT, sd_cols=if (is.null(select.DT)) NULL else sdcols.DT)
      }
    }
    jointext <-
      sprintf("setDT(%s[%s, on = %s, nomatch = NULL, mult = %s, %s%s])[%s, on = \"fjoin.i.rn\", %s%s%s]",
              .itext,
              .DTtext,
              deparse(flip_on(on)),
              deparse(mult.DT),
              sprintf(if (sfc_present) "setDF(list(%s%s, fjoin.i.rn = fjoin.i.rn))" else "data.frame(%s%s, fjoin.i.rn)",
                      paste(sapply(names.DT[include.DT], function(x) sprintf("%s = i.%s",x,x)), collapse=", "),
                      if (add_dummy_col.DT) ", fjoin.ind = TRUE" else ""
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
    # both mult.DT and mult - solution depends on whether outer wrt .i
    # add fjoin.DT.rn if not already present for outer.DT

    if (!outer.i) {
      # (3) mult.DT and mult, inner wrt .i

      .DTtext <- ".DT[, fjoin.DT.rn := .I]"
      .itext  <- ".i"
      if (screen_NAs) {
        if (na_omit_cost_rc(nrow(.DT), 1L + length(sdcols.DT)) > na_omit_cost_rc(nrow(.i), length(sdcols.i))) {
          .itext <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=if (is.null(select.i)) NULL else sdcols.i)
        } else {
          .DTtext <- na_omit_text(.DTtext, na_cols=equi_names.DT, sd_cols=if (is.null(select.DT)) NULL else c(sdcols.DT, "fjoin.DT.rn"))
        }
      }
      jointext <-
        sprintf("setDT(%s[%s, on = %s, nomatch = NULL, %s%s%s])[%s%s]%s",
                .DTtext,
                .itext,
                deparse(on),
                argtext_mult,
                sprintf(if (sfc_present) "setDF(list(%s%s%s))" else "data.frame(%s%s%s)",
                        paste(jvars, collapse=", "),
                        if (outer.DT) "" else if (sfc_present) ", fjoin.DT.rn = fjoin.DT.rn" else ", fjoin.DT.rn",
                        if (add_dummy_col.DT) ", fjoin.ind = TRUE" else ""
                ),
                argtext_verbose,
                if (mult.DT=="first") {
                  ", first(.SD), by = \"fjoin.DT.rn\""
                } else {
                  "!duplicated(fjoin.DT.rn, fromLast=TRUE)"
                },
                argtext_verbose,
                if (outer.DT) "" else "[, fjoin.DT.rn := NULL][]")
      if (!(outer.DT || as_DT)) jointext <- sprintf("setDF(%s)[]", jointext)

    } else {
      # (4) mult.DT and mult, outer wrt .i

      .DTtext <- ".DT[, fjoin.DT.rn := .I]"
      .itext  <- ".i[, fjoin.i.rn := .I]"
      if (screen_NAs) .DTtext <- na_omit_text(.DTtext, na_cols=equi_names.DT, sd_cols=if (is.null(select.DT)) NULL else c(sdcols.DT, "fjoin.DT.rn"))
      jointext <-
        sprintf("setDT(%s[%s, on = %s, nomatch = NULL, %s%s%s])[%s%s][.i, on = \"fjoin.i.rn\", %s%s]",
                .DTtext,
                .itext,
                deparse(on),
                argtext_mult,
                sprintf(if (sfc_present) "setDF(list(%s%s%s%s))" else "data.frame(%s%s%s%s)",
                        paste(sapply(names.DT[include.DT], function(x) sprintf("%s = x.%s",x,x)), collapse=", "),
                        if (sfc_present) ", fjoin.i.rn = fjoin.i.rn" else ", fjoin.i.rn",
                        if (outer.DT) "" else if (sfc_present) ", fjoin.DT.rn = fjoin.DT.rn" else ", fjoin.DT.rn",
                        if (add_dummy_col.DT) ", fjoin.ind = TRUE" else ""
                ),
                argtext_verbose,
                if (mult.DT=="first") {
                  ", first(.SD), by = \"fjoin.DT.rn\""
                } else {
                  "!duplicated(fjoin.DT.rn, fromLast=TRUE)"
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

  if (outer.DT) {
    if (!indicate && all(include.DT)) {
      # no j only if no selection and no deselection of fjoin.ind
      .DTantitext <- ".DT[!fjoin.temp$fjoin.DT.rn]"
    } else {
      # NB not include.DT as need potentially excluded join columns
      .DTantinames <- names.DT[include_anti.DT]
      .DTantitext <- sprintf("setDT(.DT[!fjoin.temp$fjoin.DT.rn, %s])",
                             if (sfc_present) {
                               sprintf("setDF(list(%s))", paste(sprintf("%s = %s", .DTantinames, .DTantinames), collapse=", "))
                             } else {
                               sprintf("data.frame(%s)", paste(.DTantinames, collapse=", "))
                             })
    }
    if (rename_anti.DT) .DTantitext <-
        sprintf("setnames(%s, %s, %s)", .DTantitext, deparse(oldnames_anti.DT), deparse(newnames_anti.DT))
    if (indicate) .DTantitext <-
        sprintf("%s[, .join := %s]", .DTantitext, if (!i.main) "1L" else "2L")
    jointext <- sprintf("with(list(fjoin.temp = %s), rbind(fjoin.temp, %s, fill = TRUE))[, fjoin.DT.rn := NULL]", jointext, .DTantitext)
    jointext <- if (as_DT) sprintf("%s[]", jointext) else sprintf("setDF(%s)[]", jointext)
  }

  # --------------------------------------------------------------------------
  if (show) {
    cat(".DT : ", .labels[[1]], "\n", ".i  : ", .labels[[2]], "\n", "Join: ", jointext, "\n\n", sep="")
  }

  if (do) {
    if (asis.DT) on.exit(clean_up(.DT), add=TRUE)
    if (asis.i) on.exit(clean_up(.i), add=TRUE)
    ans <- (eval(parse(text=jointext), envir=list2env(list(.DT=.DT, .i=.i), parent=getNamespace("data.table"))))
    if (!as_DT) {
      if (as_tbl_df) ans <- tibble::as_tibble(ans)
      if (as_sf)     ans <- sf::st_as_sf(ans, sf_column_name=sf_col, sfc_last=FALSE)
    }
    if (sfc_present) ans <- refresh_sfc_cols(ans)
    ans
  }
}

