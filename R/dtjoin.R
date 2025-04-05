#' Join data.frames using an enhanced and extended \code{DT[i]}-like data.table
#' syntax
#'
#' @description Write (and optionally run) \code{data.table} code for a join,
#'   using \code{DT[i]}-style syntax with many efficient enhancements.
#'   Accepts any \code{data.frame}-like inputs (not only \code{data.table}s),
#'   permits left, right, inner, and full joins, prevents unwanted matches on
#'   \code{NA} and \code{NaN} by default, does not garble join columns in
#'   non-equality joins, allows 'mult' on both sides of the join, creates an
#'   optional join indicator column, allows specifying which columns to select
#'   from each side, and provides convenience options to control column order
#'   and prefixing.
#'
#'   Also permits \emph{mock joins}, where no \code{data.frame} inputs are
#'   provided, which print template code to the console that can be swiped and
#'   adapted.
#'
#'   \code{dtjoin()} is the workhorse function for \code{fjoin_inner()},
#'   \code{fjoin_left()}, \code{fjoin_right()}, and \code{fjoin_full()}, which
#'   are wrappers providing a much more conventional interface for join
#'   operations. These functions are recommended for most users and cases.
#'
#' @param .DT,.i \code{data.frame}-like objects (plain, \code{tibble},
#'   \code{data.table} etc.), or else both omitted (\code{NULL}) for a mock join
#'   statement with no data.
#' @param on A character vector of join predicates acceptable to the \code{on}
#'   argument of \code{[.data.table}, e.g. \code{c("id", "col_x == col_y", "date
#'   < date")}.
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
#'   with \code{mult}.
#' @param nomatch (as in \code{[.data.table}) Either \code{NA} (the default) to
#'   retain rows of \code{.i} with no match in \code{.DT}, or \code{NULL} to
#'   exclude them.
#' @param nomatch.DT Like \code{nomatch} but with the roles of \code{.DT} and
#'   \code{.i} reversed, and a different default: either \code{NA} to append
#'   rows of \code{.DT} with no match in \code{.i}, or \code{NULL} (the default)
#'   to leave them out.
#' @param indicate  Whether to add a column \code{".join"} with values \code{1L}
#'   if from the "home" table only, \code{2L} if from the "foreign" table only,
#'   and \code{3L} if joined from both tables. C.f. the _merge option in Stata.
#'   Default \code{FALSE}.
#' @param select,select.DT,select.i Character vectors of columns to be selected
#'   from either input if present (\code{select}) or from one or other
#'   specifically (e.g. \code{select.DT}). \code{NULL} (the default) selects all
#'   columns. Use \code{NA} (or \code{""}) to select no columns. Join columns
#'   are always selected.
#' @param on.first Whether to place the join columns first in the join result.
#'   Default \code{FALSE}.
#' @param i.main Whether to treat \code{.i} as the "home" table and \code{.DT}
#'   as the "foreign" table for column prefixing and \code{indicate}. Default
#'   \code{FALSE}, i.e. \code{.DT} is the "home" table.
#' @param i.first Whether to place \code{.i}'s columns before \code{.DT}'s in
#'   the join result. The default is to use the value of \code{i.main}, i.e.
#'   bring \code{.i}'s columns to the front if \code{.i} is the "home" table.
#' @param prefix A prefix to attach to column names in the "foreign" table that
#'   are the same as a column name in the "home" table. The default is
#'   \code{"i."} if the "foreign" table is \code{.i} (\code{i.main} is
#'   \code{FALSE}) and \code{"x."} if it is \code{.DT} (\code{i.main} is
#'   \code{TRUE}).
#' @param preserve  (rarely used) Whether to include the "foreign" table's
#'   equality join column(s) in addition to the "home" table's (equivalent to
#'   "keep" in dplyr). Default \code{FALSE}. Note that non-equality join columns
#'   from the foreign table are always included separately.
#' @param do Whether to execute the join. Default is \code{TRUE} unless
#'   \code{.DT} and \code{.i} are both omitted/\code{NULL}, in which case a mock
#'   join statement is produced. The join statement is always printed to the
#'   console regardless of \code{do}.
#' @param verbose (passed to \code{[.data.table}) Whether data.table should
#'   print information to the console during execution. Default \code{FALSE}.
#' @param ... Further arguments (for internal use).
#'
#' @returns A \code{data.table} (the result of the join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # Simple mock joins
#' dtjoin(on = "id", match.na = TRUE)
#' dtjoin(on = "id")
#' dtjoin(on = "id", i.main = TRUE)
#' dtjoin(on = c("id", "t1 < t2"), nomatch.DT = NA)
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
    nomatch    = NA,
    nomatch.DT = NULL,
    indicate   = FALSE,
    # output columns
    select     = NULL,
    select.DT  = NULL,
    select.i   = NULL,
    on.first   = FALSE,
    i.main     = FALSE,
    i.first    = i.main,
    prefix     = if (i.main) "x." else "i.",
    preserve   = FALSE,
    # execution options
    verbose    = FALSE,
    do         = !(is.null(.DT) && is.null(.i)),
    ...
) {

  dot_args <- list(...)

  # TODO: check_on(on)
  # TODO: check_select(select.DT)
  # TODO: check_select(select.i)
  check_TF(match.na)
  check_mult(mult)
  check_mult(mult.DT)
  check_nomatch(nomatch)
  check_nomatch(nomatch.DT)
  check_TF(do)
  check_TF(indicate)
  check_TF(on.first)
  check_TF(i.first)
  check_TF(i.main)
  check_TF(preserve)
  check_TF(verbose)

  mock <- is.null(.DT) && is.null(.i)
  do   <- !mock && do

  if (mock) {
    tmp <- make_mock_tables(on)
    .DT <- tmp[[1]]
    .i  <- tmp[[2]]
  } else {
    check_input_class(.DT)
    check_input_class(.i)
    if (do) {
      asis.DT           <- data.table::is.data.table(.DT)
      asis.i            <- data.table::is.data.table(.i)
      if (!asis.DT) .DT <- shallow_DT(.DT)
      if (!asis.i) .i   <- shallow_DT(.i)
    }
  }

  .labels <-
    if (".labels" %in% names(dot_args)) {
      dot_args$.labels
    } else {
      c(make_label_dtjoin(.DT, substitute(.DT)), make_label_dtjoin(.i, substitute(.i)))
    }

  select.DT      <- c(select,select.DT)
  select.i       <- c(select,select.i)

  on             <- clean_on(on)

  has_mult       <- mult != "all"
  has_mult.DT    <- mult.DT != "all"
  outer.i        <- !(is.null(nomatch) || nomatch %in% 0L)
  outer.DT       <- !(is.null(nomatch.DT) || nomatch.DT %in% 0L)
  rename.DT_anti <- outer.DT && i.main

  # ----------------------------------------------------------------------------
  # jvars_, is_joincol_, equi_names_, oldnames_DT_anti, newnames_DT_anti

  names_DT      <- unique(names(.DT))
  is_joincol_DT <- rep(FALSE, length(names_DT))
  jvars_DT      <- rep(NA_character_, length(names_DT))

  names_i       <- unique(names(.i))
  is_joincol_i  <- rep(FALSE, length(names_i))
  jvars_i       <- rep(NA_character_, length(names_i))

  if (!match.na) {
    equi_names_DT <- character(0)
    equi_names_i  <- character(0)
  }

  if (rename.DT_anti) {
    oldnames_DT_anti <- character(0)
    newnames_DT_anti <- character(0)
  }

  for (i in seq_along(on)) {

    s <- strsplit_predicate(on[i])

    idx_DT <- match(s[1], names_DT)
    if (is.na(idx_DT)) stop(sprintf("No column named \"%s\" found in `.DT`", s[1]))

    idx_i  <- match(s[3], names_i)
    if (is.na(idx_i)) stop(sprintf("No column named \"%s\" found in `.i`", s[3]))

    is_joincol_DT[idx_DT] <- TRUE
    is_joincol_i[idx_i]   <- TRUE

    if (!match.na && allows_equi(s[2])) {
      equi_names_DT <- c(equi_names_DT, s[1])
      equi_names_i  <- c(equi_names_i, s[3])
    }

    if (rename.DT_anti) {
      if (s[2] == "==" && !preserve) {
        if (s[1] != s[3]) {
          # id1 -> id2
          oldnames_DT_anti <- c(oldnames_DT_anti, s[1])
          newnames_DT_anti <- c(newnames_DT_anti, s[3])
        }
      } else {
        if (s[1] == s[3])
          # id -> PREF.id=id
          oldnames_DT_anti <- c(oldnames_DT_anti, s[1])
          newnames_DT_anti <- c(newnames_DT_anti, sprintf("%s%s", prefix, s[3]))
      }
    }

    if (!(has_mult.DT && !has_mult)) {
      # general case
      if (!i.main) {
        # .DT home table
        if (s[2] == "==" && !preserve) {
          # (id, id)   -> (id, NULL)  (id garbles to id=i.id)
          # (id1, id2) -> (id1, NULL) (id1 garbles to id1=id2)
          jvars_DT[idx_DT] <- s[1]
        } else {
          # (id, id)   -> (id=x.id, PREF.id=id)
          # (id1, id2) -> (id1=x.id1, id2)
          jvars_DT[idx_DT] <- sprintf("%s = x.%s", s[1], s[1])
          jvars_i[idx_i]   <- if (s[1] == s[3]) sprintf("%s%s = %s", prefix, s[3], s[3]) else s[3]
        }
      } else {
        # .i home table
        if (s[2] == "==" && !preserve) {
          # (id, id)   -> (NULL, id)  (id garbles to id=i.id)
          # (id1, id2) -> (NULL, id2) (no garbling)
          jvars_i[idx_i] <- s[3]
        } else {
          # (id, id)   -> (PREF.id=x.id, id) (id garbles to id=i.id)
          # (id1, id2) -> (id1=x.id1, id2)   (avoid id1 garbling)
          if (s[1] == s[3]) {
            jvars_DT[idx_DT] <- sprintf("%s%s = x.%s", prefix, s[1], s[1])
          } else {
            jvars_DT[idx_DT] <- sprintf("%s = x.%s", s[1], s[1])
          }
          jvars_i[idx_i] <- s[3]
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
            jvars_DT[idx_DT] <- sprintf("%s = i.%s", s[1], s[1])
          } else {
            jvars_DT[idx_DT] <- sprintf("%s = %s", s[1], s[3])
          }
        } else {
          # (id, id)   -> (id, PREF.id=i.id) (do not garble)
          # (id1, id2) -> (id1, id2)         (do not garble)
          jvars_DT[idx_DT] <- s[1]
          jvars_i[idx_i]   <- if (s[1] == s[3]) sprintf("%s%s = i.%s", prefix, s[3], s[3]) else s[3]
        }
      } else {
        # .i home table
        if (s[2] == "==" && !preserve) {
          # (id, id)   -> (NULL, id=i.id) (manually garble)
          # (id1, id2) -> (NULL, id2)     (no garbling)
          jvars_i[idx_i] <- if (s[1] == s[3]) sprintf("%s = i.%s", s[3], s[3]) else s[3]
        } else {
          # (id, id)   -> (PREF.id=id, id=i.id) (do not garble)
          # (id1, id2) -> (id1, id2)            (do not garble)
          if (s[1] == s[3]) {
            jvars_DT[idx_DT] <- sprintf("%s%s = %s", prefix, s[1], s[1])
            jvars_i[idx_i]   <- sprintf("%s = i.%s", s[3], s[3])
          } else {
            jvars_DT[idx_DT] <- s[1]
            jvars_i[idx_i]   <- s[3]
          }
        }
      }
    }
  }

  # selected (non-join) columns
  is_selected_DT <- if (is.null(select.DT)) !is_joincol_DT else !is_joincol_DT & (names_DT %in% select.DT)
  is_selected_i  <- if (is.null(select.i))  !is_joincol_i else !is_joincol_i   & (names_i %in% select.i)
  jvars_DT[is_selected_DT] <- names_DT[is_selected_DT]
  jvars_i[is_selected_i]   <- names_i[is_selected_i]

  if (!i.main) {
    # (c,c) <- (c,PREF.c=i.c)
    jvars_i <- ifelse(is_selected_i & jvars_i %in% names_DT, sprintf("%s%s = i.%s",prefix,jvars_i,jvars_i), jvars_i)
  } else {
    # (c,c) <- (PREF.c=c,c=i.c)
    jvars_DT <- ifelse(is_selected_DT & jvars_DT %in% names_i, sprintf("%s%s = %s",prefix,jvars_DT,jvars_DT), jvars_DT)
    jvars_i  <- ifelse(is_selected_i & jvars_i %in% names_DT, sprintf("%s = i.%s",jvars_i,jvars_i), jvars_i)
  }

  include_DT <- !is.na(jvars_DT)
  include_i  <- !is.na(jvars_i)

  jvars_DT   <- jvars_DT[include_DT]
  jvars_i    <- jvars_i[include_i]

  # ----------------------------------------------------------------------------
  # handle outer.DT, rename.DT_anti

  if (outer.DT) {
    include_DT_anti <- is_joincol_DT | include_DT
    if (rename.DT_anti) {
      # add renames for non-join columns
      # x.v <- v
      tmp <- names_DT[is_selected_DT & names_DT %in% names_i]
      oldnames_DT_anti <- c(oldnames_DT_anti, tmp)
      newnames_DT_anti <- c(newnames_DT_anti, sprintf("%s%s", prefix, tmp))
      rename.DT_anti <- length(newnames_DT_anti) != 0L
    }
    # need fjoin.DT.rn in all cases
    names_DT      <- c(names_DT, "fjoin.DT.rn")
    jvars_DT      <- c(jvars_DT, "fjoin.DT.rn")
    include_DT    <- c(include_DT, TRUE)
    is_joincol_DT <- c(is_joincol_DT, FALSE)
  }

  # ----------------------------------------------------------------------------
  # jvars, jtext, jtext_DT_anti, add_DT_dummy_col

  if (!on.first) {
    jvars <- if (i.first) c(jvars_i, jvars_DT) else c(jvars_DT, jvars_i)
  } else {
    is_joincol_DT     <- is_joincol_DT[include_DT]
    is_joincol_i      <- is_joincol_i[include_i]
    joincol_jvars_DT  <- jvars_DT[is_joincol_DT]
    joincol_jvars_i   <- jvars_i[is_joincol_i]
    other_jvars_DT    <- jvars_DT[!is_joincol_DT]
    other_jvars_i     <- jvars_i[!is_joincol_i]
    jvars <-
      if (i.first) {
        c(joincol_jvars_i, joincol_jvars_DT, other_jvars_i, other_jvars_DT)
      } else {
        c(joincol_jvars_DT, joincol_jvars_i, other_jvars_DT, other_jvars_i)
      }
  }

  add_DT_dummy_col <- FALSE
  if (indicate) {
    if (!outer.i) {
      jvars <- c(list(".join = 3L"), jvars)
    } else {
      add_DT_dummy_col <- TRUE
      jvars <- c(list(sprintf(".join = fifelse(is.na(fjoin.ind), %s, 3L)", if (!i.main) "2L" else "1L")), jvars)
    }
  }

  jtext <- paste0("data.frame(", paste(jvars, collapse = ", "), ")")

  # ----------------------------------------------------------------------------
  # argtext_, screen_NAs

  argtext_nomatch   <- if (!outer.i) "nomatch = NULL, " else ""
  argtext_mult      <- if (mult != "all") sprintf("mult = %s, ", deparse(mult)) else ""
  argtext_verbose   <- if (verbose) ", verbose = TRUE" else ""
  argtext_indicate  <- if (add_DT_dummy_col) "[, fjoin.ind := TRUE]" else ""

  screen_NAs <-
    !match.na &&
    length(equi_names_DT) &&
    if (inherits(.DT, "data.table")) .DT[, anyNA(.SD), .SDcols=equi_names_DT]  else anyNA(.DT[, equi_names_DT]) &&
    if (inherits(.i, "data.table")) .i[, anyNA(.SD), .SDcols=equi_names_i]  else anyNA(.i[, equi_names_i])

  # ----------------------------------------------------------------------------
  # jointext

  if (!has_mult.DT) {

    .DTtext <- if (outer.DT) ".DT[, fjoin.DT.rn := .I]" else ".DT"
    .itext  <- ".i"
    if (screen_NAs) {
      if (!outer.i && na_omit_cost(.DT) > na_omit_cost(.i)) {
        .itext <- sprintf("na.omit(%s, cols = %s)", .itext, deparse(equi_names_i))
      } else {
        # one-sided or .i smaller
        .DTtext <- sprintf("na.omit(%s, cols = %s)", .DTtext, deparse(equi_names_DT))
      }
    }
    jointext <-
      sprintf("setDT(%s%s[%s, on = %s, %s%s%s%s%s])[]",
              .DTtext,
              argtext_indicate,
              .itext,
              deparse(on),
              argtext_nomatch,
              argtext_mult,
              jtext,
              if (!has_mult) ", allow.cartesian = TRUE" else "",
              argtext_verbose)

  } else if (mult == "all") {
    # mult.DT but not mult

    .DTtext <- if (outer.DT) ".DT[, fjoin.DT.rn := .I]" else ".DT"
    .itext  <- ".i[, fjoin.i.rn := .I]"
    if (screen_NAs) {
      # can be either (even if outer wrt .i) as long we na.omit in the right place
      if (na_omit_cost(.DT) > na_omit_cost(.i)) {
        .itext <- sprintf("na.omit(%s, cols = %s)", .itext, deparse(equi_names_i))
      } else {
        .DTtext <- sprintf("na.omit(%s, cols = %s)", .DTtext, deparse(equi_names_DT))
      }
    }
    jointext <-
      sprintf("setDT(setDT(%s[%s, on = %s, nomatch = NULL, mult = %s, data.frame(%s%s, fjoin.i.rn)%s])[%s, on = \"fjoin.i.rn\", %s%s%s])[]",
              .itext,
              .DTtext,
              deparse(flip_on(on)),
              deparse(mult.DT),
              paste(sapply(names_DT[include_DT], \(x) sprintf("%s = i.%s",x,x)), collapse = ", "),
              if (add_DT_dummy_col) ", fjoin.ind = TRUE" else "",
              argtext_verbose,
              ".i",                # TODO: make variable
              argtext_nomatch,
              jtext,
              argtext_verbose)

  } else {
    # both mult.DT and mult - solution depends on whether outer wrt .i

    if (!outer.i) {
      # inner wrt .i
      .DTtext <- ".DT[, fjoin.DT.rn := .I]"
      .itext  <- ".i"
      if (screen_NAs) {
        # can be either (even if inner) as long we na.omit in the right place
        if (na_omit_cost(.DT) > na_omit_cost(.i)) {
          .itext <- sprintf("na.omit(%s, cols = %s)", .itext, deparse(equi_names_i))
        } else {
          .DTtext <- sprintf("na.omit(%s, cols = %s)", .DTtext, deparse(equi_names_DT))
        }
      }
      jointext <-
        sprintf("setDT(setDT(%s[%s, on = %s, nomatch = NULL, %s%s%s])[%s%s]%s)[]",
        .DTtext,
                .itext,
                deparse(on),
                argtext_mult,
                sprintf("data.frame(%s%s%s)",
                        paste(jvars, collapse = ", "),
                        if (outer.DT) "" else ", fjoin.DT.rn",
                        if (add_DT_dummy_col) ", fjoin.ind = TRUE" else ""),
                argtext_verbose,
                if (mult.DT=="first") {
                  ", first(.SD), by = \"fjoin.DT.rn\""
                } else {
                  "!duplicated(fjoin.DT.rn, fromLast=TRUE)"
                },
                argtext_verbose,
                if (outer.DT) "" else "[, fjoin.DT.rn := NULL][]")
    } else {
      # outer wrt .i
      .DTtext <- ".DT[, fjoin.DT.rn := .I]"
      .itext  <- ".i[, fjoin.i.rn := .I]"
      if (screen_NAs) .DTtext <- sprintf("na.omit(%s, cols = %s)", .DTtext, deparse(equi_names_DT))
      jointext <-
        sprintf("setDT(%s[%s, on = %s, nomatch = NULL, %sdata.frame(%s%s%s, fjoin.i.rn)%s])[%s%s][.i, on = \"fjoin.i.rn\", %s%s]",
                .DTtext,
                .itext,
                deparse(on),
                argtext_mult,
                paste(sapply(names_DT[include_DT], \(x) sprintf("%s = x.%s",x,x)), collapse = ", "),
                if (outer.DT) "" else ", fjoin.DT.rn",
                if (add_DT_dummy_col) ", fjoin.ind = TRUE" else "",
                argtext_verbose,
                if (mult.DT=="first") {
                  ", first(.SD), by = \"fjoin.DT.rn\""
                } else {
                  "!duplicated(fjoin.DT.rn, fromLast=TRUE)"
                },
                argtext_verbose,
                jtext,
                argtext_verbose)
    }
  }

  if (outer.DT) {
    if (all(include_DT)) {
      .DTantitext <- ".DT[!temp$fjoin.DT.rn]"
    } else {
      # NB not include_DT as need potentially excluded join columns
      .DTantitext <- sprintf("setDT(.DT[!temp$fjoin.DT.rn, data.frame(%s)])", paste(names_DT[include_DT_anti], collapse = ", "))
    }
    if (rename.DT_anti) .DTantitext <-
      sprintf("setnames(%s, %s, %s)", .DTantitext, deparse(oldnames_DT_anti), deparse(newnames_DT_anti))
    if (indicate) .DTantitext <-
      sprintf("%s[, .join := %s]", .DTantitext, if (!i.main) "1L" else "2L")
    jointext <- sprintf("with(list(temp = %s), rbind(temp, %s, fill = TRUE))[, fjoin.DT.rn := NULL][]", jointext, .DTantitext)
  }

  # --------------------------------------------------------------------------

  cat("\n", ".DT :", .labels[[1]], "\n", ".i  :", .labels[[2]])
  cat("\n", "Join:", jointext, "\n\n")

  if (do) {
    if (asis.DT) on.exit(clean_up(.DT), add = TRUE)
    if (asis.i) on.exit(clean_up(.i), add = TRUE)
    return(eval(parse(text = jointext),
                envir = list2env(list(.DT = .DT, .i = .i),
                                 parent = getNamespace("data.table"))))
  }
}

