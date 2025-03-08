#' Return the semi-join of the \code{i}-table in a \code{DT[i]}-style data.table
#' join
#'
#' @description Write (and optionally run) \code{data.table} code to return the
#' semi-join of the \code{i}-table in an enhanced \code{DT[i]}-style join.
#' Arguments are as for \code{dtjoin}, except for those controlling the output
#' columns, which do not apply.
#'
#' @inheritParams dtjoin
#' @param nomatch,nomatch.DT Permitted for consistency with \code{dtjoin} but
#'   have no effect on the resulting semi-join.
#'
#' @returns A \code{data.table} (the resulting semi-join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TODO
#'
#' @export
dtjoin_semi_i <- function(
    .DT        = NULL,
    .i         = NULL,
    on,
    match.na   = FALSE,
    mult       = "all",
    mult.DT    = "all",
    nomatch    = NULL,
    nomatch.DT = NULL,
    do         = !(is.null(.DT) && is.null(.i)),
    verbose    = FALSE
) {

  check_TF(match.na)
  check_mult(mult)
  check_mult(mult.DT)
  check_nomatch(nomatch)
  check_nomatch(nomatch.DT)
  check_TF(do)
  check_TF(verbose)

  check_setup(do, .DT, .i)

  mock        <- is.null(.DT) && is.null(.i)
  has_mult    <- mult != "all"
  has_mult.DT <- mult.DT != "all"

  on <- clean_on(on)

  # ----------------------------------------------------------------------------

  if (mock) {
    tmp <- make_mock_tables(on)
    .DT <- tmp[[1]]
    .i  <- tmp[[2]]
  }

  # ----------------------------------------------------------------------------

  is_joincol_DT <- rep(FALSE, length(names(.DT)))
  is_joincol_i <- rep(FALSE, length(names(.i)))

  if (!match.na) {
    equi_names_DT <- character(0)
    equi_names_i  <- character(0)
  }

  for (i in seq_along(on)) {

    s <- strsplit_predicate(on[i])

    idx_DT <- match(s[1], names(.DT))
    if (is.na(idx_DT)) stop(sprintf("No column named \"%s\" found in `.DT`", s[1]))

    idx_i <- match(s[3], names(.i))
    if (is.na(idx_i)) stop(sprintf("No column named \"%s\" found in `.i`", s[3]))

    is_joincol_DT[idx_DT] <- TRUE
    is_joincol_i[idx_i] <- TRUE

    if (!match.na && s[2] == "==") {
      equi_names_DT <- c(equi_names_DT, s[1])
      equi_names_i  <- c(equi_names_i, s[3])
    }
  }

  jvars <- as.character(names(.i))
  # common cols will get i's values if join cols but otherwise e.g. c=i.c or will get .DT's values
  jvars <- ifelse(!is_joincol_i & jvars %in% names(.DT), sprintf("%s = i.%s",jvars,jvars), jvars)
  # if (on.first) jvars <- c(jvars[is_joincol_i], jvars[!is_joincol_i])
  jtext <- paste0("setDT(data.frame(", paste(jvars, collapse = ", "), "))")

  # ----------------------------------------------------------------------------

  # # mult "all" to "first" if a join (not use_in)
  # if (!use_in) argtext_mult <- sprintf("mult = %s, ", if (!has_mult) "\"first\"" else deparse(mult))
  argtext_verbose <- if (verbose) ", verbose = TRUE" else ""

  screen_NAs <-
    !match.na &&
    length(equi_names_DT) &&
    if (inherits(.DT, "data.table")) .DT[, anyNA(.SD), .SDcols=equi_names_DT]  else anyNA(.DT[, equi_names_DT]) &&
    if (inherits(.i, "data.table")) .i[, anyNA(.SD), .SDcols=equi_names_i]  else anyNA(.i[, equi_names_i])

  # ----------------------------------------------------------------------------

  if (!has_mult.DT) {
    # no mult.DT

    if (i == 1L && s[2] == "==") {
      # single equality - use %in% instead of join

      .DTtext <- sprintf(".DT$%s", s[1])
      .itext  <- ".i"

      if (screen_NAs) {
        # we can na.omit on .i if we like as those rows can't be matches
        if (na_omit_cost_rc(nrow(.DT), 1L) > na_omit_cost(.i)) {
          .itext  <- sprintf("na.omit(%s, cols = \"%s\")", .itext, s[3])
        } else {
          .DTtext <- sprintf("na.omit(%s)", .DTtext)
        }
      }

      jointext <-
        sprintf("%s[%s %%in%% %s%s]", .itext, s[3], .DTtext, argtext_verbose)

    } else {
      # general case

      .DTtext <- ".DT"
      .itext  <- ".i"

      if (screen_NAs) {
        if (na_omit_cost_rc(nrow(.DT), length(equi_names_DT)) > na_omit_cost(.i)) {
          .itext  <- sprintf("na.omit(%s, cols = %s)", .itext, deparse(equi_names_i))
        } else {
          .DTtext <- sprintf("%s[, na.omit(.SD, cols = %s), .SDcols = %s]",
                             .DTtext, deparse(equi_names_DT), deparse(names(.DT)[is_joincol_DT]))
        }
      }

      jointext <-
        sprintf("%s[%s, on = %s, nomatch = NULL, mult = %s, %s%s]",
                .DTtext,
                .itext,
                deparse(on),
                if (has_mult) deparse(mult) else "\"first\"",
                jtext,
                argtext_verbose)
    }

  } else if (!has_mult) {
    # no mult, mult.DT

    .DTtext <- ".DT"
    .itext  <- ".i"

    if (screen_NAs) {
      # can't na.omit on .i here
      .DTtext <- sprintf("%s[, na.omit(.SD, cols = %s), .SDcols = %s]",
                         .DTtext, deparse(equi_names_DT), deparse(names(.DT)[is_joincol_DT]))
    }

    jointext <-
      sprintf("%s[sort(unique(%s[%s, on = %s, nomatch = NULL, mult = %s, which = TRUE%s]))]",
              .itext,
              .itext,
              .DTtext,
              deparse(flip_on(on)),
              deparse(mult.DT),
              argtext_verbose)

  } else {
    # mult and mult.DT

    .DTtext <- ".DT"
    .itext  <- ".i"

    if (screen_NAs) {
      if (na_omit_cost_rc(nrow(.DT), length(equi_names_DT)) > na_omit_cost(.i)) {
        .itext  <- sprintf("na.omit(%s, cols = %s)", .itext, deparse(equi_names_i))
      } else {
        .DTtext <- sprintf("%s[, na.omit(.SD, cols = %s), .SDcols = %s]",
                           .DTtext, deparse(equi_names_DT), deparse(names(.DT)[is_joincol_DT]))
      }
    }

    jointext <-
      # NB fjoin.DT.rn here refers to .DT after na.omit if applicable
      sprintf("%s[, fjoin.DT.rn := .I][%s, on = %s, nomatch = NULL, mult = %s, %s%s][%s%s][, fjoin.DT.rn := NULL][]",
              .DTtext,
              .itext,
              deparse(on),
              deparse(mult),
              paste0("setDT(data.frame(", paste(c(jvars, "fjoin.DT.rn"), collapse = ", "), "))"),
              argtext_verbose,
              if (mult.DT=="first") {
                ", first(.SD), by = \"fjoin.DT.rn\""
              } else {
                "!duplicated(fjoin.DT.rn, fromLast=TRUE)"
              },
              argtext_verbose)
  }

  # ----------------------------------------------------------------------------

  if (!mock) {
    cat(".DT :", deparse(substitute(.DT)), "\n")
    cat(".i  :", deparse(substitute(.i)), "\n")
  }
  cat("Join:", jointext, "\n")

  if (do) eval(parse(text=jointext))
}
