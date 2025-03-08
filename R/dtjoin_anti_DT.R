#' Return the anti-join of the \code{DT}-table in a \code{DT[i]}-style data.table join
#'
#' @description
#' Write (and optionally run) \code{data.table} code to return the anti-join of
#' the \code{DT}-table in an enhanced \code{DT[i]}-style join. The arguments are
#' as for \code{dtjoin}, except for those controlling the output columns, which
#' do not apply here.
#'
#' @inheritParams dtjoin
#' @param nomatch,nomatch.DT Permitted for consistency with \code{dtjoin} but
#'   have no effect on the resulting anti-join.
#'
#' @returns A \code{data.table} (the resulting anti-join), or \code{NULL} if
#'   \code{do} is \code{FALSE}. The data.table code is always printed to the
#'   console.
#'
#' @examples
#' # TODO
#'
#' @export
dtjoin_anti_DT <- function(
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

  if (!match.na) equi_names_DT <- character(0)
  equi_names_i  <- character(0)

  for (i in seq_along(on)) {

    s <- strsplit_predicate(on[i])

    idx_DT <- match(s[1], names(.DT))
    if (is.na(idx_DT)) stop(sprintf("No column named \"%s\" found in `.DT`", s[1]))

    idx_i <- match(s[3], names(.i))
    if (is.na(idx_i)) stop(sprintf("No column named \"%s\" found in `.i`", s[3]))

    is_joincol_DT[idx_DT] <- TRUE
    is_joincol_i[idx_i]   <- TRUE

    if (s[2] == "==") {
      if (!match.na) equi_names_DT <- c(equi_names_DT, s[1])
      equi_names_i  <- c(equi_names_i, s[3])
    }
  }

  # ----------------------------------------------------------------------------

  argtext_verbose <- if (verbose) ", verbose = TRUE" else ""

  screen_NAs <-
    !match.na &&
    length(equi_names_DT) &&
    if (inherits(.DT, "data.table")) .DT[, anyNA(.SD), .SDcols=equi_names_DT]  else anyNA(.DT[, equi_names_DT]) &&
    if (inherits(.i, "data.table")) .i[, anyNA(.SD), .SDcols=equi_names_i]  else anyNA(.i[, equi_names_i])

  .DTtext  <- ".DT"
  .itext  <- ".i"

  if (sum(is_joincol_i) == length(equi_names_i)) {
    if (i == 1L && !(has_mult || has_mult.DT)) {
      .itext  <- sprintf("%s$%s", .itext, s[3])
      if (screen_NAs) .itext <- sprintf("na.omit(%s)", .itext)
    } else {
      if (screen_NAs) .itext <- sprintf("%s[, na.omit(.SD), .SDcols = %s]", .itext, deparse(equi_names_i))
    }
  } else {
    if (screen_NAs) .itext <- sprintf("%s[, na.omit(.SD, cols = %s), .SDcols = %s]", .itext, deparse(equi_names_i), deparse(names(.i)[is_joincol_i]))
  }

  jointext <-

    if (!(has_mult || has_mult.DT)) {
    # no mult or mult.DT

      if (i == 1L && s[2] == "==") {
        # single equality - not-in

        sprintf("%s[!%s %%in%% %s%s]",
                .DTtext, s[1],
                .itext,
                argtext_verbose)

      } else {
        # general case - not-join

        sprintf("%s[!%s, on = %s%s]",
                .DTtext,
                .itext,
                deparse(on),
                argtext_verbose)
      }

  } else if (has_mult) {
    # mult, with or without mult.DT - not-which

    sprintf("%s[!%s[%s, on = %s, nomatch = NULL, mult = %s, which = TRUE%s]]",
            .DTtext,
            .DTtext,
            .itext,
            deparse(on),
            deparse(mult),
            argtext_verbose)

  } else {
    # mult.DT, no mult - not-rn

    # NB could na.omit on .DT in this case
    # NB fjoin.DT.rn as variable to return vector
    sprintf("%s[!%s[%s[, fjoin.DT.rn := .I], on = %s, nomatch = NULL, mult = %s, fjoin.DT.rn%s]][, fjoin.DT.rn := NULL][]",
            .DTtext,
            .itext,
            .DTtext,
            deparse(flip_on(on)),
            deparse(mult.DT),
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
