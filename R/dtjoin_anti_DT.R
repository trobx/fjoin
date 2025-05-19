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
    select     = NULL,
    do         = !(is.null(.DT) && is.null(.i)),
    show       = !do,
    verbose    = FALSE,
    ...
) {

  # TODO: check_on(on)
  # TODO: check_select(select args)
  check_TF(match.na)
  check_mult(mult)
  check_mult(mult.DT)
  check_nomatch(nomatch)
  check_nomatch(nomatch.DT)
  check_TF(do)
  check_TF(show)
  check_TF(verbose)

  dot_args <- list(...)

  on   <- clean_on(on)
  mock <- is.null(.DT) && is.null(.i)
  do   <- !mock && do
  show <- show || !do

  if (mock) {
    tmp <- make_mock_tables(on)
    .DT <- tmp[[1]]
    .i  <- tmp[[2]]
    as_DT <- TRUE
  } else {
    check_input_class(.DT)
    check_input_class(.i)
    as_DT <- data.table::is.data.table(.DT)
    as_tbl_df <- !as_DT && inherits(.DT, "tbl_df") && any(c("package:dplyr", "package:tibble") %in% search())
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

  if (has_select <- !is.null(select)) select <- unique(select)

  has_mult    <- mult != "all"
  has_mult.DT <- mult.DT != "all"

  # ----------------------------------------------------------------------------

  names.DT      <- unique(names(.DT))
  is_joincol.DT <- rep(FALSE, length(names.DT))

  names.i      <- unique(names(.i))
  is_joincol.i <- rep(FALSE, length(names.i))

  if (!match.na) equi_names.DT <- character(0)
  equi_names.i  <- character(0)

  for (i in seq_along(on)) {

    s <- strsplit_predicate(on[i])

    idx.DT <- match(s[1], names(.DT))
    if (is.na(idx.DT)) stop(sprintf("No column named \"%s\" found in `.DT`", s[1]))

    idx.i <- match(s[3], names(.i))
    if (is.na(idx.i)) stop(sprintf("No column named \"%s\" found in `.i`", s[3]))

    is_joincol.DT[idx.DT] <- TRUE
    is_joincol.i[idx.i]   <- TRUE

    if (allows_equi(s[2])) {
      if (!match.na) equi_names.DT <- c(equi_names.DT, s[1])
      equi_names.i  <- c(equi_names.i, s[3])
    }
  }

  if (has_select) jtext <- sprintf("data.frame(%s)", paste(names.DT[is_joincol.DT | names.DT %in% select], collapse = ", "))

  # ----------------------------------------------------------------------------

  argtext_verbose <- if (verbose) ", verbose = TRUE" else ""

  screen_NAs <-
    !match.na &&
    length(equi_names.DT) &&
    if (inherits(.DT, "data.table")) .DT[, anyNA(.SD), .SDcols=equi_names.DT]  else anyNA(.DT[, equi_names.DT]) &&
    if (inherits(.i, "data.table")) .i[, anyNA(.SD), .SDcols=equi_names.i]  else anyNA(.i[, equi_names.i])

  .DTtext  <- ".DT"
  .itext   <- ".i"

  if (sum(is_joincol.i) == length(equi_names.i)) {
    if (i == 1L && !(has_mult || has_mult.DT)) {
      .itext <- sprintf("%s$%s",  if (screen_NAs) na_omit_text(.itext, sd_cols=s[3]) else .itext, s[3])
    } else {
      if (screen_NAs) .itext <- na_omit_text(.itext, sd_cols=equi_names.i)
    }
  } else {
    if (screen_NAs) .itext <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=names(.i)[is_joincol.i])
  }

  jointext <-

    if (!(has_mult || has_mult.DT)) {
    # no mult or mult.DT

      if (i == 1L && s[2] == "==") {
        # single equality: not-in
        # TODO use %chin% if char
        sprintf("%s[!%s %s %s%s%s]",
                .DTtext,
                s[1],
                if (is.character(s[1])) "%chin%" else "%in%",
                .itext,
                if (has_select) sprintf(", %s", jtext) else "",
                argtext_verbose)

      } else {
        # general case: not-join
        sprintf("%s[!%s, on = %s%s%s]",
                .DTtext,
                .itext,
                deparse(on),
                if (has_select) sprintf(", %s", jtext) else "",
                argtext_verbose)
      }

  } else if (has_mult) {
    # mult, with or without mult.DT: not-which
    sprintf("%s[!%s[%s, on = %s, nomatch = NULL, mult = %s, which = TRUE%s]%s]",
            .DTtext,
            .DTtext,
            .itext,
            deparse(on),
            deparse(mult),
            argtext_verbose,
            if (has_select) sprintf(", %s", jtext) else "")

  } else {
    # mult.DT, no mult: not-rn
    # NB could na.omit on .DT in this case
    # NB fjoin.DT.rn as variable to return vector
    sprintf("%s[!%s[%s[, fjoin.DT.rn := .I], on = %s, nomatch = NULL, mult = %s, fjoin.DT.rn%s]%s",
            .DTtext,
            .itext,
            .DTtext,
            deparse(flip_on(on)),
            deparse(mult.DT),
            argtext_verbose,
            if (has_select) sprintf(", %s]", jtext) else "][, fjoin.DT.rn := NULL][]")
  }

  # will be DF if has_select and DT otherwise
  if (has_select) {
    if (as_DT) jointext <- sprintf("setDT(%s)", jointext)
  } else {
    if (!as_DT) jointext <- sprintf("setDF(%s)", jointext)
  }


  # ----------------------------------------------------------------------------

  if (show) {
    cat(".DT : ", .labels[[1]], "\n", ".i  : ", .labels[[2]], "\n", "Join: ", jointext, "\n\n", sep="")
  }

  if (do) {
    if (asis.DT) on.exit(clean_up(.DT), add = TRUE)
    if (asis.i) on.exit(clean_up(.i), add = TRUE) # but no temp cols
    ans <- (eval(parse(text = jointext), envir = list2env(list(.DT = .DT, .i = .i), parent = getNamespace("data.table"))))
    return(if (as_tbl_df) tibble::as_tibble(ans) else ans)
  }

}
