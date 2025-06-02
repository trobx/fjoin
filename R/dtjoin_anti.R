#' Anti-join of \code{DT} in a \code{DT[i]}-style join of data.frame-like
#' objects
#'
#' @description
#' Write (and optionally run) data.table code to return the anti-join of
#' \code{DT} (the rows with no match) using an enhanced functional version of
#' \code{DT[i]}-style syntax. Arguments are as for \link{dtjoin} except those
#' controlling the order and prefixing output columns, which do not apply.

#'
#' The functions \link{fjoin_left_anti} and \link{fjoin_right_anti} provide
#' a more conventional interface that is recommended over \code{dtjoin_anti}
#' for most users and cases.
#'
#' @inheritParams dtjoin_semi
#' @param nomatch,nomatch.DT Permitted for consistency with \code{dtjoin} but
#'   have no effect on the resulting anti-join.
#'
#' @returns A \code{data.frame}, \code{data.table}, \code{tibble}, or
#'  \code{sf}/\code{sf}-\code{tibble} depending on the class of \code{.DT}, or
#'  else \code{NULL} if \code{do} is \code{FALSE}.
#'
#' @examples
#' # TODO
#'
#' @export
dtjoin_anti <- function(
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
    if (do) {
      orig.DT           <- .DT
      asis.DT           <- inherits(.DT, "data.table")
      asis.i            <- inherits(.i, "data.table")
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

  has_select <- !is.null(select)
  if (has_select) select <- unique(select)

  has_mult    <- mult != "all"
  has_mult.DT <- mult.DT != "all"

  # ----------------------------------------------------------------------------

  names.DT      <- unique(names(.DT))
  is_joincol.DT <- rep(FALSE, length(names.DT))

  names.i      <- unique(names(.i))
  is_joincol.i <- rep(FALSE, length(names.i))

  if (!match.na) equi_names.DT <- rep(NA_character_, length(on))
  equi_names.i  <- rep(NA_character_, length(on))

  for (i in seq_along(on)) {

    s <- strsplit_predicate(on[i])

    idx.DT <- match(s[1], names.DT)
    if (is.na(idx.DT)) stop(sprintf("No column named \"%s\" found in `.DT`", s[1]))

    idx.i <- match(s[3], names.i)
    if (is.na(idx.i)) stop(sprintf("No column named \"%s\" found in `.i`", s[3]))

    is_joincol.DT[idx.DT] <- TRUE
    is_joincol.i[idx.i]   <- TRUE

    if (allows_equi(s[2])) {
      if (!match.na) equi_names.DT[[i]] <- s[1]
      equi_names.i[[i]] <- s[3]
    }
  }

  if (!match.na) equi_names.DT <- equi_names.DT[!is.na(equi_names.DT)]
  equi_names.i  <- equi_names.i[!is.na(equi_names.i)]

  screen_NAs <- !match.na && length(equi_names.DT) && .DT[, anyNA(.SD), .SDcols=equi_names.DT] && .i[, anyNA(.SD), .SDcols=equi_names.i]

  sfc_present <- any_inherits(orig.DT, "sfc")

  as_DT <- asis.DT || !do
  if (!as_DT) {
    as_sf <- FALSE
    # sf/sf-tibble iff sfc col(s) present, sf installed, and .DT is sf whose active geometry is selected
    if (sfc_present && inherits(orig.DT, "sf") && requireNamespace("sf", quietly = TRUE)) {
      sf_col <- attr(orig.DT, "sf_column")
      if (!has_select || sf_col %in% select) as_sf <- TRUE
    }
    as_tbl_df <- inherits(orig.DT, "tbl_df") && requireNamespace("tibble", quietly = TRUE)
  }

  if (has_select) {
    jvars <- names.DT[is_joincol.DT | names.DT %in% select]
    if (sfc_present) {
      jvars <- sprintf("%s = %s", jvars, jvars)
      jtext <- sprintf("setDF(list(%s))", paste(jvars, collapse=", "))
    } else {
      jtext <- sprintf("data.frame(%s)", paste(jvars, collapse = ", "))
    }
  }

  # ----------------------------------------------------------------------------

  argtext_verbose <- if (verbose) ", verbose = TRUE" else ""

  .DTtext  <- ".DT"
  .itext   <- ".i"

  if (sum(is_joincol.i) == length(equi_names.i)) {
    if (i == 1L && !(has_mult || has_mult.DT)) {
      .itext <- sprintf("%s$%s",  if (screen_NAs) na_omit_text(.itext, sd_cols=s[3]) else .itext, s[3])
    } else {
      if (screen_NAs) .itext <- na_omit_text(.itext, sd_cols=equi_names.i)
    }
  } else {
    if (screen_NAs) .itext <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=names.i[is_joincol.i])
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
    if (asis.DT) on.exit(clean_up(.DT), add=TRUE)
    if (asis.i) on.exit(clean_up(.i), add=TRUE)
    ans <- (eval(parse(text=jointext), envir=list2env(list(.DT=.DT, .i=.i), parent=getNamespace("data.table"))))
    if (sfc_present) ans <- recompute_sfc_bboxes(ans)
    if (!as_DT) {
      if (as_tbl_df) ans <- tibble::as_tibble(ans)
      if (as_sf)     ans <- sf::st_as_sf(ans, sf_column_name=sf_col)
    }
    ans
  }
}
