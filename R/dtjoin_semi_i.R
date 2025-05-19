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

  if (!match.na) {
    equi_names.DT <- character(0)
    equi_names.i  <- character(0)
  }

  for (i in seq_along(on)) {

    s <- strsplit_predicate(on[i])

    idx.DT <- match(s[1], names(.DT))
    if (is.na(idx.DT)) stop(sprintf("No column named \"%s\" found in `.DT`", s[1]))

    idx.i <- match(s[3], names(.i))
    if (is.na(idx.i)) stop(sprintf("No column named \"%s\" found in `.i`", s[3]))

    is_joincol.DT[idx.DT] <- TRUE
    is_joincol.i[idx.i] <- TRUE

    if (!match.na && allows_equi(s[2])) {
      equi_names.DT <- c(equi_names.DT, s[1])
      equi_names.i  <- c(equi_names.i, s[3])
    }
  }

  # will create prefixed jvars on the fly in the cases where select-on-join is used
  included <- if (has_select) names.i[is_joincol.i | names.i %in% select] else names.i

  # ----------------------------------------------------------------------------

  argtext_verbose <- if (verbose) ", verbose = TRUE" else ""

  screen_NAs <-
    !match.na &&
    length(equi_names.DT) &&
    if (inherits(.DT, "data.table")) .DT[, anyNA(.SD), .SDcols=equi_names.DT]  else anyNA(.DT[, equi_names.DT]) &&
    if (inherits(.i, "data.table")) .i[, anyNA(.SD), .SDcols=equi_names.i]  else anyNA(.i[, equi_names.i])

  # ----------------------------------------------------------------------------

  if (!has_mult.DT) {
    # no mult.DT

    if (i == 1L && s[2] == "==") {
    # no mult.DT, single equality: not-in
    # TODO use %chin% if char

      # TODO in dtjoin, na.omit test should only cover selected .i as done below

      if (screen_NAs && na_omit_cost_rc(nrow(.DT), 1L) > na_omit_cost_rc(nrow(.i), length(included))) {
        # we can na.omit on .i if we like as those rows can't be matches
        .DTtext <- sprintf(".DT$%s", s[1])
        .itext  <- na_omit_text(".i", na_cols=s[3], sd_cols = if (has_select) included else NULL)
        jointext <-
          sprintf("%s[%s %s %s%s]",
                  .itext,
                  s[3],
                  if (is.character(s[1])) "%chin%" else "%in%",
                  .DTtext,
                  argtext_verbose)
        if (!as_DT) jointext <- sprintf("setDF(%s)[]", jointext) # very different from other cases
      } else {
        .DTtext <- sprintf("%s$%s", if (screen_NAs) na_omit_text(".DT", sd_cols=s[1]) else ".DT", s[1])
        .itext  <- ".i"
        jointext <-
          sprintf("%s[%s %s %s%s%s]",
                  .itext,
                  s[3],
                  if (is.character(s[1])) "%chin%" else "%in%",
                  .DTtext,
                  if (has_select) sprintf(", data.frame(%s)", paste(included, collapse = ",")) else "",
                  argtext_verbose)
        if (has_select) {
          if (as_DT) jointext <- sprintf("setDT(%s)[]", jointext)
        } else {
          if (!as_DT) jointext <- sprintf("setDF(%s)[]", jointext)
        }
      }

    } else {
      # no mult.DT, general case: inner join with mult for uniqueness

      .DTtext <- ".DT"
      .itext  <- ".i"
      if (screen_NAs) {
        if (na_omit_cost_rc(nrow(.DT), length(equi_names.DT)) > na_omit_cost_rc(nrow(.i), length(included))) {
          .itext  <- na_omit_text(.itext,
                                  na_cols=equi_names.i,
                                  sd_cols=if (has_select) included else NULL)
        } else {
          .DTtext <- na_omit_text(.DTtext,
                                  na_cols=equi_names.DT,
                                  sd_cols=names(.DT)[is_joincol.DT])
        }
      }
      jointext <-
        sprintf("%s[%s, on = %s, nomatch = NULL, mult = %s, data.frame(%s)%s]",
                .DTtext,
                .itext,
                deparse(on),
                if (has_mult) deparse(mult) else "\"first\"",
                paste(ifelse(included %in% names.DT, sprintf("%s = i.%s",included,included), included), collapse = ", "),
                argtext_verbose)
      if (as_DT) jointext <- sprintf("setDT(%s)[]", jointext)
    }

  } else if (!has_mult) {
    # no mult, mult.DT: select unique which

    .DTtext <- ".DT"
    .itext  <- ".i"
    if (screen_NAs) {
      # can't na.omit on .i here
      .DTtext <- na_omit_text(.DTtext,
                              na_cols=equi_names.DT,
                              sd_cols=names(.DT)[is_joincol.DT])
    }
    jointext <-
      sprintf("%s[fsort(as.numeric(unique(%s[%s, on = %s, nomatch = NULL, mult = %s, which = TRUE%s])))%s]",
              .itext,
              .itext,
              .DTtext,
              deparse(flip_on(on)),
              deparse(mult.DT),
              argtext_verbose,
              if (has_select) sprintf(", data.frame(%s)", paste(included, collapse = ", ")) else ""
              )
    if (has_select) {
      if (as_DT) jointext <- sprintf("setDT(%s)[]", jointext)
    } else {
      if (!as_DT) jointext <- sprintf("setDF(%s)[]", jointext)
    }

  } else {
    # mult and mult.DT: (complex)

    .DTtext <- ".DT"
    .itext  <- ".i"

    if (screen_NAs) {
      if (na_omit_cost_rc(nrow(.DT), length(equi_names.DT)) > na_omit_cost_rc(nrow(.i), length(included))) {
        .itext  <- na_omit_text(.itext,
                                na_cols=equi_names.i,
                                sd_cols=if (has_select) included else NULL)
      } else {
        .DTtext <- na_omit_text(.DTtext,
                                na_cols=equi_names.DT,
                                sd_cols=names(.DT)[is_joincol.DT])
      }
    }

    jointext <-
      # NB fjoin.DT.rn here refers to .DT after na.omit if applicable
      sprintf("setDT(%s[, fjoin.DT.rn := .I][%s, on = %s, nomatch = NULL, mult = %s, data.frame(%s)%s])[%s%s][, fjoin.DT.rn := NULL]",
              .DTtext,
              .itext,
              deparse(on),
              deparse(mult),
              paste(c(ifelse(included %in% names.DT, sprintf("%s = i.%s",included,included), included), "fjoin.DT.rn"), collapse = ", "),
              argtext_verbose,
              if (mult.DT=="first") {
                ", first(.SD), by = \"fjoin.DT.rn\""
              } else {
                "!duplicated(fjoin.DT.rn, fromLast=TRUE)"
              },
              argtext_verbose)
    jointext <- sprintf(if (as_DT) "%s[]" else "setDF(%s)[]", jointext)
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
