#' Semi-join of \code{DT} in a \code{DT[i]}-style join of data.frame-like
#' objects
#'
#' @description
#' Write (and optionally run) data.table code to return the semi-join of
#' \code{DT} (the rows with at least one match) using an enhanced functional
#' version of \code{DT[i]}-style join syntax. Arguments are the same as for
#' \code{\link{dtjoin}} except those controlling the order and prefixing of
#' output columns, which do not apply.
#'
#' The functions \code{\link{fjoin_left_semi}} and \code{\link{fjoin_right_semi}}
#' provide a more conventional interface that is recommended over
#' \code{dtjoin_semi} for most users and cases.
#'
#' @inheritParams dtjoin
#' @param mult.DT Permitted for consistency with \code{dtjoin} but
#'   has no effect on the resulting semi-join.
#' @param nomatch,nomatch.DT Permitted for consistency with \code{dtjoin} but
#'   have no effect on the resulting semi-join.
#' @param select Character vector of columns of \code{.DT} to be selected.
#'   \code{NULL} (the default) selects all columns. Join columns are always
#'   selected.
#'
#' @returns A \code{data.frame}, \code{data.table}, \code{tibble}, or
#'  \code{sf}/\code{sf}-\code{tibble} depending on the class of \code{.DT}, or
#'  else \code{NULL} if \code{do} is \code{FALSE}.
#'
#' @examples
#' # TODO
#'
#' @export
dtjoin_semi <- function(
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

  check_arg_on(on)
  check_arg_TF(match.na)
  check_arg_mult(mult)
  check_arg_mult(mult.DT)
  check_arg_nomatch(nomatch)
  check_arg_nomatch(nomatch.DT)
  check_arg_select(select)
  check_arg_TF(do)
  check_arg_TF(show)
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
    tmp <- make_mock_tables(on)
    .DT <- tmp[[1]]
    .i  <- tmp[[2]]
    asis.DT <- TRUE
    asis.i  <- TRUE
  } else {
    check_input_class(.DT)
    check_input_class(.i)
    orig.DT           <- .DT
    asis.DT           <- inherits(.DT, "data.table")
    asis.i            <- inherits(.i, "data.table")
    if (!asis.DT) {
      .DT <- shallow_DT(.DT)
      if (show) .labels[[1]] <- paste(.labels[[1]], "(cast as data.table)")
    }
    if (!asis.i) {
      .i <- shallow_DT(.i)
      if (show) .labels[[2]] <- paste(.labels[[2]], "(cast as data.table)")
    }
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

  if (!match.na) {
    equi_names.DT  <- rep(NA_character_, length(on))
    equi_names.i  <- rep(NA_character_, length(on))
  }

  for (i in seq_along(on)) {

    s <- strsplit_predicate(on[i])

    idx.DT <- match(s[1], names.DT)
    if (is.na(idx.DT)) stop(sprintf("No column named \"%s\" found in `.DT`", s[1]))

    idx.i <- match(s[3], names.i)
    if (is.na(idx.i)) stop(sprintf("No column named \"%s\" found in `.i`", s[3]))

    is_joincol.DT[idx.DT] <- TRUE
    is_joincol.i[idx.i]   <- TRUE

    if (!match.na && allows_equi(s[2])) {
      equi_names.DT[[i]] <- s[1]
      equi_names.i[[i]]  <- s[3]
    }
  }

  if (!match.na) {
    equi_names.DT <- equi_names.DT[!is.na(equi_names.DT)]
    equi_names.i  <- equi_names.i[!is.na(equi_names.i)]
  }

  # will create prefixed jvars on the fly in the cases where select-on-join is used
  included <- if (has_select) names.DT[is_joincol.DT | names.DT %in% select] else names.DT

  screen_NAs <- !match.na && length(equi_names.DT) && .DT[, anyNA(.SD), .SDcols=equi_names.DT] && .i[, anyNA(.SD), .SDcols=equi_names.i]

  sfc_present <- any_inherits(.DT, "sfc", mask=names.DT %in% select)

  as_DT <- asis.DT
  if (do && !as_DT) {
    as_sf <- FALSE
    # sf/sf-tibble iff sfc col(s) present, sf installed, and .DT is sf whose active geometry is selected
    if (sfc_present && inherits(orig.DT, "sf") && requireNamespace("sf", quietly = TRUE)) {
      sf_col <- attr(orig.DT, "sf_column")
      if (!has_select || sf_col %in% select) as_sf <- TRUE
    }
    as_tbl_df <- inherits(orig.DT, "tbl_df") && requireNamespace("tibble", quietly = TRUE)
  }

  # ----------------------------------------------------------------------------

  argtext_verbose <- if (verbose) ", verbose = TRUE" else ""

  if (!has_mult) {

    if (i == 1L && s[2] == "==") {
    # (1) no mult, single equality: in

      if (screen_NAs && na_omit_cost_rc(nrow(.DT), 1L) > na_omit_cost_rc(nrow(.i), length(included))) {
        .DTtext <- na_omit_text(".DT", na_cols=s[1], sd_cols=if (has_select) included else NULL)
        .itext  <- sprintf(".i$%s", s[3])
        jointext <-
          sprintf("%s[%s %s %s%s]",
                  .DTtext,
                  s[1],
                  if (is.character(s[1])) "%chin%" else "%in%",
                  .itext,
                  argtext_verbose)
        if (!as_DT) jointext <- sprintf("setDF(%s)[]", jointext) # very different from other cases

      } else {

        .DTtext <- ".DT"
        .itext  <- sprintf("%s$%s", if (screen_NAs) na_omit_text(".i", sd_cols=s[3]) else ".i", s[3])
        jtext <-
          if (has_select) {
            if (sfc_present) {
              sprintf(", setDF(list(%s))", paste(sprintf("%s = %s",included,included), collapse=", "))
            } else {
              sprintf(", data.frame(%s)", paste(included, collapse=", "))
            }
          } else ""
        jointext <-
          sprintf("%s[%s %s %s%s%s]",
                  .DTtext,
                  s[1],
                  if (is.character(s[1])) "%chin%" else "%in%",
                  .itext,
                  jtext,
                  argtext_verbose)
        if (has_select) {
          if (as_DT) jointext <- sprintf("setDT(%s)[]", jointext)
        } else {
          if (!as_DT) jointext <- sprintf("setDF(%s)[]", jointext)
        }
      }
    } else {

      # (2) no mult, general case: flip tables and inner join with mult for uniqueness

      .DTtext <- ".DT"
      .itext  <- ".i"
      if (screen_NAs) {
        if (na_omit_cost_rc(nrow(.DT), length(included)) > na_omit_cost_rc(nrow(.i), length(equi_names.i))) {
          .itext  <- na_omit_text(.itext,
                                  na_cols=equi_names.i,
                                  sd_cols=names.i[is_joincol.i])
        } else {
          .DTtext <- na_omit_text(.DTtext,
                                  na_cols=equi_names.DT,
                                  sd_cols=if (has_select) included else NULL)
        }
      }
      jtext <- if (sfc_present) {
        sprintf("setDF(list(%s))", paste(sprintf(ifelse(included %in% names.DT, "%s = i.%s", "%s = %s"),included,included), collapse=", "))
      } else {
        sprintf("data.frame(%s)", paste(ifelse(included %in% names.DT, sprintf("%s = i.%s",included,included), included), collapse=", "))
      }
      jointext <-
        sprintf("%s[%s, on = %s, nomatch = NULL, mult = %s, %s%s]",
                .itext,
                .DTtext,
                deparse(flip_on(on)),
                if (has_mult.DT) deparse(mult.DT) else "\"first\"",
                jtext,
                argtext_verbose)

      if (as_DT) jointext <- sprintf("setDT(%s)[]", jointext)
    }

  } else {
    # (3) mult: select unique which

    .DTtext <- ".DT"
    .itext  <- ".i"
    if (screen_NAs) {
      .itext  <- na_omit_text(.itext,
                              na_cols=equi_names.i,
                              sd_cols=names.i[is_joincol.i])
    }
    jtext <-
      if (has_select) {
        if (sfc_present) {
          sprintf(", setDF(list(%s))", paste(sprintf("%s = %s",included,included), collapse=", "))
        } else {
          sprintf(", data.frame(%s)", paste(included, collapse=", "))
        }
      } else ""
    jointext <-
      sprintf("%s[fsort(as.numeric(unique(%s[%s, on = %s, nomatch = NULL, mult = %s, which = TRUE%s])))%s]",
              .DTtext,
              .DTtext,
              .itext,
              deparse(on),
              deparse(mult),
              argtext_verbose,
              jtext
              )
    if (has_select) {
      if (as_DT) jointext <- sprintf("setDT(%s)[]", jointext)
    } else {
      if (!as_DT) jointext <- sprintf("setDF(%s)[]", jointext)
    }
  }

  # ----------------------------------------------------------------------------

  if (show) {
    cat(".DT : ", .labels[[1]], "\n", ".i  : ", .labels[[2]], "\n", "Join: ", jointext, "\n\n", sep="")
  }

  if (do) {
    if (asis.DT) on.exit(clean_up(.DT), add=TRUE)
    if (asis.i) on.exit(clean_up(.i), add=TRUE)
    ans <- (eval(parse(text=jointext), envir=list2env(list(.DT=.DT, .i=.i), parent=getNamespace("data.table"))))
    if (!as_DT) {
      if (as_tbl_df) ans <- tibble::as_tibble(ans)
      if (as_sf)     ans <- sf::st_as_sf(ans, sf_column_name=sf_col)
    }
    if (sfc_present) ans <- refresh_sfc_cols(ans)
    ans
  }
}
