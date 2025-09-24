#' Anti-join of \code{DT} in a \code{DT[i]}-style join of data.frame-like
#' objects
#'
#' @description
#' Write (and optionally run) \pkg{data.table} code to return the anti-join of
#' \code{DT} (the rows of \code{DT} not joining with \code{i}) using a
#' generalisation of \code{DT[i]} syntax.
#'
#' The functions \code{\link{fjoin_left_anti}} and \code{\link{fjoin_right_anti}}
#' provide a more conventional interface that is recommended over
#' \code{dtjoin_anti} for most users and cases.
#'
#' @inherit dtjoin_semi params return details seealso
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

  # input----------------------------------------------------------------------

  check_names(.DT)
  check_names(.i)
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

  has_select <- !is.null(select)
  if (has_select) select <- unique(select)

  has_mult    <- mult != "all"
  has_mult.DT <- mult.DT != "all"

  # cols.on, cols.DT------------------------------------------------------------

  cols.DT        <- data.table::setDT(list(name = unique(names(.DT))))
  cols.on$idx.DT <- match(cols.on$joincol.DT, cols.DT$name)
  if (anyNA(cols.on$idx.DT)) stop(
    paste("Join column(s) not found in `.DT`:",
          paste(cols.on[is.na(cols.on$idx.DT),"joincol.DT"], collapse = ", "))
  )
  cols.DT$is_joincol <- FALSE
  data.table::set(cols.DT, cols.on$idx.DT, "is_joincol", TRUE)
  if (has_select) cols.DT$is_selected <- cols.DT$is_joincol | cols.DT$name %in% select
  selected_cols <- if (has_select) cols.DT$name[cols.DT$is_selected] else cols.DT$name

  if (any(!cols.on$joincol.i %in% names(.i))) stop(
    paste("Join column(s) not found in `.i`:",
          paste(cols.on$joincol.i[!cols.on$joincol.i %in% names(.i)], collapse = ", "))

  )

  # screen_NAs, equi_names_-----------------------------------------------------

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

  # sfc_present-----------------------------------------------------------------

  sfc_present <- any_inherits(.DT, "sfc", mask=if (has_select) cols.DT$is_selected else NULL)

  # output class----------------------------------------------------------------

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

  # output key------------------------------------------------------------------

  set_key <- as_DT && data.table::haskey(.DT)
  if (set_key) {
    key <- subset_while_in(data.table::key(.DT), selected_cols)
    if (is.null(key)) set_key <- FALSE
  }

  # jvars, jtext (if selective)-------------------------------------------------

  if (has_select) {
    jvars <- selected_cols
    if (sfc_present) {
      jvars <- sprintf("%s = %s", jvars, jvars)
      jtext <- sprintf("setDF(list(%s))", paste(jvars, collapse=", "))
    } else {
      jtext <- sprintf("data.frame(%s)", paste(jvars, collapse = ", "))
    }
  }

  # jointext--------------------------------------------------------------------

  argtext_verbose <- if (verbose) ", verbose = TRUE" else ""

  .DTtext  <- ".DT"
  .itext   <- ".i"

  if (screen_NAs) {
    .itext <-
      if (identical(cols.on$op, "==") && !(has_mult || has_mult.DT)) {
        sprintf("%s$%s", na_omit_text(.itext, sd_cols=cols.on$joincol.i), cols.on$joincol.i)
      } else if (all(allows_equi)) {
        na_omit_text(.itext, sd_cols=equi_names.i)
      } else {
        na_omit_text(.itext, na_cols=equi_names.i, sd_cols=cols.on$joincol.i)
      }
  } else {
    if (identical(cols.on$op, "==") && !(has_mult || has_mult.DT)) {
      .itext <- sprintf("%s$%s", .itext, cols.on$joincol.i)
    }
  }

  jointext <-

    if (!(has_mult || has_mult.DT)) {
    # no mult or mult.DT

      if (identical(cols.on$op, "==")) {
        # (1) single equality: not-in
        sprintf("%s[!%s %s %s%s%s]",
                .DTtext,
                cols.on$joincol.DT,
                if (is.character(.DT[[cols.on$joincol.DT]])) "%chin%" else "%in%",
                .itext,
                if (has_select) sprintf(", %s", jtext) else "",
                argtext_verbose)

      } else {
        # (2) general case: not-join
        sprintf("%s[!%s, on = %s%s%s]",
                .DTtext,
                .itext,
                deparse(on_df_to_vec(cols.on)),
                if (has_select) sprintf(", %s", jtext) else "",
                argtext_verbose)
      }

  } else if (has_mult) {
    # (3) mult, with or without mult.DT: not-which
    sprintf("%s[!%s[%s, on = %s, nomatch = NULL, mult = %s, which = TRUE%s]%s]",
            .DTtext,
            .DTtext,
            .itext,
            deparse(on_df_to_vec(cols.on)),
            deparse(mult),
            argtext_verbose,
            if (has_select) sprintf(", %s", jtext) else "")

  } else {
    # (4) mult.DT, no mult: not-rn
    # NB could na.omit on .DT in this case
    sprintf("%s[!%s[%s[, fjoin.which.DT := .I], on = %s, nomatch = NULL, mult = %s, fjoin.which.DT%s]%s",
            .DTtext,
            .itext,
            .DTtext,
            deparse(on_df_to_vec(cols.on, flip=TRUE)),
            deparse(mult.DT),
            argtext_verbose,
            if (has_select) sprintf(", %s]", jtext) else "][, fjoin.which.DT := NULL][]")
  }

  # will be DF if has_select and DT otherwise
  if (has_select) {
    if (as_DT) jointext <- sprintf("setDT(%s)[]", jointext)
  } else {
    if (!as_DT) jointext <- sprintf("setDF(%s)[]", jointext)
  }

  # outputs---------------------------------------------------------------------

  if (show) {
    cat(".DT : ", .labels[[1]], "\n", ".i  : ", .labels[[2]], "\n", "Join: ", jointext, "\n\n", sep="")
  }

  if (do) {
    if (asis.DT) on.exit(drop_temp_cols(.DT), add=TRUE)
    if (asis.i) on.exit(drop_temp_cols(.i), add=TRUE)
    ans <- (eval(parse(text=jointext), envir=list2env(list(.DT=.DT, .i=.i), parent=getNamespace("data.table"))))
    if (as_DT) {
      if (set_key) attr(ans, "sorted") <- key
    } else{
      if (as_tbl_df) ans <- tibble::as_tibble(ans)
      if (as_sf)     ans <- sf::st_as_sf(ans, sf_column_name=sf_col, sfc_last=FALSE)
    }
    if (sfc_present) ans <- refresh_sfc_cols(ans)
    ans
  }
}
