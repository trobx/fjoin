#' Semi-join of \code{DT} in a \code{DT[i]}-style join of data frame-like
#' objects
#'
#' @description
#' Write (and optionally run) \pkg{data.table} code to return the semi-join of
#' \code{DT} (the rows of \code{DT} that join with \code{i}) using a
#' generalisation of \code{DT[i]} syntax.
#'
#' The functions \code{\link{fjoin_left_semi}} and \code{\link{fjoin_right_semi}}
#' provide a more conventional interface that is recommended over
#' \code{dtjoin_semi} for most users and cases.
#'
#' @inherit dtjoin params return seealso
#'
#' @param mult.DT Permitted for consistency with \code{dtjoin} but
#'   has no effect on the resulting semi-join.
#' @param nomatch,nomatch.DT Permitted for consistency with \code{dtjoin} but
#'   have no effect on the resulting semi-join.
#' @param select Character vector of columns of \code{.DT} to be selected.
#'   \code{NULL} (the default) selects all columns. Join columns are always
#'   selected.
#'
#' @details
#' Details are as for \code{\link{dtjoin}} except for arguments controlling
#' the order and prefixing of output columns, which do not apply.
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

  # input-----------------------------------------------------------------------

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

  cols.DT <- data.table::setDT(list(name = unique(names(.DT))))
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

  # output class----------------------------------------------------------------

  as_DT <- asis.DT

  if (do) {

    if (as_DT) {
      set_key <- data.table::haskey(.DT)
      if (set_key) {
        key <- subset_while_in(data.table::key(.DT), selected_cols)
        if (is.null(key)) set_key <- FALSE
      }

    } else {
      as_tbl_df <- as_grouped_df <- as_sf <- FALSE
      # (grouped) tibble
      if (requireNamespace("dplyr", quietly = TRUE)) {
        if (inherits(orig.DT, "grouped_df")) {
          groups <- names(attr(orig.DT,"groups"))[-length(names(attr(orig.DT,"groups")))]
          groups <- groups[groups %in% selected_cols]
          as_grouped_df <- length(groups) > 0L
        }
        if (!as_grouped_df) as_tbl_df <- (inherits(orig.DT, "tbl_df"))
      }
      # sf data frame
      if (inherits(orig.DT, "sf") && requireNamespace("sf", quietly = TRUE)) {
        sf_col <- attr(orig.DT, "sf_column")
        as_sf <- sf_col %in% selected_cols
        agr <- fast_na.omit(attr(.DT, "agr"))
        if (length(agr) > 0L) agr <- agr[names(agr) %in% selected_cols]
        set_agr <- length(agr) > 0L
      }
    }
  }

  has_sfc <- any_inherits(.DT, "sfc", mask=if (has_select) cols.DT$is_selected else NULL)

  # jointext--------------------------------------------------------------------

  argtext_verbose <- if (verbose) ", verbose = TRUE" else ""

  if (!has_mult) {

    if (nrow(cols.on) == 1L && cols.on$op == "==") {
    # (1) no mult, single equality: in

      joincol.DT <- cols.on$joincol.DT
      joincol.i  <- cols.on$joincol.i

      if (screen_NAs && na_omit_cost_rc(nrow(.DT), length(selected_cols)) > na_omit_cost_rc(nrow(.i), 1L)) {
      # (1a)

        .DTtext <- na_omit_text(".DT", na_cols=joincol.DT, sd_cols=if (has_select) selected_cols else NULL)
        .itext  <- sprintf(".i$%s", joincol.i)
        jointext <-
          sprintf("%s[%s %s %s%s]",
                  .DTtext,
                  joincol.DT,
                  if (is.character(.DT[[joincol.DT]])) "%chin%" else "%in%",
                  .itext,
                  argtext_verbose)
        if (!as_DT) jointext <- sprintf("setDF(%s)[]", jointext)

      } else {
      # (1b)

        .DTtext <- ".DT"
        .itext  <- sprintf("%s$%s", if (screen_NAs) na_omit_text(".i", sd_cols=joincol.i) else ".i", joincol.i)
        jtext <-
          if (has_select) {
            if (has_sfc) {
              sprintf(", setDF(list(%s))", paste(sprintf("%s = %s",selected_cols,selected_cols), collapse=", "))
            } else {
              sprintf(", data.frame(%s)", paste(selected_cols, collapse=", "))
            }
          } else ""
        jointext <-
          sprintf("%s[%s %s %s%s%s]",
                  .DTtext,
                  joincol.DT,
                  if (is.character(.DT[[joincol.DT]])) "%chin%" else "%in%",
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
      # (2) no mult, general case: flip tables and inner join with mult

      .DTtext <- ".DT"
      .itext  <- ".i"
      if (screen_NAs) {
        if (na_omit_cost_rc(nrow(.DT), length(selected_cols)) > na_omit_cost_rc(nrow(.i), length(equi_names.i))) {
          .itext  <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=cols.on$joincol.i)
        } else {
          .DTtext <- na_omit_text(.DTtext, na_cols=equi_names.DT, sd_cols=if (has_select) selected_cols else NULL)
        }
      }
      jtext <- if (has_sfc) {
        sprintf("setDF(list(%s))", paste(sprintf(data.table::fifelse(selected_cols %in% names(.i), "%s = i.%s", "%s = %s"),selected_cols,selected_cols), collapse=", "))
      } else {
        sprintf("data.frame(%s)", paste(data.table::fifelse(selected_cols %in% names(.i), sprintf("%s = i.%s",selected_cols,selected_cols), selected_cols), collapse=", "))
      }
      jointext <-
        sprintf("%s[%s, on = %s, nomatch = NULL, mult = %s, %s%s]",
                .itext,
                .DTtext,
                deparse(on_df_to_vec(cols.on, flip=TRUE)),
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
      .itext  <- na_omit_text(.itext, na_cols=equi_names.i, sd_cols=cols.on$joincol.i)
    }
    jtext <-
      if (has_select) {
        if (has_sfc) {
          sprintf(", setDF(list(%s))", paste(sprintf("%s = %s",selected_cols,selected_cols), collapse=", "))
        } else {
          sprintf(", data.frame(%s)", paste(selected_cols, collapse=", "))
        }
      } else ""
    jointext <-
      sprintf("%s[fsort(as.numeric(unique(%s[%s, on = %s, nomatch = NULL, mult = %s, which = TRUE%s])))%s]",
              .DTtext,
              .DTtext,
              .itext,
              deparse(on_df_to_vec(cols.on)),
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
      if (as_sf) {
        ans <- sf::st_as_sf(ans, sf_column_name=sf_col, sfc_last=FALSE)
        if (set_agr) attr(ans, "agr")[names(agr)] <- agr
      }
    }
    if (has_sfc) ans <- refresh_sfc_cols(ans)
    ans
  }
}
