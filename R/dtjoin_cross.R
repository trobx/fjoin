#' Cross join of data frame-like objects \code{DT} and \code{i} using
#' a \code{DT[i]}-style interface to data.table
#'
#' @description
#' Write (and optionally run) \code{data.table} code to return the cross join of
#' two \code{data.frame}-like objects using a generalisation of \code{DT[i]}
#' syntax.
#'
#' The function \code{\link{fjoin_cross}} provides a more conventional interface
#' that is recommended over \code{dtjoin_cross} for most users and cases.
#'
#' @inherit dtjoin params return seealso
#'
#' @details
#' Details are as for \code{\link{dtjoin}} except for remarks about join
#' columns and matching logic, which do not apply.
#'
#' @examples
#' # data frames
#' df1 <- data.table::fread(data.table = FALSE, input = "
#' bread    kcal
#' Brown     150
#' White     180
#' Baguette  250
#' ")
#'
#' df2 <- data.table::fread(data.table = FALSE, input = "
#' filling kcal
#' Cheese   200
#' Pâté     160
#' ")
#'
#' dtjoin_cross(df1, df2)
#'
#' @export
dtjoin_cross <- function(
  .DT       = NULL,
  .i        = NULL,
  select    = NULL,
  select.DT = NULL,
  select.i  = NULL,
  i.home    = FALSE,
  i.first   = i.home,
  prefix    = if (i.home) "x." else "i.",
  i.class   = i.home,
  do        = !(is.null(.DT) && is.null(.i)),
  show      = !do,
  ...
) {

  # input-----------------------------------------------------------------------

  check_names(.DT)
  check_names(.i)
  check_arg_prefix(prefix)
  check_arg_select(select)
  check_arg_select(select.DT)
  check_arg_select(select.i)
  check_arg_TF(do)
  check_arg_TF(show)
  check_arg_TF(i.first)
  check_arg_TF(i.home)
  check_arg_TF(i.class)

  dots <- list(...)
  check_dots_names(dots)

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
    tmp   <- make_mock_tables(on_vec_to_df("id"))
    .DT   <- tmp[[1]]
    .i    <- tmp[[2]]
    asis.DT <- TRUE
    asis.i  <- TRUE
  } else {
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
  if (has_select) {
    select    <- unique(select)
    select.DT <- if (has_select.DT) c(select, unique(select.DT[!select.DT %in% select])) else select
    select.i  <- if (has_select.i) c(select, unique(select.i[!select.i %in% select])) else select
  } else {
    if (has_select.DT) select.DT <- unique(select.DT)
    if (has_select.i)  select.i <- unique(select.i)
  }

  # cols.DT, cols.i, has_sfc----------------------------------------------------

  cols.DT <- data.table::setDT(list(name = unique(names(.DT))))
  cols.i  <- data.table::setDT(list(name = unique(names(.i))))

  cols.DT$is_selected <- if (is.null(select.DT)) TRUE else cols.DT$name %in% select.DT
  cols.i$is_selected  <- if (is.null(select.i)) TRUE else cols.i$name %in% select.i

  has_sfc <-
    requireNamespace("sf", quietly = TRUE) &&
    (any_inherits(.DT, "sfc", mask=cols.DT$is_selected) || any_inherits(.i, "sfc", mask=cols.i$is_selected))

  cols.DT$jvar <- NA_character_
  cols.i$jvar  <- NA_character_

  selected_cols.DT <- if (is.null(select.DT)) cols.DT$name else cols.DT$name[cols.DT$is_selected]
  selected_cols.i  <- if (is.null(select.i)) cols.i$name else cols.i$name[cols.i$is_selected]

  if (!i.home) {
    # (c,c) -> (c,PREF.c=i.c)
    cols.DT$jvar[cols.DT$is_selected] <-
      if (has_sfc) sprintf("%s = %s",selected_cols.DT,selected_cols.DT) else selected_cols.DT
    cols.i$jvar[cols.i$is_selected]   <-
      data.table::fifelse(selected_cols.i %in% cols.DT$name,
                          sprintf("%s%s = i.%s",prefix,selected_cols.i,selected_cols.i),
                          if (has_sfc) sprintf("%s = %s",selected_cols.i,selected_cols.i) else selected_cols.i)
  } else {
    # (c,c) -> (PREF.c=c,c=i.c)
    cols.DT$jvar[cols.DT$is_selected] <-
      data.table::fifelse(selected_cols.DT %in% cols.i$name,
                          sprintf("%s%s = %s",prefix,selected_cols.DT,selected_cols.DT),
                          if (has_sfc) sprintf("%s = %s",selected_cols.DT,selected_cols.DT) else selected_cols.DT)
    cols.i$jvar[cols.i$is_selected]   <-
      data.table::fifelse(selected_cols.i %in% cols.DT$name,
                          sprintf("%s = i.%s",selected_cols.i,selected_cols.i),
                          if (has_sfc) sprintf("%s = %s",selected_cols.i,selected_cols.i) else selected_cols.i)
  }

  # output class----------------------------------------------------------------

  as_DT <- if (i.class) asis.i else asis.DT

  if (do) {

    if (as_DT) {
      # key from .i always
      set_key <- asis.i && data.table::haskey(.i)
      if (set_key) {
        kcols <- subset_while_in(data.table::key(orig.i), selected_cols.i)
        if (is.null(kcols)) {
          set_key <- FALSE
        } else {
          key <- if (i.home) kcols else substr_until(cols.i$jvar[match(kcols, cols.i$name)], " = ")
        }
      }

    } else {
      as_tbl_df <- as_grouped_df <- as_sf <- FALSE
      whose_class <- if (i.class) orig.i else orig.DT
      whose_cols  <- if (i.class) cols.i else cols.DT
      # (grouped) tibble
      if (requireNamespace("dplyr", quietly = TRUE)) {
        if (inherits(whose_class, "grouped_df")) {
          groups <- names(attr(whose_class,"groups"))[-length(names(attr(whose_class,"groups")))]
          groups <- substr_until(fast_na.omit(whose_cols$jvar[match(groups, whose_cols$name)]), " = ")
          as_grouped_df <- length(groups) > 0L
        }
        if (!as_grouped_df) as_tbl_df <- (inherits(whose_class, "tbl_df"))
      }
      # sf data frame
      if (inherits(whose_class, "sf") && requireNamespace("sf", quietly = TRUE)) {
        sf_col_idx <- match(attr(whose_class, "sf_column"), whose_cols$name)
        if (whose_cols$is_selected[sf_col_idx]) {
          as_sf <- TRUE
          sf_col <- substr_until(whose_cols$jvar[sf_col_idx], until=" = ")
          # non-NA agr attribute values
          agr <- fast_na.omit(attr(whose_class, "agr"))
          set_agr <- length(agr) > 0L
          if (set_agr) {
            if (i.class == i.home) {
              agr <- agr[names(agr) %in% whose_cols$name[whose_cols$is_selected]]
            } else {
              cols.agr <- data.table::setDT(list(agr=agr, name=names(agr)))
              jvar <- NULL # for R CMD check
              cols.agr[whose_cols, on="name", jvar := jvar]
              agr <- cols.agr[!is.na(jvar), stats::setNames(agr, substr_until(jvar, " = "))]
            }
            if (length(agr) == 0L) set_agr <- FALSE
          }
        }
      }
    }
  }

  # jvars, jointext-------------------------------------------------------------

  jvars.DT <- cols.DT$jvar[cols.DT$is_selected]
  jvars.i  <- cols.i$jvar[cols.i$is_selected]

  if (has_select && !(has_select.DT || has_select.i)) {
    # select-only case (selected in order)

    # for each selected name, jvar or NA
    jvars.DT <- jvars.DT[match(select, selected_cols.DT)]
    jvars.i  <- jvars.i[match(select, selected_cols.i)]

    # interleave, dropping NAs (rbind then fast_na.omit which also flattens)
    jvars <-
      if (i.first) {
        fast_na.omit(rbind(jvars.i, jvars.DT))
      } else {
        fast_na.omit(rbind(jvars.DT, jvars.i))
      }

  } else {
    jvars <- if (i.first) c(jvars.i, jvars.DT) else c(jvars.DT, jvars.i)
  }

  jtext <- sprintf(if (has_sfc) "setDF(list(%s))" else "data.frame(%s)", paste(jvars, collapse = ", "))
  jointext <- sprintf("%s[, fjoin.ind := TRUE][%s[, fjoin.ind := TRUE], on = \"fjoin.ind\", allow.cartesian = TRUE, %s]", ".DT", ".i", jtext)
  if (as_DT) jointext <- sprintf("setDT(%s)", jointext)

  # outputs---------------------------------------------------------------------

  if (show) {
    cat(".DT : ", .labels[[1]], "\n", ".i  : ", .labels[[2]], "\n", "Join: ", jointext, "\n\n", sep="")
  }

  if (do) {
    if (asis.DT) on.exit(drop_temp_cols(.DT), add=TRUE)
    if (asis.i) on.exit(drop_temp_cols(.i), add=TRUE)
    ans <- eval(parse(text=jointext), envir=list2env(list(.DT=.DT, .i=.i), parent=getNamespace("data.table")))
    if (as_DT) {
      if (set_key) data.table::setattr(ans, "sorted", key)
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
