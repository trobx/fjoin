#' Cross-join data.frame-like objects using functional \code{DT[i]}-style syntax
#'
#' @description
#' Write (and optionally run) \code{data.table} code to return the cross-join of
#' two \code{data.frame}-like objects using an enhanced functional version of
#' \code{DT[i]}-style syntax. Arguments for selecting, ordering and prefixing
#' output columns are as for \code{\link{dtjoin}}.
#'
#' The function \code{\link{fjoin_cross}} provides a more conventional interface
#' that is recommended over \code{dtjoin_cross} for most users and cases.
#'
#' @inheritParams dtjoin
#'
#' @returns A \code{data.frame}, \code{data.table}, \code{tibble}, or
#'  \code{sf}/\code{sf}-\code{tibble} depending on the class of \code{.DT}, or
#'  else \code{NULL} if \code{do} is \code{FALSE}.
#'
#' @details
#' # TODO
#'
#' @examples
#' # TODO
#'
#' @export
dtjoin_cross <- function(
    .DT        = NULL,
    .i         = NULL,
    select     = NULL,
    select.DT  = NULL,
    select.i   = NULL,
    i.first    = i.main,
    i.main     = FALSE,
    prefix     = if (i.main) "x." else "i.",
    do         = !(is.null(.DT) && is.null(.i)),
    show       = !do,
    ...

) {

  check_arg_prefix(prefix)
  check_arg_select(select)
  check_arg_select(select.DT)
  check_arg_select(select.i)
  check_arg_TF(do)
  check_arg_TF(show)
  check_arg_TF(i.first)
  check_arg_TF(i.main)

  dot_args <- list(...)

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
    tmp   <- make_mock_tables(on = "id")
    .DT   <- tmp[[1]]
    .i    <- tmp[[2]]
    asis.DT <- TRUE
    asis.i  <- TRUE
  } else {
    check_input_class(.DT)
    check_input_class(.i)
    orig.DT           <- .DT
    orig.i            <- .i
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

  has_select    <- !is.null(select)
  has_select.DT <- !is.null(select.DT)
  has_select.i  <- !is.null(select.i)
  if (has_select) select <- unique(select)
  select.DT <- if (has_select) union(select, select.DT) else unique(select.DT)
  select.i  <- if (has_select) union(select, select.i)  else unique(select.i)

  # --------------------------------------------------------------------------

  names.DT <- unique(names(.DT))
  names.i  <- unique(names(.i))

  is_selected.DT <- if (is.null(select.DT)) rep(TRUE, length(names.DT)) else names.DT %in% select.DT
  is_selected.i  <- if (is.null(select.i)) rep(TRUE, length(names.i))  else names.i %in% select.i

  selected_names.DT <- if (is.null(select.DT)) names.DT else names.DT[is_selected.DT]
  selected_names.i  <- if (is.null(select.i)) names.i else names.i[is_selected.i]

  sfc_present <- any_inherits(.DT, "sfc", mask = is_selected.DT) || any_inherits(.i, "sfc", mask = is_selected.i)

  if (!do) {
    as_DT <- asis.DT || asis.i
  } else {
    as_sf <- as_tbl_df <- FALSE
    as_tibble_ok <- requireNamespace("tibble", quietly = TRUE)
    # sf/sf-tibble iff sfc col(s) present, sf installed, and .DT or .i is sf whose active geometry is selected
    if (sfc_present && requireNamespace("sf", quietly = TRUE)) {
      # .i ahead of .DT
      if (inherits(orig.i, "sf")) {
        sf_col <- attr(orig.i, "sf_column")
        if (is_selected.i[match(sf_col, names.i)]) {
          as_sf <- TRUE
          if (!i.main && sf_col %in% names.DT) sf_col <- sprintf("%s%s", prefix, sf_col)
          as_tbl_df <- inherits(orig.i, "tbl_df") && as_tibble_ok
        }
      }
      if (!as_sf && inherits(orig.DT, "sf")) {
        sf_col <- attr(orig.DT, "sf_column")
        if (is_selected.DT[match(sf_col, names.DT)]) {
          as_sf <- TRUE
          if (i.main && sf_col %in% names.i) sf_col <- sprintf("%s%s", prefix, sf_col)
          as_tbl_df <- inherits(orig.DT, "tbl_df") && as_tibble_ok
        }
      }
    }
    as_DT <- !as_sf && (asis.DT || asis.i)
    if (!as_sf && !as_DT) as_tbl_df <- (inherits(orig.DT, "tbl_df") || inherits(orig.i, "tbl_df")) && as_tibble_ok
  }

  jvars.DT <- character(length(selected_names.DT))
  jvars.i  <- character(length(selected_names.i))

  if (!i.main) {
    # (c,c) -> (c,PREF.c=i.c)
    jvars.DT <- if (sfc_present) sprintf("%s = %s",selected_names.DT,selected_names.DT) else selected_names.DT
    jvars.i  <- ifelse(selected_names.i %in% names.DT,
                       sprintf("%s%s = i.%s",prefix,selected_names.i,selected_names.i),
                       if (sfc_present) sprintf("%s = %s",selected_names.i,selected_names.i) else selected_names.i)
  } else {
    # (c,c) -> (PREF.c=c,c=i.c)
    jvars.DT <- ifelse(selected_names.DT %in% names.i,
                       sprintf("%s%s = %s",prefix,selected_names.DT,selected_names.DT),
                       if (sfc_present) sprintf("%s = %s",selected_names.DT,selected_names.DT) else selected_names.DT)
    jvars.i  <- ifelse(selected_names.i %in% names.DT,
                       sprintf("%s = i.%s",selected_names.i,selected_names.i),
                       if (sfc_present) sprintf("%s = %s",selected_names.i,selected_names.i) else selected_names.i)
  }

  jvars <-
    if (has_select && !(has_select.DT || has_select.i)) {
    # select-only case (selected in order)
      if (i.first) {
          stats::na.omit(unlist(lapply(select, function(x) c(jvars.i[match(x,selected_names.i)], jvars.DT[match(x,selected_names.DT)]))))
      } else {
          stats::na.omit(unlist(lapply(select, function(x) c(jvars.DT[match(x,selected_names.DT)], jvars.i[match(x,selected_names.i)]))))
      }
    } else {
        if (i.first) c(jvars.i, jvars.DT) else c(jvars.DT, jvars.i)
    }

  jtext <- sprintf(if (sfc_present) "setDF(list(%s))" else "data.frame(%s)", paste(jvars, collapse = ", "))
  jointext <- sprintf("%s[, fjoin.ind := TRUE][%s[, fjoin.ind := TRUE], on = \"fjoin.ind\", allow.cartesian = TRUE, %s]", ".DT", ".i", jtext)
  if (as_DT) jointext <- sprintf("setDT(%s)", jointext)

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
