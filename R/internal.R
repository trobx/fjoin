# ------------------------------------------------------------------------------
valid_dots_names <- c(".labels")
check_dots_names <- function(dots, valid_names = valid_dots_names){
  invalid_names <- setdiff(names(dots), valid_names)
  if(length(invalid_names))
    stop("Invalid argument name(s): ", paste(invalid_names, collapse=", "))
}
check_arg_TF <- function(x) {
  if (!x %in% c(TRUE, FALSE))
    stop(sprintf("'%s' must be TRUE or FALSE", deparse(substitute(x))))
}
check_arg_order <- function(x) {
  if (!x %in% c("left", "right"))
    stop(sprintf("'%s' must be \"left\" or \"right\"", deparse(substitute(x))))
}
check_arg_on <- function(x) {
  if (!(is.character(x) && length(x) > 0L && all(nzchar(x)) && !anyNA(x)))
    stop(sprintf("'%s' must be a non-empty character vector with no empty strings or NAs", deparse(substitute(x))))
}
check_arg_select <- function(x) {
  if (!(is.null(x) || is.character(x) || ((length(x) == 1L && is.na(x)))))
    stop(sprintf("'%s' must be a character vector, NA, or NULL", deparse(substitute(x))))
}
check_arg_mult <- function(x) {
  if (!x %in% c("all", "first", "last"))
    stop(sprintf("'%s' must be \"all\", \"first\", or \"last\"", deparse(substitute(x))))
}
check_arg_nomatch <- function(x) {
  if (!(is.null(x) || x %in% c(NA, 0L)))
    stop(sprintf("'%s' must be NA, NULL, or 0L", deparse(substitute(x))))
}
check_arg_prefix <- function(x) {
  if (!(length(x) == 1 && isTRUE(make.names(x) == x)))
    stop(sprintf(
      paste("'%s' must be a single string of letters, digits, dots (.), and underscores (_)",
            "forming a syntactically valid name. See `?base::make.names` for a description."),
      deparse(substitute(x))))
}
check_names <- function(x) {
  if (!(isTRUE(all(make.names(names(x)) == names(x)))))
    stop(sprintf(
      paste("One or more column names in '%s' is either empty, NA, or not a",
            "syntactically valid R name (see `?base::make.names` for a description).",
            "A future version of fjoin should support non-valid names."),
      deparse(substitute(x))))
  if (any(grepl("^fjoin\\.", names(x))))
    stop(sprintf(
      "Column names beginning with \"fjoin.\" are reserved. Found in '%s': %s",
      deparse(substitute(x)),
      paste(names(x)[grep("^fjoin.", names(x))], collapse = ", ")))
}
check_input_class <- function(x) {
  # Check x is either a non-object list or data.frame (data.table etc.)
  if (!(is.list(x) && (is.data.frame(x) || !is.object(x))))
    stop(sprintf("'%s' must be a data.frame-like object or list", deparse(substitute(x))))
}
# ------------------------------------------------------------------------------
shallow_DT <- function(x) {
  # Shallow-copy columns of a data.frame-like object (or list of vectors) into a new DT
  # use setDT() for common length check and overallocation
  # unclass() doesn't (shallow) copy non-object
  data.table::setDT(if (is.object(x)) unclass(x) else as.list(x))
}
# ------------------------------------------------------------------------------
any_inherits <- function (x, cls, mask = NULL) {
  # Whether any cols of x (optionally masked) have given class
  if (is.null(mask)) {
    for (v in x) if (inherits(v, cls)) return(TRUE)
  } else {
    for (i in seq_along(x)) if (mask[i] && inherits(x[[i]], cls)) return(TRUE)
  }
  FALSE
}
# ------------------------------------------------------------------------------
drop_temp_cols <- function(x, pattern = "^fjoin\\.") {
  # Drop any columns (from a data.table input used as-is) with name starting "fjoin."
  # suppressWarnings() is innocuous and convenient here
  suppressWarnings(data.table::set(x, j = grep(pattern, names(x)), value = NULL))
}
# ------------------------------------------------------------------------------
refresh_sfc_cols <- function(x) {
  # update all sfc-class columns (bbox and n_empty attributes, and NULL to EMPTY)
  for (i in seq_along(x)) if (inherits(x[[i]], "sfc")) x[[i]] <- sf::st_sfc(x[[i]], recompute_bbox=TRUE)
  x
}
# ------------------------------------------------------------------------------
na_omit_cost <- function(dt) {
  na_omit_cost_rc(nrow(dt), ncol(dt))
}
na_omit_cost_rc <- function(nr, nc) {
  # Heuristic for cost of na.omit.data.table()
  # Based on regression analysis of execution time with a particular setup and machine
  (10L + nc) * (nr / 1e9L)
}
# ------------------------------------------------------------------------------
na_omit_text <- function(x, na_cols=NULL, sd_cols=NULL) {
  # A call to na.omit.data.table() as unparsed text
  sd_cols <- unique(sd_cols)
  na_cols <- unique(na_cols)
  if (is.null(sd_cols)) {
    if (is.null(na_cols)) {
      sprintf("na.omit(%s)", x)
    } else {
      sprintf("na.omit(%s, cols = %s)", x, deparse(na_cols))
    }
  } else {
    if (is.null(na_cols) || identical(na_cols, sd_cols)) {
      sprintf("%s[, na.omit(.SD), .SDcols = %s]", x, deparse(sd_cols))
    } else {
      sprintf("%s[, na.omit(.SD, cols = %s), .SDcols = %s]", x, deparse(na_cols), deparse(sd_cols))
    }
  }
}
# ------------------------------------------------------------------------------
make_mock_tables <- function(df) {
  # Create mock data.tables from a dataframe of join predicates
  names_DT <- c(df$joincol.DT, "col_DT", "col_c")
  names_i  <- c(df$joincol.i, "col_i", "col_c")
  .DT <- data.table::setnames(data.table::as.data.table(matrix(NA_integer_, nrow=1L, ncol=length(names_DT))), names_DT)
  .i  <- data.table::setnames(data.table::as.data.table(matrix(NA_integer_, nrow=1L, ncol=length(names_i))), names_i)
  list(.DT, .i)
}
# ------------------------------------------------------------------------------
make_label_fjoin <- function(t, sub_t) {
  # for calling in fjoin_*(): table label for printing, e.g. "x = A", "x (unnamed)"
  paste0(deparse(substitute(t)), if (!is.null(t) & is.name(sub_t)) sprintf(" = %s", deparse(sub_t)) else " (unnamed)")
}
make_label_dtjoin <- function(t, sub_t) {
  # for calling in dtjoin*(): table label for printing, e.g. "A", "(unnamed)"
  if (!is.null(t) & is.name(sub_t)) deparse(sub_t) else "(unnamed)"
}
# ------------------------------------------------------------------------------
fast_na.omit <- function(x) {
  # Also flattens x to a vector if a matrix or data.frame
  x[!is.na(x)]
}
# ------------------------------------------------------------------------------
fast_trimws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}
# ------------------------------------------------------------------------------
substr_until <- function(x, until, fixed = TRUE) {
  m <- regexpr(until, x, fixed=fixed)
  data.table::fifelse(m == -1L, x, substr(x, 1L, m - 1L))
}
# ------------------------------------------------------------------------------
subset_while_in <- function(x, y) {
  # Left subset of x that is in y
  if (!length(x)) return(NULL)
  i <- match(FALSE, x %in% y)
  if (is.na(i)) return(x)
  if (i==1L) return(NULL)
  return(x[1:(i-1)])
}
# ------------------------------------------------------------------------------
flips <- c(
  ">"  = "<",
  "<"  = ">",
  ">=" = "<=",
  "<=" = ">=",
  "==" = "=="
)
flip_on <- function(x) {
  # Flip join predicates
  # e.g. c("id1==id2", "date1<date2") -> c("id2==id1", "date2>date1")
  pos <- regexpr("(==|<=|>=|<|>)", x)
  ifelse(pos == -1,
         x,
         paste(
           substring(x, pos + attr(pos, "match.length")),
           flips[substring(x, pos, pos + attr(pos, "match.length") - 1)],
           substring(x, 1, pos-1))
         )
}
on_vec_to_df <- function(x) {
  # Predicates from character vector to 3-column data frame
  lhs <- op <- rhs <- rep(NA_character_, length(x))
  m <- regexpr("==|>=|<=|>|<", x)
  no_op <- m == -1
  if (any(no_op)) {
    x_no_op <- fast_trimws(x[no_op])
    lhs[no_op]  <- x_no_op
    op[no_op]   <- "=="
    rhs[no_op]  <- x_no_op
  }
  if (any(!no_op)) {
    x_op <- x[!no_op]
    mpos <- m[!no_op]
    mlen <- attr(m, "match.length")[!no_op]
    lhs[!no_op] <- fast_trimws(substr(x_op, 1L, mpos - 1L))
    op[!no_op]  <- fast_trimws(substr(x_op, mpos, mpos + mlen - 1L))
    rhs[!no_op] <- fast_trimws(substr(x_op, mpos + mlen, nchar(x_op)))
  }
  data.table::setDF(list(joincol.DT=lhs,op=op,joincol.i=rhs))
}
on_df_to_vec <- function(df, flip = FALSE) {
  # Predicates from data frame to character vector
  # with standardised whitespace and optionally flipped
  if (!flip) {
    ifelse(df$op == "==" & df$joincol.DT == df$joincol.i,
           df$joincol.DT,
           paste(df$joincol.DT,df$op,df$joincol.i))
  } else {
    ifelse(df$op == "==" & df$joincol.DT == df$joincol.i,
           df$joincol.DT,
           paste(df$joincol.i,flips[df$op],df$joincol.DT))
  }
}
# ------------------------------------------------------------------------------
vcat <- function(x) {
  cat(deparse(substitute(x)),": ",paste(x,collapse=", "),"\n", sep="")
}
vprint <- function(x) {
  cat(deparse(substitute(x)),"\n")
  print(x)
}
