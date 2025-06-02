# ------------------------------------------------------------------------------
check_TF <- function(x) {
  if (!x %in% c(TRUE, FALSE))
    stop(sprintf("Argument '%s' must be TRUE or FALSE", deparse(substitute(x))))
}
# ------------------------------------------------------------------------------
check_arg_order <- function(x) {
  if (!x %in% c("left", "right"))
    stop(sprintf("Argument '%s' must be \"left\" or \"right\"", deparse(substitute(x))))
}
# ------------------------------------------------------------------------------
check_mult <- function(x) {
  if (!x %in% c("all", "first", "last"))
    stop(sprintf("Argument '%s' must be \"all\", \"first\", or \"last\"", deparse(substitute(x))))
}
# ------------------------------------------------------------------------------
check_nomatch <- function(x) {
  if (! (is.null(x) || x %in% c(NA, 0L)))
    stop(sprintf("Argument '%s' must be NA, NULL, or 0L", deparse(substitute(x))))
}
# ------------------------------------------------------------------------------
check_input_class <- function(x) {
  # Check x is a plain list or data.frame (data.table etc.)
  if (!(is.list(x) && (is.data.frame(x) || !is.object(x))))
    stop(sprintf("Argument '%s' must be a data.frame-like object or list", deparse(substitute(x))))
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
shallow_DT <- function(x, use_setDT = TRUE) {
  # Shallow-copy columns of a data.frame-like object (or list of vectors) into a new DT
  # use_setDT = FALSE (no overallocation) is for pure read-only with no assignments
  # if a non-object list, can't use unclass (as returns the original object), and always use setDT to trigger common length check
  if (identical(class(x), "list")) {
    data.table::setDT(lapply(x, \(v) v))
  } else {
    if (use_setDT) data.table::setDT(unclass(x)) else data.table::setattr(unclass(x), "class", c("data.table", "data.frame"))
  }
}
# ------------------------------------------------------------------------------
make_mock_tables <- function(on) {
  # Create mock data.tables from an 'on' text expression
  tmp <- lapply(on, \(x) strsplit_predicate(x))
  names_DT <- c(vapply(tmp, function(x) x[1], character(1)), "col_DT", "col_c")
  names_i  <- c(vapply(tmp, function(x) x[3], character(1)), "col_i", "col_c")
  .DT <- stats::setNames(data.table::as.data.table(matrix(NA_integer_, nrow=1L, ncol=length(names_DT))), names_DT)
  .i  <- stats::setNames(data.table::as.data.table(matrix(NA_integer_, nrow=1L, ncol=length(names_i))), names_i)
  list(.DT, .i)
}
# ------------------------------------------------------------------------------
clean_up <- function(x, pattern = "^fjoin") {
  # Drop any columns with name starting with "fjoin"
  # suppressWarnings() is innocuous and convenient
  suppressWarnings(data.table::set(x, j = which(grepl(pattern, names(x))), value = NULL))
}
# ------------------------------------------------------------------------------
recompute_sfc_bboxes <- function(x) {
  # update the bbox attributes of all sfc-class columns
  for (i in seq_along(x)) if (inherits(x[[i]], "sfc")) x[[i]] <- sf::st_sfc(x[[i]], recompute_bbox=TRUE)
  x
}
# ------------------------------------------------------------------------------
na_omit_cost <- function(dt) {
  na_omit_cost_rc(nrow(dt), ncol(dt))
}
na_omit_cost_rc <- function(nr, nc) {
  # Heuristic for cost of na.omit.data.table()
  # Based on regression analysis of execution time in secs with a particular setup and machine
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
# na_omit_text(".DT")
# na_omit_text(".DT", na_cols="id_A")
# na_omit_text(".DT", sd_cols=c("id_A", "A"))
# na_omit_text(".DT", na_cols="id_A", sd_cols="id_A")
# na_omit_text(".DT", na_cols="id_A", sd_cols=c("id_A", "A"))
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
fast_trimws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}
# ------------------------------------------------------------------------------
clean_on <- function(x) {
  # Standardise the spacing of a vector of 'on' expressions e.g. c("id1==id2", " date1<  date2") -> c("id2 == id1", "date2 > date1")
  pos <- regexpr("(==|<=|>=|<|>)", x)
  ifelse(pos == -1,
         fast_trimws(x),
         paste(
           fast_trimws(substr(x, 1L, pos - 1L)),
           fast_trimws(substr(x, pos, pos + attr(pos, "match.length") - 1L)),
           fast_trimws(substr(x, pos + attr(pos, "match.length"), nchar(x)))
         )
  )
}
#clean_on(c("a==b"," c   >=`hello world ` ","  e "))
# ------------------------------------------------------------------------------
flip_on <- function(x) {
  # Flip a vector of 'on' expressions e.g. c("id1==id2", "date1<date2") -> c("id2==id1", "date2>date1")
  flips <- c(
    ">"  = "<",
    "<"  = ">",
    ">=" = "<=",
    "<=" = ">=",
    "==" = "=="
  )
  pos <- regexpr("(==|<=|>=|<|>)", x)
  ifelse(pos == -1,
         fast_trimws(x),
         paste(
           fast_trimws(substring(x, pos + attr(pos, "match.length"))),
           flips[substring(x, pos, pos + attr(pos, "match.length") - 1)],
           fast_trimws(substring(x, 1, pos-1))
         )
  )
}
#flip_on(c("a==b"," c   >=`hello world ` ","  e "))
# ------------------------------------------------------------------------------
strsplit_predicate <- function(x) {
  # Check and split a join predicate phrase e.g. "id1 > id2" -> c("id1", ">", "id2")
  # not vectorised
  # not as fast as previous version but more general and doesn't rely on no spaces
  pos <- regexpr("==|>=|<=|>|<", x)
  if (pos == -1) {
    x <- fast_trimws(x)
    c(x,"==",x)
  } else {
    c(fast_trimws(substr(x, 1L, pos - 1L)),
      fast_trimws(substr(x, pos, pos + attr(pos, "match.length") - 1L)),
      fast_trimws(substr(x, pos + attr(pos, "match.length"), nchar(x))))
  }
}
#strsplit_predicate("id1>=id2")
#strsplit_predicate("id1 >= id2")
#strsplit_predicate("    id1 >= id2  ")
#strsplit_predicate("id")
#strsplit_predicate("id1 >= ")
# ------------------------------------------------------------------------------
allows_equi <- function(x) {
  # Whether operator is equality/weak inequality
  x %in% c("==", ">=", "<=")
}
#allows_equi(">=")
#allows_equi(">")
# ------------------------------------------------------------------------------
vcat <- function(x) {
  cat(deparse(substitute(x))," : ",x,"\n")
}
