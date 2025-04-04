# ------------------------------------------------------------------------------
check_TF <- function(x) {
  if (!x %in% c(TRUE, FALSE))
    stop(sprintf("Argument '%s' must be TRUE or FALSE", deparse(substitute(x))))
}
# ------------------------------------------------------------------------------
check_arg_order <- function(x) {
  if (!x %in% c("x", "y"))
    stop(sprintf("Argument '%s' must be \"x\" or \"y\"", deparse(substitute(x))))
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
  # Check x is a list or data.frame (data.table etc.)
  if (!(is.data.frame(x) || is.list(x)))
    stop(sprintf("Argument '%s' must be a data.frame-like object or list", deparse(substitute(x))))
}
# ------------------------------------------------------------------------------
shallow_DT <- function(x, use_setDT = TRUE) {
  # Shallow-copy columns of a data.frame-like object (or list of vectors) into a new DT
  if (use_setDT) {
    data.table::setDT(unclass(x))
  } else {
    # pure read-only (no assignments), no overallocation, no length checks for list input
    data.table::setattr(unclass(x), "class", c("data.table", "data.frame"))
  }
}
# ------------------------------------------------------------------------------
clean_up <- function(x, pattern = "^fjoin") {
  # Drop any columns with name starting with "fjoin"
  # suppressWarnings() is innocuous and convenient
  suppressWarnings(data.table::set(x, j = names(x)[grepl(pattern, names(x))], value = NULL))
}
# ------------------------------------------------------------------------------
make_mock_tables <- function(on) {
  # Create mock data.frames from an 'on' expression
  tmp <- lapply(on, \(x) strsplit_predicate(x))
  names_DT <- c(vapply(tmp, function(x) x[1], character(1)), "col_DT", "col_c")
  names_i  <- c(vapply(tmp, function(x) x[3], character(1)), "col_i", "col_c")
  .DT <- stats::setNames(as.data.frame(matrix(NA_integer_, nrow=1L, ncol=length(names_DT))), names_DT)
  .i  <- stats::setNames(as.data.frame(matrix(NA_integer_, nrow=1L, ncol=length(names_i))), names_i)
  list(.DT, .i)
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
  # Much faster than trimws()
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
