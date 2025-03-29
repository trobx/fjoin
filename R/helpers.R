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
check_setup <- function(do, .DT, .i) {
  if (do) {
    if (!isNamespaceLoaded("data.table"))
      stop("Argument 'do' is TRUE but data.table is not loaded")
    if (!data.table::is.data.table(.DT))
      stop("Argument 'do' is TRUE but '.DT' is not a data.table")
    if (!data.table::is.data.table(.i))
      stop("Argument 'do' is TRUE but '.i' is not a data.table")
  } else {
    if (!(is.null(.DT) && is.null(.i))) {
      if (!is.data.frame(.DT))
        stop("'.DT' must have class \"data.frame\"")
      if (!is.data.frame(.i))
        stop("'.i' must have class \"data.frame\"")
    }
  }
}

# ------------------------------------------------------------------------------
clean_up <- function(.DT, .i) {
  # innocuous and convenient compared to trying to check for named objects and
  # track which dummy columns added
  suppressWarnings(data.table::set(.DT, j = "fjoin.ind", value = NULL))
  suppressWarnings(data.table::set(.i, j = "fjoin.ind", value = NULL))
  suppressWarnings(data.table::set(.DT, j = "fjoin.DT.rn", value = NULL))
  suppressWarnings(data.table::set(.i, j = "fjoin.i.rn", value = NULL))
}

# ------------------------------------------------------------------------------
# much faster than trimws
fast_trimws <- function(x) gsub("^\\s+|\\s+$", "", x)

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
make_label_fjoin <- function(t, sub_t) {
  # for calling in fjoin_*(): table label for printing, e.g. "x = A", "x (unnamed)"
  paste0(deparse(substitute(t)), if (!is.null(t) & is.name(sub_t)) sprintf(" = %s", deparse(sub_t)) else " (unnamed)")
}

make_label_dtjoin <- function(t, sub_t) {
  # for calling in dtjoin*(): table label for printing, e.g. "A", "(unnamed)"
  if (!is.null(t) & is.name(sub_t)) deparse(sub_t) else "(unnamed)"
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
  # Heuristic for cost of na.omit()
  # Based on regression analysis of execution time in secs with a particular setup and machine
  (10L + nc) * (nr / 1e9L)
}

# # ------------------------------------------------------------------------------
# dots_to_list <- function(valid_names,...) {
#   # No longer used. Check for valid args in ... and return them as a list
#   # note that list() evaluates the args (they are not substituted)
#   args <- list(...)
#   if (length(args)) {
#     invalid_names <- setdiff(names(args), valid_names)
#     if (length(invalid_names))
#       stop(sprintf("Unused additional argument(s): %s. Valid additional arguments are: %s",
#                    paste(invalid_names, collapse = ", "), paste(valid_names, collapse = ", ")))
#   }
#   return(args)
# }
#
# # ------------------------------------------------------------------------------
# vcat <- function(v) cat(sprintf("%s :", deparse(substitute(v))), v, "\n")
#
