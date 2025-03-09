# ------------------------------------------------------------------------------
# Applies across the whole package
# https://stackoverflow.com/questions/69544896/how-can-i-use-data-table-in-a-package-without-importing-all-functions
.datatable.aware <- TRUE

# ------------------------------------------------------------------------------
# For note/error: "no visible binding for global variable '.SD'". R CMD check gives note on Windows but error On Linux/macOS, where enforces stricter checks, treating undeclared variable bindings as errors.

utils::globalVariables(c(".SD"))
