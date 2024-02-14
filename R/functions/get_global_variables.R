get_global_variables <- function(..., global_variables = global_variables) {
  global_variables[[substitute(...)]]
}
