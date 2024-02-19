source_functions <- function(py = FALSE) {
  # Source R functions
  r_functions_path <- file.path(getwd(), "R/functions")
  r_functions <- list.files(r_functions_path)

  for (f in seq_along(r_functions)) {
    source(file.path(r_functions_path, r_functions[f]), verbose = FALSE)
  }

  if (py) {
    # Source python functions
    py_functions_path <- file.path("python/functions")
    py_functions <- list.files(py_functions_path)

    for (f in seq_along(py_functions)) {
      reticulate::source_python(file.path(py_functions_path, py_functions[f]))
    }
  }
}
