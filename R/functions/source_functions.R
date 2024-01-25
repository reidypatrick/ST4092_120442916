source_functions <- function(functions_path) {
  functions_path <- file.path(getwd(), "R/functions")
  functions <- list.files(functions_path)

  for (f in seq_along(functions)) {
    source(file.path(functions_path, functions[f]))
  }
}
