set_global_variables <- function(verbose = 1,
                                 source = FALSE) {
  # By default, runs all code chunks and prints all outputs
  # Undesirable for report knitting,
  # Desirable for testing and validation

  global_variables <<- list(
    verbose = verbose,
    source = source
  )
}
