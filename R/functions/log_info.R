log_info <- function(message, verbose = get_global_variables(source)) {
  if (verbose == 1) {
    cat(
      paste("[INFO][",
        substr(Sys.time(), 12, 19),
        "]: ",
        message, "\n",
        sep = ""
      )
    )
  }
}
