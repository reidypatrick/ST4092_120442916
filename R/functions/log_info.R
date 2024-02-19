log_info <- function(
    message = "",
    verbose = getOption("log_verbose")) {
  if (verbose == TRUE) {
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
