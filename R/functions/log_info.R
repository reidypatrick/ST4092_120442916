log_info <- function(message = "", verbose = get_global_variables(source)) {
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
