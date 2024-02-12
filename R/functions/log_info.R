log_info <- function(message) {
  if (global_variables$verbose == 1) {
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
