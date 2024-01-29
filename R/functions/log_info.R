log_info <- function(message) {
  cat(
    paste("[INFO][", substr(Sys.time(), 12, 19), "]: ", message, "\n", sep = "")
    )
}
