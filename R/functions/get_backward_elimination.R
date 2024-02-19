get_backward_elimination <- function(
    source_or_run = getOption("source_or_run"), 
    formula = NULL) {
  if (!exists("backward_elimination")) {
    if (source_or_run == "object") {
      log_info("Sourcing backward_elimination.rds")
      backward_elimination <- readRDS("data/objects/backward_elimination.rds")
    }
    if (source_or_run == "run") {
      log_info("Running Backward Elimination")
      backward_elimination <- stats::step(formula,
        direction = "backward", trace = 0
      )
    }
    log_info("Done")
  } else {
    log_info("Found backward_elimination in R Environment")
  }
  backward_elimination
}
