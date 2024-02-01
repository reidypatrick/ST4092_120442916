get_backward_elimination <- function(how = "object") {
  if (!exists("backward_elimination")) {
    if (how == "object") {
      log_info("Sourcing backward_elimination.rds")
      backward_elimination <- readRDS("data/objects/backward_elimination.rds")
    }
    if (how == "run") {
      log_info("Running Backward Elimination")
      backward_elimination <- stats::step(glm1, direction = "backward", trace = 0)
    }
    log_info("Done")
  } else {
    log_info("Found backward_elimination in R Environment")
  }
  backward_elimination
}
