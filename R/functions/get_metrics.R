get_metrics <- function(results_object) {
  yardstick::metrics(data = results_object$val_data, truth = ClaimNb, estimate = .pred)
}
