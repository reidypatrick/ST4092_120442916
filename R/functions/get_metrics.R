get_metrics <- function(results_object, .truth, .estimate) {
  yardstick::metrics(data = results_object$val_data,
                     truth = .truth,
                     estimate = .estimate)
}
