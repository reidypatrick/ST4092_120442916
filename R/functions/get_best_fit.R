get_best_fit <- function(model_list, tune_grid) {
  metrics_df <- data.frame(
    matrix(rep(numeric(length(model_list)), 5),
      ncol = 5,
      dimnames = list(
        seq_along(model_list),
        c("index", "loss", "mse", "val_loss", "val_mse")
      )
    )
  )


  for (i in seq_len(nrow(metrics_df))) {
    metrics_df[i, ] <- c(
      i,
      unlist(
        lapply(model_list[[i]]$history$metrics, tail, 1)
      )
    )
  }

  tune_grid[metrics_df[which.min(metrics_df$mse), ]$index, ]
}
