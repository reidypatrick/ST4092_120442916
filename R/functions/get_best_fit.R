get_best_fit <- function(model_list) {
  metrics.df <- data.frame(
    matrix(rep(numeric(length(model_list)), 5), ncol = 5,
           dimnames = list(seq_len(256), 
                           c("index", "loss", "mse", "val_loss", "val_mse")))) 
  
  
  for (i in seq_len(nrow(metrics.df))) {
    metrics.df[i,] <- c(i, unlist(lapply(model_list[[i]]$history$metrics, tail, 1)))
  }
  
  tune_grid[metrics.df[which.min(metrics.df$mse),]$index,]
}