fit_keras_poisson_layers <- function(
    x_train,
    y_train,
    n_layers = 1,
    nodes = c(16, 1),
    act_funs = c("relu", "exponential"),
    batchsize = 1024,
    n_epochs = 30,
    lr = 0.001,
    m = c("mean_squared_error"),
    verbose = 1) {
  # Function for fitting n layer keras FFNN, with options for tuning but
  # minimal error handling w.r.t alignment of layers, nodes and activation
  # functions

  # log system time
  start <- Sys.time()


  # Check nodes and n_layers match
  if (length(nodes) != (n_layers + 1)) {
    stop("Incompatible number of layers and number of nodes provided")
  }

  # Setup layer names
  names <- character(n_layers + 1)
  names[length(names)] <- "Output"
  for (i in seq_len(n_layers)) {
    names[i] <- paste("Hidden", i, sep = "")
  }

  # Save and print parameters
  params <- list(
    act_funs = act_funs,
    batchsize = batchsize,
    n_epochs = n_epochs,
    lr = lr,
    m = m
  )

  if (verbose == 1) {
    log_info("Start Model Training")
    print(params)
  }

  # 0 layers, nodes = 1, act = exp
  # 1 layers, nodes = c(16,1), act = relu,exp
  # 2 layers, nodes = c(16,16,1), act = relu, relu,exp

  # Construct model architecture
  model <- keras_model_sequential()
  model %>%
    layer_dense(
      units = nodes[1], activation = act_funs[1], name = names[1],
      input_shape = ncol(x_train)
    )

  for (layer in seq_len(n_layers)) {
    model %>%
      layer_dense(
        units = nodes[layer + 1],
        activation = act_funs[layer + 1],
        name = names[layer + 1]
      )
  }

  # Define model loss and optimiser
  model %>% compile(
    loss = "poisson",
    optimizer = optimizer_adam(learning_rate = lr),
    metrics = m
  )

  # Fit model
  history <- model %>%
    keras::fit(
      x = x_train %>% as.matrix(),
      y = y_train %>% as.matrix(),
      epochs = n_epochs,
      batch_size = batchsize,
      validation_split = 0.2,
      verbose = verbose
    )
  params <- c(params, time = Sys.time() - start)

  if (verbose == 1) {
    log_info("Done")
  }

  list(model = model, history = history, params = params)
}
