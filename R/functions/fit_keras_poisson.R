fit_keras_poisson <- function(
    x_train,
    y_train,
    nodes = c(16, 16),
    batchsize = 512,
    n_epochs = 30,
    act_funs = c("relu", "relu", "exponential"),
    lr = 0.001,
    m = c("mean_squared_error"),
    verbose = getOption("log_verbose")) {
  # Function for fitting two layer keras FFNN, with options for tuning

  # log system time
  start <- Sys.time()
  # Save and print parameters
  params <- list(
    batchsize = batchsize,
    n_epochs = n_epochs,
    act_funs = act_funs,
    lr = lr,
    m = m
  )

  log_info("Start Model Training")

  if (verbose == TRUE) {
    print(params)
  }
  # Construct model architecture
  model <- keras_model_sequential()
  model %>%
    layer_dense(
      units = nodes[1], activation = act_funs[1], name = "Hidden1",
      input_shape = ncol(x_train)
    ) %>%
    layer_dense(
      units = nodes[2], activation = act_funs[2], name = "Hidden2"
    ) %>%
    layer_dense(
      units = 1, activation = act_funs[3], name = "Output"
    )

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

  more_params <- list(
    time = Sys.time() - start,
    shape = c(
      model$layers[[1]]$input_shape[[2]],
      model$layers[[2]]$input_shape[[2]],
      model$layers[[3]]$input_shape[[2]]
    )
  )

  params <- append(
    params,
    more_params
  )

  log_info("Done")

  list(model = model, history = history, params = params)
}
