tune_keras_dropout <- function(recipe, n_neurons, .dropout, n_epochs) {
  #' trains and tunes keras neural networks.
  #'
  #' @description
  #' takes recipe and tunes on hidden layers, dropout rate
  #' and number of epochs. Returns list of fits and metrics.
  #' use tune_keras_penalty to tune weight decay rate.

  ### Define Tuning Grid -------------------------------------------------------
  grid_dropout <- expand.grid(
    hidden_units = n_neurons,
    dropout = .dropout,
    epochs = n_epochs,
    activation = "relu"
  )

  ### Define Model -------------------------------------------------------------
  nn_model_dropout <- mlp(
    hidden_units = tune(),
    dropout = tune(),
    epochs = tune(),
    activation = tune()
  ) %>%
    set_engine("keras") %>%
    set_mode("regression")

  nn_model_dropout$engine$set_params(loss = "poisson", optimizer = optimizer_adam())

  ### Define Workflow ----------------------------------------------------------
  workflow_dropout <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(nn_model_dropout)

  ### Fit Model ----------------------------------------------------------------
  fit_dropout <- workflow_dropout %>%
    tune_grid(
      resamples = bootstraps(train_data, times = n_boots),
      grid = grid_dropout,
      control = control_grid(
        save_pred = TRUE,
        save_workflow = TRUE
      )
    )
}
