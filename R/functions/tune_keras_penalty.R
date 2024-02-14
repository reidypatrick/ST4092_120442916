tune_keras_penalty <- function(recipe, n_neurons, .penalty, n_epochs) {
  #' trains and tunes keras neural networks.
  #'
  #' @description
  #' takes recipe and tunes on hidden layers, weight decay rate
  #' and number of epochs. Returns list of fits and metrics.
  #' use tune_keras_dropout to tune dropout rate.


  ### Define Tuning Grid -------------------------------------------------------
  grid_penalty <- expand.grid(
    hidden_units = n_neurons,
    penalty = .penalty,
    epochs = n_epochs,
    activation = "relu"
  )

  ### Define Model -------------------------------------------------------------
  nn_model_penalty <- mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune(),
    activation = tune()
  ) %>%
    set_engine("keras") %>%
    set_mode("regression")

  ### Define Workflow ----------------------------------------------------------
  workflow_penalty <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(nn_model_penalty)

  ### Fit Model ----------------------------------------------------------------
  fit_penalty <- workflow_penalty %>%
    tune_grid(
      resamples = bootstraps(train_data, times = n_boots),
      grid = grid_penalty,
      control = control_grid(
        save_pred = TRUE,
        save_workflow = TRUE
      )
    )
}
