library(CASdatasets)
library(OpenML)
library(farff)
library(tidyverse)
library(tidymodels)
library(parsnip)
library(caret)
library(keras)

# Preparation ------------------------------------------------------------------
## Source Data -----------------------------------------------------------------

data <- OpenML::getOMLDataSet(data.id = 41214)

data$desc$description
cars_orig <- data$data

df <- cars_orig

## Initialise control variables ------------------------------------------------

n_boots <- 5
n_epochs <- 20



## Encode categorical variables ------------------------------------------------
df$Region <- as.integer(factor(df$Region))
df$Area <- as.integer(factor(df$Area))
df$VehBrand <- as.integer(factor(df$VehBrand))
df$VehGas <- as.integer(factor(df$VehGas))


# Model Building ---------------------------------------------------------------
### Create Recipe --------------------------------------------------------------
recipe <- recipe(ClaimNb ~ ., data = df) %>%
  step_scale(all_predictors()) %>%
  step_center(all_predictors())

### Split Data -----------------------------------------------------------------
set.seed(42)
indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[indices, ]
test_data <- df[-indices, ]

### Preprocess the data --------------------------------------------------------
preprocessed_data <- prep(recipe, training = train_data, retain = TRUE)

# Weight Decay (Penalty) -------------------------------------------------------
### Define Tuning Grid ---------------------------------------------------------

grid_penalty <- expand.grid(
  hidden_units = c(64, 32),
  penalty = c(0.001, 0.01, 0.05),
  epochs = c(n_epochs),
  activation = "relu"
)

### Define Model ---------------------------------------------------------------

nn_model_penalty <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune(),
  activation = tune()
) %>%
  set_engine("keras") %>%
  set_mode("regression")

### Define Workflow ------------------------------------------------------------

workflow_penalty <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(nn_model_penalty)

### Fit Model ------------------------------------------------------------------
fit_penalty <- workflow_penalty %>%
  tune_grid(
    resamples = bootstraps(train_data, times = n_boots),
    grid = grid_penalty,
    control = control_grid(
      save_pred = TRUE,
      save_workflow = TRUE
    )
  )

save.image(file = ".Rdata")
save.image(file = "dry_run.Rdata")

# Dropout ----------------------------------------------------------------------
### Define Tuning Grid ---------------------------------------------------------

grid_dropout <- expand.grid(
  hidden_units = c(32, 64),
  dropout = c(0.2, 0.3, 0.4),
  epochs = c(n_epochs),
  activation = "relu"
)

### Define Model ---------------------------------------------------------------
nn_model_dropout <- mlp(
  hidden_units = tune(),
  dropout = tune(),
  epochs = tune(),
  activation = tune()
) %>%
  set_engine("keras") %>%
  set_mode("regression")

### Define Workflow ------------------------------------------------------------
workflow_dropout <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(nn_model_dropout)

### Fit Model ------------------------------------------------------------------
fit_dropout <- workflow_dropout %>%
  tune_grid(
    resamples = bootstraps(train_data, times = n_boots),
    grid = grid_dropout,
    control = control_grid(
      save_pred = TRUE,
      save_workflow = TRUE
    )
  )



# Results ----------------------------------------------------------------------

## Gather best fits ------------------------------------------------------------
best_fit_dropout <- fit_best(fit_dropout)
best_fit_penalty <- fit_best(fit_penalty)

## Extract predictions ---------------------------------------------------------
predictions_dropout <- predict(best_fit_dropout, new_data = test_data) %>%
  mutate(index = seq(nrow(test_data)))

predictions_penalty <- predict(best_fit_penalty, new_data = test_data) %>%
  mutate(index = seq(nrow(test_data)))

val_data_dropout <- test_data %>%
  mutate(index = seq(nrow(test_data))) %>%
  right_join(predictions_dropout, join_by(index))

val_data_penalty <- test_data %>%
  mutate(index = seq(nrow(test_data))) %>%
  right_join(predictions_dropout, join_by(index))

## Calculate Metrics -----------------------------------------------------------
yardstick::metrics(data = val_data_dropout, truth = ClaimNb, estimate = .pred)
yardstick::metrics(data = val_data_penalty, truth = ClaimNb, estimate = .pred)


# boots = 5
# epochs = 20
# grid_size = 6


save.image(file = ".Rdata")
save.image(file = "dry_run.Rdata")
