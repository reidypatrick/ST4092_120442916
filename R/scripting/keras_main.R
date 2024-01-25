library(CASdatasets)
library(OpenML)
library(farff)
library(tidyverse)
library(tidymodels)
library(parsnip)
library(caret)
library(keras)
source("~/ST4092_120442916/code/functions/source_functions.R")
source_functions("code/functions")



# Preparation ------------------------------------------------------------------
## Source Data -----------------------------------------------------------------

data <- OpenML::getOMLDataSet(data.id = 41214)

data$desc$description
cars_orig <- data$data

df <- cars_orig

## Initialise control variables ------------------------------------------------

n_boots <- 5
n_epochs <- c(20)
n_neurons <- c(16)
t_dropout <- c(0.3)
t_penalty <- c(0.01)

## Encode categorical variables ------------------------------------------------
df$Region <- as.integer(factor(df$Region))
df$Area <- as.integer(factor(df$Area))
df$VehBrand <- as.integer(factor(df$VehBrand))
df$VehGas <- as.integer(factor(df$VehGas))


# Model Building ---------------------------------------------------------------
## Create Recipe --------------------------------------------------------------
recipe <- recipe(ClaimNb ~ ., data = df) %>%
  step_scale(all_predictors()) %>%
  step_center(all_predictors())

## Split Data -----------------------------------------------------------------
set.seed(42)
indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[indices, ]
test_data <- df[-indices, ]

## Preprocess the data --------------------------------------------------------
preprocessed_data <- prep(recipe, training = train_data, retain = TRUE)

## Fit Models ------------------------------------------------------------------
### Weight Decay (Penalty) -----------------------------------------------------
fit_penalty <- tune_keras_penalty(recipe, n_neurons, t_penalty, n_epochs)

### Dropout --------------------------------------------------------------------

fit_dropout <- tune_keras_dropout(recipe, n_neurons, t_dropout, n_epochs)

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

save.image(file = "dry_run.Rdata")
