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

# Collect Data -----------------------------------------------------------------
data <- OpenML::getOMLDataSet(data.id = 41214)

cars_orig <- data$data

df <- cars_orig %>%
  mutate(VehGas = as.factor(VehGas)) %>% 
  mutate(ClaimNb = as.integer(ClaimNb))

## Initialise control variables ------------------------------------------------
n_boots <- 5
n_epochs <- c(50)
n_neurons <- c(16, 32, 64)
t_dropout <- c(0.2, 0.3, 0.4)

# Model Building ---------------------------------------------------------------
## Create Recipe ---------------------------------------------------------------
recipe <- recipe(ClaimNb ~ ., data = df) %>%
  update_role(IDpol, new_role = "id variable") %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_factor())


## Preprocess the data ---------------------------------------------------------
preprocessed_data <- prep(recipe, training = df, retain = TRUE)
baked_data <- bake(preprocessed_data, new_data = NULL)

## Split Data ------------------------------------------------------------------
set.seed(42)
indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- baked_data[indices, ]
test_data <- baked_data[-indices, ]

# Fit Model --------------------------------------------------------------------
model1 <- keras_model_sequential()
model1 %>% 
  layer_dense(units = 44, activation = "relu", input_shape = c(44)) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1, activation = "k_exp") 



model8 = keras_model_sequential() 
model8 %>% 
  layer_dense(units = 44, 
              activation = 'tanh', 
              input_shape = c(ncol(train_data)-1),
              kernel_initializer=
                initializer_random_uniform(minval = -0.5, 
                                           maxval = 0.5, 
                                           seed = set.seed(120442916))
              ) %>% 
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = 'tanh') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 16, activation = 'tanh') %>%
  layer_dense(units = 4, activation = 'tanh') %>%
  layer_dense(units = 1)

model1 %>% compile(
  loss = "Poisson",
  optimizer = optimizer_rmsprop(),
  metrics = c("mean_squared_error", "mean_absolute_error", "poisson")
)

history <- model8 %>% fit(
  x = train_data %>% dplyr::select(-ClaimNb) %>% as.matrix(), 
  y = train_data %>% as.matrix(),
  epochs = 30, 
  batch_size = 512,
  validation_split = 0.2
)

summary(baked_data)

predictions <- model1 %>% predict(test_data %>% dplyr::select(-ClaimNb) %>% as.matrix())

head(predictions)
