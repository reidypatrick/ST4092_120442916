library(keras)
library(tidyverse)
library(CASdatasets)
library(OpenML)
library(farff)
library(tidymodels)
library(stringi)
library(sf)
library(raster)
library(ggplot2)
library(broom)
library(gridExtra)
library(knitr)
library(kableExtra)
library(PerformanceAnalytics)
#install_keras()

# Load Dataset -----------------------------------------------------------
data <- OpenML::getOMLDataSet(data.id = 41214)

cars_orig <- data$data

cars_numeric <- cars_orig %>% 
  mutate(across(where(is.factor), ~ as.numeric(unclass(.)))) %>% 
  mutate(VehGas = as.numeric(VehGas == 'Diesel'))

## Split data ------------------------------------------------------------------

cars_split <- initial_split(cars_numeric, prop = 0.8)
train <- training(cars_split) 
test <- testing(cars_split)

x_train <- train %>% dplyr::select(-ClaimNb) 
y_train <- train %>% dplyr::select(ClaimNb)

x_test <- test %>% dplyr::select(-ClaimNb)
y_test <- test %>% dplyr::select(ClaimNb)

## Reshape data ----------------------------------------------------------------
# x_train <- array_reshape(x_train, c(nrow(x_train), 784))
# x_test <- array_reshape(x_test, c(nrow(x_test), 784))

x_train <- array_reshape(x_train, c(nrow(x_train)))
x_test <- array_reshape(x_test, c(nrow(x_test)))

# ## Rescale data ----------------------------------------------------------------
# x_train <- x_train/255
# x_test <- x_test/255
# 
# y_train <- to_categorical(y_train, 10)
# y_test <- to_categorical(y_test, 10)

# Defining the Model -----------------------------------------------------------
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(12)) %>% 
  layer_dropout(rate = 0.04) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dropout(rate = 0.03) %>% 
  layer_dense(units = 1, activation = "exponential") 

summary(model)  

## Compile the model -----------------------------------------------------------
model %>% compile(
  optimizer = 'adam',
  loss = 'poisson',
  metrics = c('mean_squared_error')
)

# Training and Evaluation ------------------------------------------------------
history <- model %>% fit(
  x_train, y_train,
  epochs = 30, 
  batch_size = 120,
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test)

model %>% predict(x_test) 
