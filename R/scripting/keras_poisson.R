library(CASdatasets)
library(OpenML)
library(farff)
library(tidyverse)
library(tidymodels)
library(parsnip)
library(caret)
library(keras)
library(scales)
library(healthyR.ai)
library(deepviz)
source("~/ST4092_120442916/code/functions/source_functions.R")
source_functions("code/functions")

# Collect Data -----------------------------------------------------------------
data <- OpenML::getOMLDataSet(data.id = 41214)

df.n <- data$data %>% 
  dplyr::select(where(is.numeric)) %>% 
  mutate(across(c(where(is.numeric), -IDpol, -ClaimNb), ~ scale_col(.)))

df <- data$data %>% 
  mutate(ClaimNb = as.integer(ClaimNb)) %>% 
  dplyr::select(c(IDpol, !where(is.numeric))) %>% 
  mutate(VehGas = as.integer(VehGas == "Diesel")) %>% 
  left_join(df.n, join_by(IDpol)) %>% 
  dplyr::select(-IDpol)

summary(df)

## Initialise control variables ------------------------------------------------
n_boots <- 5
n_epochs <- c(50)
n_neurons <- c(16, 32, 64)
t_dropout <- c(0.2, 0.3, 0.4)

# Model Building ---------------------------------------------------------------
## Create Recipe ---------------------------------------------------------------
recipe <- recipe(ClaimNb ~ ., data = df) %>%
  step_dummy(all_factor())


## Preprocess the data ---------------------------------------------------------
preprocessed_data <- prep(recipe, training = df, retain = TRUE)
baked_data <- bake(preprocessed_data, new_data = NULL)

## Split Data ------------------------------------------------------------------
indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- baked_data[indices, ] 
test_data <- baked_data[-indices, ] 

x_train <- train_data %>% select(-ClaimNb)
x_test <- test_data %>% select(-ClaimNb) %>% as.matrix()

y_train <- train_data$ClaimNb
y_test <- test_data$ClaimNb


# Fit Model --------------------------------------------------------------------
# Clear unused memory
gc()

# Construct model
model1 <- keras_model_sequential()
model1 %>% 
  layer_dense(units = 64, activation = "relu", input_shape = ncol(train_data)-1) %>% 
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "exponential") 

model1 %>% compile(
  loss = "poisson",
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = c("mean_squared_error")
)

history <- model1 %>%
  keras::fit(
    x = x_train %>% as.matrix(),
    y = y_train %>% as.matrix(),
    epochs = 30,
    batch_size = 256,
    validation_split = 0.2
  )

Predictions <- model1 %>% 
  predict(x_test)

head(Predictions)
summary(Predictions)

val_data_model1 <- test_data %>% 
  cbind(Predictions) %>% 
  relocate(ClaimNb, .before = Predictions)

head(val_data_model1)

plot(val_data_model1$ClaimNb, val_data_model1$Predictions)

par(mfrow = c(1,2))
hist(Predictions, freq = FALSE)
hist(data$data$ClaimNb, freq = FALSE)

plot_model(model1)
reticulate::source_python("code/scripting/py_plot_model.py")
plot_model(model1)
rm(plot_model)

plot_model(model1)

# Tune:
#   Number of hidden layers
#   Nodes per layer
#   Activation functions per layer
#   Exponential vs. Softplus
#   Batchsize


