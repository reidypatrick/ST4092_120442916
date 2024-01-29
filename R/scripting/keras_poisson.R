library(OpenML)
library(tidyverse)
library(tidymodels)
library(keras)

source("~/ST4092_120442916/R/functions/source_functions.R")
source_functions()

# Collect Data -----------------------------------------------------------------
log_info("Download OpenML dataset")
data <- OpenML::getOMLDataSet(data.id = 41214)

# Preprocess the data ---------------------------------------------------------
log_info("Preprocess data")
df.n <- data$data %>% 
  dplyr::select(where(is.numeric)) %>% 
  mutate(across(c(where(is.numeric), -IDpol, -ClaimNb), ~ scale_col(.)))

df <- data$data %>% 
  mutate(ClaimNb = as.integer(ClaimNb)) %>% 
  dplyr::select(c(IDpol, !where(is.numeric))) %>% 
  mutate(VehGas = as.integer(VehGas == "Diesel")) %>% 
  left_join(df.n, join_by(IDpol)) %>% 
  dplyr::select(-IDpol)

## Create Recipe ---------------------------------------------------------------
recipe <- recipe(ClaimNb ~ ., data = df) %>%
  step_dummy(all_factor())

## Bake Data -------------------------------------------------------------------
preprocessed_data <- prep(recipe, training = df, retain = TRUE)
baked_data <- bake(preprocessed_data, new_data = NULL)

## Split Data ------------------------------------------------------------------
log_info("Test/train split")
indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- baked_data[indices, ] 
test_data <- baked_data[-indices, ] 

x_train <- train_data %>% select(-ClaimNb)
x_test <- test_data %>% select(-ClaimNb) %>% as.matrix()

y_train <- train_data$ClaimNb
y_test <- test_data$ClaimNb


# Fit Model --------------------------------------------------------------------
# Clear unused memory ----------------------------------------------------------
log_info("Optimising memory usage")
gc()

## Initialise control variables ------------------------------------------------
t_batchsize <- c(512,1024)
t_epochs <- c(30,50)
t_act_final <- c("softplus", "exponential")
t_lr <- c(0.01,0.001)

t_hidden_nodes <- c(16,32)
t_hidden_act <- c("relu", "tanh")

tune_grid = expand.grid(
  batchsize = t_batchsize, 
  epochs = t_epochs, 
  final_act  = t_act_final, 
  learn_rate = t_lr,
  t_hidden_act1 = t_hidden_act, t_hidden_nodes1 = t_hidden_nodes,
  t_hidden_act2 = t_hidden_act, t_hidden_nodes2 = t_hidden_nodes)

# Fit model
log_info("Start Model Training")
poisson_fit <- fit_keras_poisson(x_train, y_train, lr = 0.001,
                                 batchsize = 256, nodes = c(32,32),
                                 n_epochs = 50)
log_info("Done")

model_list <- list()

for (i in seq_len(nrow(tune_grid))) {
poisson_fit <- fit_keras_poisson(
  x_train,
  y_train,
  nodes = c(tune_grid$t_hidden_nodes1[i], tune_grid$t_hidden_nodes2[i]),
  batchsize = tune_grid$batchsize[i],
  n_epochs = tune_grid$epochs[i],
  act_funs = c(tune_grid$t_hidden_act1[i], 
               tune_grid$t_hidden_act2[i], 
               tune_grid$final_act[i]),
  lr = tune_grid$learn_rate[i])

  model_list[[i]] <- poisson_fit
}

Sys.time()

# Tune:
#   Batchsize
#   Epochs
#   Nodes per layer
#   Activation functions per layer
#   Exponential vs. Softplus
#   Number of hidden layers
