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

## Implement tuning grid -------------------------------------------------------
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

# Fit model --------------------------------------------------------------------
## Test model with tune grid ---------------------------------------------------
model_list <- list()

for (i in (length(model_list)+1):nrow(tune_grid)) {
  log_info("Start Loop")
  gc()
  log_info(paste("Model Number:", i))
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
  saveRDS(model_list, "model_list.rds")
  log_info("Finish Loop")  
}

## Extract best fit and predictions etc. ---------------------------------------
metrics.df <- data.frame(
  matrix(rep(numeric(256), 5), ncol = 5,
         dimnames = list(seq_len(256), c("index", "loss", "mse", "val_loss", "val_mse")))) 


for (i in seq_len(nrow(metrics.df))) {
  metrics.df[i,] <- c(i, unlist(lapply(model_list[[i]]$history$metrics, tail, 1)))
}

# TODO -------------------------------------------------------------------------
# Tune:
#   DONE: Batchsize 
#   DONE: Epochs
#   DONE: Nodes per layer
#   DONE: Activation functions per layer
#   DONE: Exponential vs. Softplus
#   TODO: Number of hidden layers


