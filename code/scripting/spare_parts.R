# Spare Parts from keras testing ----

# Make predictions on the test set for both models
predictions_dropout <- predict(best_fit, new_data = test_data)
predictions_weight_decay <- predict(fit_weight_decay, new_data = test_data)

# Evaluate the models
evaluate_result_dropout <- yardstick::metrics(truth = test_data$ClaimNb, estimate = predictions_dropout$.pred)
evaluate_result_weight_decay <- yardstick::metrics(truth = test_data$ClaimNb, estimate = predictions_weight_decay$.pred)

print("Dropout Model:")
print(evaluate_result_dropout)

print("Weight Decay Model:")
print(evaluate_result_weight_decay)

## ChatGpt Attempt 2 -----------------------------------------------------------

# Assuming 'df' is your data frame
# Encode categorical variables
df$Region <- as.integer(factor(df$Region))
df$Area <- as.integer(factor(df$Area))
df$VehBrand <- as.integer(factor(df$VehBrand))
df$VehGas <- as.integer(factor(df$VehGas))

# Define features and target variable
X <- df[c('Region', 'Area', 'VehBrand')]
y <- df$ClaimNb

# Create a recipe for preprocessing
recipe <- recipe(ClaimNb ~ ., data = df) %>%
  step_scale(all_predictors()) %>%
  step_center(all_predictors())

# Split the data into training and testing sets
set.seed(42)
indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[indices, ]
test_data <- df[-indices, ]

# Preprocess the data
preprocessed_data <- prep(recipe, training = train_data, retain = TRUE)
baked_data <- bake(preprocessed_data, new_data = NULL)

# Create a neural network model using keras engine
nn_model <- mlp(
  hidden_units = c(64, 32),  # Adjust as needed
  epochs = 50,               # Adjust as needed
  activation = 'relu'
) %>%
  set_engine('keras') %>%
  set_mode('regression')

# Define a tidymodels workflow
workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(nn_model)

# Train the model
fit <- workflow %>%
  fit(ClaimNb~., data = baked_data)

# Make predictions on the test set
predictions <- predict(fit, new_data = bake(preprocessed_data, new_data = test_data))

# Evaluate the model
evaluate_result <- yardstick::metrics(truth = test_data$ClaimNb, estimate = predictions$.pred)

print(evaluate_result)