library(CASdatasets)
library(OpenML)
library(farff)
library(tidyverse)
library(tidymodels)
library(parsnip)
library(caret)
library(keras)

# Import data
# data <- OpenML::getOMLDataSet(data.id = 41214)

# Get dataframe
cars <- data$data %>% 
  dplyr::select(-IDpol)

# Split data
cars_split <- initial_split(cars, prop = 0.8)
train <- training(cars_split)
test <- testing(cars_split)

# Train basic glm

glm1 <- glm(ClaimNb ~ ., data = train, family = poisson(link=log))

# 10% of data ~23 mins

backward_elimination <- stats::step(glm1, direction = "backward", trace = 1)

# Get summary
summary(glm1)

predictions <- predict(glm1, newdata = test, type = "response") %>% 
  as_tibble() %>% 
  mutate(index = seq(nrow(test)))


val_data <- test %>% 
  mutate(index = seq(nrow(test))) %>% 
  left_join(predictions, join_by(index))

yardstick::metrics(data = val_data, truth = ClaimNb, estimate = value)

print(backward_elimination)

save.image("glm_testing20.Rdata")
