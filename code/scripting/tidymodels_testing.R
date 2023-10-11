library(CASdatasets)
library(OpenML)
library(farff)
library(tidyverse)
library(tidymodels)


data <- OpenML::getOMLDataSet(data.id = 41214)

data$desc$description
cars <- data$desc$description

cars_split <- initial_split(cars, prop = 0.8)
train <- training(cars_split)
test <- testing(cars_split)

model_recipe <- train %>% 
  recipe(ClaimNb ~.) %>% 
  update_role(IDpol, new_role = "id_variable") %>% 
  step_dummy(all_factor())


bake(model_recipe %>% prep(), train)
