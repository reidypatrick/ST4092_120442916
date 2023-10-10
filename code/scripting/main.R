library(CASdatasets)
library(OpenML)
library(farff)
library(tidyverse)
library(tidymodels)


data <- OpenML::getOMLDataSet(data.id = 41214)

data$desc$description

cars <- data$dat

cars_split <- initial_split(cars, prop = 0.8)
train <- training(cars_split)
test <- testing(cars_split)

glm1 <- glm(ClaimNb ~ Exposure + Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, data = test)
summary(glm1)
anova(glm1)

predictions <- predict(glm1, test) %>% 
  data.frame() %>% 
  rowid_to_column() %>% 
  mutate("predictions" = .[]) %>% 
  select(rowid, predictions) %>% 
  right_join(test, join_by(rowid == IDpol))

rmse(predictions, ClaimNb, predictions)


View(predictions)
?rename
