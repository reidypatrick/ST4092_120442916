library(CASdatasets)
library(OpenML)
library(farff)
library(tidyverse)
library(tidymodels)


data <- OpenML::getOMLDataSet(data.id = 41214)

data$desc$description

cars <- data$data

cars_split <- initial_split(cars, prop = 0.8)
train <- training(cars_split)
test <- testing(cars_split)

glm1 <- glm(ClaimNb ~ Exposure + Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, data = train, family = quasipoisson(link=log))
summary(glm1)
anova(glm1)

write_csv(cars, "data/output/cars.csv")

predictions = predict(glm1, test, type = "response")
