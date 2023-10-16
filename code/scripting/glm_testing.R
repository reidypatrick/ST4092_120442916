library(CASdatasets)
library(OpenML)
library(farff)
library(tidyverse)
library(tidymodels)
library(ggplot2)


data <- OpenML::getOMLDataSet(data.id = 41214)

data$desc$description

cars <- data$data

cars_split <- initial_split(cars, prop = 0.8)
train <- training(cars_split)
test <- testing(cars_split)


glm1 <- glm(ClaimNb ~ ., data = train, family = poisson(link=log))

summary(glm1)
anova(glm1)

write_csv(cars, "data/output/cars.csv")

predictions = predict(glm1, test, type = "response")


reg_count <- ggplot(cars, aes(x = Region)) +
  geom_bar() +
  labs(title = "Count of Cars by Region", x = "Region", y = "Count")

area_count <- ggplot(cars, aes(x = Area)) +
  geom_bar() +
  labs(title = "Count of Cars by Region", x = "Area", y = "Count")

brand_count <- ggplot(cars, aes(x = VehBrand)) +
  geom_bar() +
  labs(title = "Count of Cars by Region", x = "VehBrand", y = "Count")

gas_count <- ggplot(cars, aes(x = Region)) +
  geom_bar() +
  labs(title = "Count of Cars by Region", x = "VehGas", y = "Count")

grid_plot <- reg_count + area_count + brand_count + gas_count + 
  plot_layout(nrow = 2, byrow = TRUE)

