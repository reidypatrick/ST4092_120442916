# truncate exposure to 1
# truncate claims to 5
# band driv age
# band veh age
# drop density and change area to ordered categorical

source("R/scripting/config.R")

head(cars_orig)

data_glm_prep <- cars_orig %>%
  mutate(Exposure = ifelse(Exposure > 1, 1, Exposure)) %>%
  mutate(ClaimNb = ifelse(ClaimNb > 5, 5, ClaimNb)) %>%
  mutate(Area = factor(Area))



par(mfrow = c(1, 2))
create_map(data_glm_prep, Area, shape)
create_map(data_glm_prep, Density, shape)
cor(data_glm_prep$Area, data_glm_prep$Density)


data_glm_prep %>%
  ggplot(data, aes(x = Region, fill = Region)) +
  geom_bar(position = "dodge") +
  labs(x = "Region", y = "Count", fill = "Area") +
  ggtitle("Count of Area for each Region")

hist(cars_orig$DrivAge)

# AIC, BIC, R-Squared, MSE
