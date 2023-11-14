# Import data
# data <- OpenML::getOMLDataSet(data.id = 41214)

# Get dataframe
cars <- data$data

# Split data
cars_split <- initial_split(cars, prop = 0.8)
train <- training(cars_split)
test <- testing(cars_split)

# Train basic glm
glm1 <- glm(ClaimNb ~ ., data = train, family = poisson(link=log))

# Get summary
summary(glm1)


