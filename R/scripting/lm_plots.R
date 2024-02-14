par(mfrow = c(1, 3))

# Set seed for reproducibility
set.seed(123)

# Generate nearly linear data
x <- 1:25
y_true <- 2 * x + rnorm(25, mean = 0, sd = 5)
# True linear relationship with normally distributed error

# Fit linear model
model <- lm(y_true ~ x)

# Predictions from the model
y_pred <- predict(model)

# Plot the data and linear model
plot(x, y_true,
  col = "blue",
  pch = 20, main = "Linear Model: LSE", xlab = "X", ylab = "Y"
)
abline(model, col = "red")

# Add lines indicating normally distributed errors
segments(x, y_true, x, y_pred, col = "green")


# Set seed for reproducibility
set.seed(123)

# Generate nearly linear data
x <- 1:25
y_true <- 2 * x + rnorm(25, mean = 0, sd = 3)
# True linear relationship with normally distributed error

# Fit linear model
model <- lm(y_true ~ x)

# Predictions from the model
y_pred <- predict(model)

# Plot the data and linear model
plot(x, y_true,
  col = "blue",
  pch = 20, main = "Linear Model:MLE", xlab = "X", ylab = "Y"
)
abline(model, col = "red")

# Add bell curves around each true data
# point to represent normally distributed errors
for (i in seq(1, 25, 5)) {
  points <- rnorm(100, mean = y_pred[i], sd = 2)
  d <- density(points)
  lines(i + d$y, d$x, col = "green", lwd = 2)
}



# Generate nearly linear data
set.seed(09022024)
x <- 1:25
y_true <- x * rexp(25, rate = 1)
# True linear relationship with normally distributed error

# Fit linear model
model <- lm(y_true ~ x)

# Predictions from the model
y_pred <- predict(model)

# Plot the data and linear model
plot(x, y_true,
  col = "blue", pch = 20,
  main = "Generalised Linear Model", xlab = "X", ylab = "Y"
)

for (i in seq(4, 20, 4)) {
  points <- rexp(100, rate = 1)
  d <- density(points)
  lines(i + d$y, y_pred[i] + d$x, col = "green", lwd = 2)
}

# Set seed for reproducibility
set.seed(123)

# Number of random variables
n <- 1000




set.seed(123)

# Generate nearly linear data
x <- 1:25
lambda <- exp(x) # True lambda values for Poisson distribution
y_true <- rpois(25, lambda)
# True linear relationship with Poisson-distributed error

# Fit linear model
model <- lm(y_true ~ x)

# Predictions from the model
y_pred <- predict(model)

# Plot the data and linear model
plot(x, y_true,
  col = "blue",
  pch = 20, main = "Linear Model: MLE", xlab = "X", ylab = "Y"
)
abline(model, col = "red")

# Add Poisson curves around each true data point
for (i in seq_along(x)) {
  points <- rpois(100, lambda[i])
  d <- density(points)
  lines(i + d$y, d$x, col = "green", lwd = 2)
}
