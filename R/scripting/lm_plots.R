par(mfrow = c(1,3))

# Set seed for reproducibility
set.seed(123)

# Generate nearly linear data
x <- 1:25
y_true <- 2 * x + rnorm(25, mean = 0, sd = 5)  # True linear relationship with normally distributed error

# Fit linear model
model <- lm(y_true ~ x)

# Predictions from the model
y_pred <- predict(model)

# Plot the data and linear model
plot(x, y_true, col = "blue", pch = 20, main = "Linear Model: LSE", xlab = "X", ylab = "Y")
abline(model, col = "red")

# Add lines indicating normally distributed errors
segments(x, y_true, x, y_pred, col = "green")


# Set seed for reproducibility
set.seed(123)

# Generate nearly linear data
x <- 1:25
y_true <- 2 * x + rnorm(25, mean = 0, sd = 3)  # True linear relationship with normally distributed error

# Fit linear model
model <- lm(y_true ~ x)

# Predictions from the model
y_pred <- predict(model)

# Plot the data and linear model
plot(x, y_true, col = "blue", pch = 20, main = "Linear Model:MLE", xlab = "X", ylab = "Y")
abline(model, col = "red")

# Add bell curves around each true data point to represent normally distributed errors
for (i in seq(1,25,5)) {
  points <- rnorm(100, mean = y_pred[i], sd = 2)
  d <- density(points)
  lines(i + d$y, d$x, col = "green", lwd = 2)
}



# Generate nearly linear data
set.seed(09022024)
x <- 1:25
y_true <- x * rexp(25, rate = 1)  # True linear relationship with normally distributed error

# Fit linear model
model <- lm(y_true ~ x)

# Predictions from the model
y_pred <- predict(model)

# Plot the data and linear model
plot(x, y_true, col = "blue", pch = 20, main = "Generalised Linear Model", xlab = "X", ylab = "Y")
#abline(model, col = "red")

for (i in seq(4,20,4)) {
  points <- rexp(100, rate = 1)
  d <- density(points)
  lines(i + d$y, y_pred[i] + d$x, col = "green", lwd = 2)
}

# # Set seed for reproducibility
# set.seed(123)
# 
# # Number of random variables
# n <- 1000
# 
# # Generate exponential random variables
# exp_values <- rexp(n, rate = 1)  # rate = 1 (lambda = 1) for exponential distribution
# 
# # Plot histogram
# hist(exp_values, breaks = 30, freq = FALSE, col = "skyblue", main = "Exponential Random Variables", xlab = "Value", ylab = "Density")
# 
# # Overlay the density curve
# curve(dexp(x, rate = 1), col = "red", lwd = 2, add = TRUE, n = 1000, from = 0, to = max(exp_values))
