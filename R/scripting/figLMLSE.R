# {r figLMLSE, include=TRUE, echo=FALSE, fig.cap="Linear Model: LSE", paged.print = TRUE, out.height="50%"}
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
plot(
  x,
  y_true,
  col = "blue",
  pch = 20,
  xlab = "X",
  ylab = "Y"
)
abline(model, col = "red")

# Add lines indicating normally distributed errors
segments(x, y_true, x, y_pred, col = "green")

# Add legend
legend("topleft",
  legend = c(
    expression(y[i]), expression(y[i] - hat(y)[i]),
    expression(hat(Y) == beta[0] + beta[1] * X)
  ),
  col = c("blue", "green", "red"),
  pch = c(20, NA, NA),
  lty = c(NA, 1, 1),
  cex = 0.8
)
