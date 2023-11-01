library(ggplot2)
library(dplyr)
library(reshape2)

# Function to generate histograms for all numeric columns
generate_histograms <- function(data) {
  numeric_cols <- Filter(is.numeric, data)
  plots <- lapply(numeric_cols, function(col) {
    col_name <- colnames(data)[which(data == col)]
    ggplot(data, aes(x = !!col)) +
      geom_histogram() +
      labs(title = paste("Histogram of", col_name))
  })
  return(plots)
}

# Function to generate boxplots for all numeric columns
generate_boxplots <- function(data) {
  numeric_cols <- Filter(is.numeric, data)
  plots <- lapply(numeric_cols, function(col) {
    col_name <- colnames(data)[which(data == col)]
    ggplot(data, aes(y = !!col)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", col_name))
  })
  return(plots)
}

# Function to generate bar plots for all categorical columns
generate_barplots <- function(data) {
  categorical_cols <- Filter(is.factor, data)
  plots <- lapply(categorical_cols, function(col) {
    col_name <- colnames(data)[which(data == col)]
    ggplot(data, aes(x = !!col)) +
      geom_bar() +
      labs(title = paste("Barplot of", col_name))
  })
  return(plots)
}

# Function to generate scatter plots for all pairs of numeric columns
generate_scatterplots <- function(data) {
  numeric_cols <- Filter(is.numeric, data)
  combinations <- combn(numeric_cols, 2, simplify = FALSE)
  plots <- lapply(combinations, function(pair) {
    col1_name <- colnames(data)[which(data == pair[[1]])]
    col2_name <- colnames(data)[which(data == pair[[2]])]
    ggplot(data, aes(x = !!pair[[1]], y = !!pair[[2]])) +
      geom_point() +
      labs(title = paste("Scatter Plot of", col1_name, "vs.", col2_name))
  })
  return(plots)
}

# Function to print a list of plots
print_plots <- function(plots) {
  for (i in seq_along(plots)) {
    print(plots[[i]])
  }
}

# Example usage with the mtcars dataset

histograms <- generate_histograms(cars)
boxplots <- generate_boxplots(cars)
barplots <- generate_barplots(cars)
scatterplots <- generate_scatterplots(cars)

# Print all generated plots
print_plots(histograms)
print_plots(boxplots)
print_plots(barplots)
print_plots(scatterplots)

melt(cars)

cars1<-cars %>% 
  mutate(across(where(is.factor), ~ as.numeric(unclass(.)))) %>% 
  mutate(VehGas = as.numeric(VehGas == 'Diesel'))

cor(cars1 %>% select(-IDpol)) %>% heatmap(keep.dendro = TRUE)

ggplot(data = melt(cars), aes(x=ClaimNb, y=VehPower, fill=value)) + 
  geom_tile()
