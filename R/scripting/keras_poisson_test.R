library(keras)
library(dplyr, warn.conflicts = FALSE)
library(magrittr)


# kaggle datasets download -d floser/french-motor-claims-datasets-fremtpl2freq
# unzip archive.zip

df <- cars_orig
df
#> # A tibble: 678,013 × 12
#>    IDpol ClaimNb Exposure Area  VehPower VehAge DrivAge BonusMalus VehBrand
#>    <dbl>   <dbl>    <dbl> <chr>    <dbl>  <dbl>   <dbl>      <dbl> <chr>   
#>  1     1       1     0.1  D            5      0      55         50 B12     
#>  2     3       1     0.77 D            5      0      55         50 B12     
#>  3     5       1     0.75 B            6      2      52         50 B12     
#>  4    10       1     0.09 B            7      0      46         50 B12     
#>  5    11       1     0.84 B            7      0      46         50 B12     
#>  6    13       1     0.52 E            6      2      38         50 B12     
#>  7    15       1     0.45 E            6      2      38         50 B12     
#>  8    17       1     0.27 C            7      0      33         68 B12     
#>  9    18       1     0.71 C            7      0      33         68 B12     
#> 10    21       1     0.15 B            7      0      41         50 B12     
#> # … with 678,003 more rows, and 3 more variables: VehGas <chr>, Density <dbl>,
#> #   Region <chr>

target <- df$ClaimNb

recipe <- recipe(ClaimNb ~ ., data = df) %>%
  update_role(IDpol, new_role = "id variable") %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_factor())


## Preprocess the data ---------------------------------------------------------
preprocessed_data <- prep(recipe, training = df, retain = TRUE)
features <- bake(preprocessed_data, new_data = NULL) %>% 
  select(-ClaimNb) %>% 
  as.matrix()
#> Loaded Tensorflow version 2.6.0


fit_and_evaluate <- function(loss, final_activation) {
  model <- keras_model_sequential(input_shape = ncol(features)) %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dense(units = 1, activation = final_activation)
  
  model %>%
    compile(
      optimizer = "adam",
      loss = loss,
      metrics = c("mse", "poisson")
    ) %>%
    fit(features, target+1, batch_size = 256, epochs = 5)
  
  evaluate(model, features, target, batch_size = 256)
}



fit_and_evaluate(loss = "Poisson", final_activation = k_exp)
#>     loss      mse  poisson 
#> 1.044463 1.050129 1.044463
fit_and_evaluate(loss = "MSE", final_activation = "linear")
#>     loss      mse  poisson 
#> 1.021652 1.021652 1.031347
predict(model, test_data %>% as.matrix())

