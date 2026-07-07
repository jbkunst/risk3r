# Calculate predictive metrics for glm models

Calculate predictive metrics for glm models

## Usage

``` r
model_metrics(model, newdata = NULL)
```

## Arguments

- model:

  model

- newdata:

  Optional data frame

## Examples

``` r

N <- 10000
predicted <- runif(N)
actual <- rbinom(N, size = 1, prob = predicted)

daux <- data.frame(actual = actual, predicted = predicted)
m <- glm(actual ~ predicted, family = binomial, data = daux)

model_metrics(m)
#> ℹ Creating woe binning ...
#> # A tibble: 1 × 4
#>      ks   auc    iv  gini
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.497 0.829  1.67 0.659
model_metrics(m, newdata = head(daux, 100))
#> ℹ Creating woe binning ...
#> # A tibble: 1 × 4
#>      ks   auc    iv  gini
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.624 0.897  2.89 0.794
```
