# Metrics

Metrics from Metrics package https://github.com/mfrasco/Metrics

## Usage

``` r
metrics(actual, predicted)
```

## Arguments

- actual:

  A binary vector

- predicted:

  A numeric vector containing scores or probabilities

## Value

A data frame with usual and opinionated metrics

## Examples

``` r

N <- 10000

predicted <- runif(N)

actual <- rbinom(N, size = 1, prob = predicted)

metrics(actual, predicted)
#> ℹ Creating woe binning ...
#> # A tibble: 1 × 4
#>      ks   auc    iv  gini
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.494 0.833  1.83 0.666

predicted[sample(c(TRUE, FALSE), size = N, prob = c(1, 99), replace = TRUE)] <- NA

metrics(actual, predicted)
#> 111 of 10000 'predicted' values are NAs, they will be ignorated
#> ℹ Creating woe binning ...
#> # A tibble: 1 × 4
#>      ks   auc    iv  gini
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.494 0.825  1.63 0.650
```
