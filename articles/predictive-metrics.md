# Predictive Metrics

``` r

library(risk3r)

N <- 10000

predicted <- runif(N)

actual <- rbinom(N, size = 1, prob = predicted)

metrics(actual, predicted)
#> ℹ Creating woe binning ...
#> # A tibble: 1 × 4
#>      ks   auc    iv  gini
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.504 0.836  1.79 0.672
```

Remove `NA` if are present.

``` r

predicted2 <- predicted
predicted2[sample(c(TRUE, FALSE), size = N, prob = c(1, 99), replace = TRUE)] <- NA

metrics(actual, predicted2)
#> 104 of 10000 'predicted' values are NAs, they will be ignorated
#> ℹ Creating woe binning ...
#> # A tibble: 1 × 4
#>      ks   auc    iv  gini
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.505 0.828  1.69 0.656
```

## Using a glm model

``` r

daux <- data.frame(actual = actual, predicted = predicted)

m <- glm(actual ~ predicted, family = binomial, data = daux)

model_metrics(m)
#> ℹ Creating woe binning ...
#> # A tibble: 1 × 4
#>      ks   auc    iv  gini
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.504 0.836  1.80 0.672
```

Works with new data.

``` r

new_daux <- head(daux, 1000)

model_metrics(m, newdata = new_daux)
#> ℹ Creating woe binning ...
#> # A tibble: 1 × 4
#>      ks   auc    iv  gini
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.526 0.847  2.16 0.694
```
