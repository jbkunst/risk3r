# Get labels for Information Values

Get labels for Information Values

## Usage

``` r
iv_label(x)
```

## Arguments

- x:

  A numeric vector

## Examples

``` r

N <- 10000

predicted <- runif(N)

actual <- rbinom(N, size = 1, prob = predicted)

iv_label(information_value(actual, predicted))
#> ℹ Creating woe binning ...
#> [1] suspicious
#> Levels: unpredictive weak medium strong suspicious

predicted <- runif(N)

iv_label(information_value(actual, predicted))
#> ℹ Creating woe binning ...
#> [1] unpredictive
#> Levels: unpredictive weak medium strong suspicious
```
