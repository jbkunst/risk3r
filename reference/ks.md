# Calculate Kolmogorov-Smirnov statistic

More metrics in the Metrics package https://github.com/mfrasco/Metrics

## Usage

``` r
ks(actual, predicted)
```

## Arguments

- actual:

  A binary vector

- predicted:

  A numeric vector containing scores or probabilities

## Value

The KS statistic

## Examples

``` r

N <- 10000

predicted <- runif(N)

actual <- rbinom(N, size = 1, prob = predicted)

ks(actual, predicted)
#> [1] 0.5055711

predicted[sample(c(TRUE, FALSE), size = N, prob = c(1, 99), replace = TRUE)] <- NA

ks(actual, predicted)
#> 100 of 10000 'predicted' values are NAs, they will be ignorated
#> [1] 0.5066255
```
