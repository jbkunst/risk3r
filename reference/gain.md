# Gains

Gains

## Usage

``` r
gain(actual, predicted, percents = c(0.1, 0.2, 0.3, 0.4, 0.5))
```

## Arguments

- actual:

  A binary vector

- predicted:

  A numeric vector containing scores or probabilities

- percents:

  Values to calculate the gain

## Examples

``` r

N <- 10000

predicted <- runif(N)

actual <- rbinom(N, size = 1, prob = predicted)

gain(actual, predicted)
#> [1] 0.1932319 0.3662395 0.5170204 0.6427713 0.7509011

gain(actual, predicted, c(0.5, 1))
#> [1] 0.7509011 1.0000000
```
