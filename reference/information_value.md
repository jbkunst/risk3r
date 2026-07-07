# Calculate Information Value

Calculate Information Value

## Usage

``` r
information_value(actual, predicted)
```

## Arguments

- actual:

  A binary vector

- predicted:

  A vector: character, numeric or containing scores or probabilities

## Value

The KS statistic

## Examples

``` r

N <- 10000

predicted <- runif(N)

actual <- rbinom(N, size = 1, prob = predicted)

information_value(actual, predicted)
#> ℹ Creating woe binning ...
#> [1] 1.734067

predicted[sample(c(TRUE, FALSE), size = N, prob = c(1, 99), replace = TRUE)] <- NA

information_value(actual, predicted)
#> ℹ Creating woe binning ...
#> [1] 1.59469
```
