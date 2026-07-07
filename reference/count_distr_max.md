# Get the max proportion

Get the max proportion

## Usage

``` r
count_distr_max(x)
```

## Arguments

- x:

  The value to get the maximun proportion.

## Examples

``` r

x <- sample(letters[1:3], prob = 1:3, size = 10000, replace = TRUE)

count_distr_max(x)
#> [1] 0.499
```
