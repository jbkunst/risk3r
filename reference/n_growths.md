# Number of up/growths

Number of up/growths

## Usage

``` r
n_growths(x)
```

## Arguments

- x:

  A numeric vector to calculate the number of growths

## Examples

``` r

set.seed(123)

x <- round(cumsum(rnorm(24)) * 100, 0)
x <- abs(x)
x
#>  [1]  56  79  77  84  97 268 314 188 119  75 197 233 273 284 229 407 457 260 331
#> [20] 283 176 155  52  21

plot(x, type = "l")


n_growths(x)
#> [1] 12

n_growths(c(1, 2, 0))
#> [1] 1

n_growths(c(0))
#> [1] 0
```
