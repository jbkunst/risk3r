# Relative Strength Index (RSI) and Chande Momentum Oscillator (CMO)

Relative Strength Index (RSI) and Chande Momentum Oscillator (CMO)

## Usage

``` r
rsi(x)

cmo(x)
```

## Arguments

- x:

  A numeric vector to calculate the RSI

## Examples

``` r

set.seed(123)

x <- round(cumsum(rnorm(24)) * 100, 0)
x <- abs(x)
x
#>  [1]  56  79  77  84  97 268 314 188 119  75 197 233 273 284 229 407 457 260 331
#> [20] 283 176 155  52  21

plot(x, type = "l")


rsi(x)
#> [1] 48.88606

# just test
rsi(rev(x))
#> [1] 51.11394

rsi(x) + rsi(rev(x))
#> [1] 100
```
