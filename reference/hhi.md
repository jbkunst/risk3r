# Herfindahl-Hirschman Index

Herfindahl-Hirschman Index

## Usage

``` r
hhi(x)
```

## Arguments

- x:

  A vector to obtain the HHI.

## Examples

``` r

x <- sample(LETTERS[1:10], size = 1000, replace = TRUE, prob = log(1:10))

hhi(x)
#> [1] 0.123522

plot(table(x), main = hhi_label(hhi(x)))


x <- sample(LETTERS[1:5], size = 1000, replace = TRUE, prob = exp(1:5))

hhi(x)
#> [1] 0.472534

plot(table(x), main = hhi_label(hhi(x)))
```
