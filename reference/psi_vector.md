# Function to calculate the vector psi given 2 vector of counts

Function to calculate the vector psi given 2 vector of counts

## Usage

``` r
psi_vector(old, new)
```

## Arguments

- old:

  A vector of original count distribution

- new:

  A vector with the new count distribution

## Value

A vector with psi index (value) a label and the table for with counts,
percents, woe.

## Examples

``` r

o <- sample(letters[1:5], size = 9000, prob = 1:5, replace = TRUE)
n <- sample(letters[1:5], size = 1000, prob = 1:5 + 4, replace = TRUE)

table(o)
#> o
#>    a    b    c    d    e 
#>  589 1218 1749 2440 3004 
table(n)
#> n
#>   a   b   c   d   e 
#> 130 163 187 242 278 

psi_vector(
  table(o),
  table(n)
)
#> n
#>            a            b            c            d            e 
#> 0.0443065980 0.0051462581 0.0002820861 0.0033067607 0.0101992077 

p <- psi_vector(
  table(o),
  table(n)
)

p
#> n
#>            a            b            c            d            e 
#> 0.0443065980 0.0051462581 0.0002820861 0.0033067607 0.0101992077 

sum(p)
#> [1] 0.06324091

psi_label(sum(p))
#> [1] insignificant change
#> Levels: insignificant change some minor change major shift in population
```
