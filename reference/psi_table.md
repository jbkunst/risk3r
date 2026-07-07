# Function to calculate the vector table given 2 vector of counts

Function to calculate the vector table given 2 vector of counts

## Usage

``` r
psi_table(old, new)
```

## Arguments

- old:

  A vector of original distribution

- new:

  A vector with the new distribution

## Value

A table with psi index (value) a label and the table for with counts,
percents, woe.

## Examples

``` r

o <- factor(sample(letters[1:5], size = 9000, prob = 1:5, replace = TRUE))
n <- factor(sample(letters[1:5], size = 1000, prob = 1:5 + 4, replace = TRUE))

psi_table(o, n)
#> # A tibble: 5 × 6
#>   category count_old count_new percent_old percent_new      psi
#>   <fct>        <int>     <int>       <dbl>       <dbl>    <dbl>
#> 1 a              570       148      0.0633       0.148 0.0719  
#> 2 b             1175       160      0.131        0.16  0.00599 
#> 3 c             1808       191      0.201        0.191 0.000499
#> 4 d             2442       247      0.271        0.247 0.00229 
#> 5 e             3005       254      0.334        0.254 0.0218  
```
