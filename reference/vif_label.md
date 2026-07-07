# Get labels for VIF values

Get labels for VIF values

## Usage

``` r
vif_label(x)
```

## Arguments

- x:

  A numeric vector

## Examples

``` r

set.seed(123)

vifs <- sample(1:10)

vif_label(vifs)
#>  [1] low (<5)       high (>= 10)   low (<5)       moderate (<10) moderate (<10)
#>  [6] moderate (<10) low (<5)       moderate (<10) moderate (<10) low (<5)      
#> Levels: low (<5) moderate (<10) high (>= 10)
```
