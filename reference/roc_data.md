# Auxiliar functions for data plots

Auxiliar functions for data plots

## Usage

``` r
roc_data(actual, predicted)

ecdf_data(actual, predicted)
```

## Arguments

- actual:

  actual

- predicted:

  predicted

## Examples

``` r

N <- 10000
predicted <- runif(N)
actual <- rbinom(N, size = 1, prob = predicted)

roc_data(actual, predicted)
#> # A tibble: 10,001 × 2
#>        x        y
#>    <dbl>    <dbl>
#>  1     0 0       
#>  2     0 0.000197
#>  3     0 0.000394
#>  4     0 0.000591
#>  5     0 0.000788
#>  6     0 0.000984
#>  7     0 0.00118 
#>  8     0 0.00138 
#>  9     0 0.00158 
#> 10     0 0.00177 
#> # ℹ 9,991 more rows
ecdf_data(actual, predicted)
#> # A tibble: 10,000 × 3
#>    actual predicted     ecdf
#>     <int>     <dbl>    <dbl>
#>  1      0  0.000246 0.000203
#>  2      0  0.000289 0.000406
#>  3      0  0.000409 0.000610
#>  4      0  0.000419 0.000813
#>  5      0  0.000449 0.00102 
#>  6      0  0.000487 0.00122 
#>  7      0  0.000549 0.00142 
#>  8      0  0.000588 0.00163 
#>  9      0  0.000710 0.00183 
#> 10      0  0.000784 0.00203 
#> # ℹ 9,990 more rows
```
