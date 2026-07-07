# Apply consecutive filter expressions to a data frame and return results

Apply consecutive filter expressions to a data frame and return results

## Usage

``` r
apply_filters(
  data,
  filters,
  summary_fun = function(data) {
     dplyr::summarise(data)
 },
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame.

- filters:

  A list of named quoted expressions.

- summary_fun:

  A summary function. Recive a data frame and return a data frame.

- verbose:

  A logical value

## Examples

``` r

require(rlang) # summarise, n_distinct, n
#> Loading required package: rlang
require(dplyr) # quo
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

data <- mtcars

filters <- list(
  `am equals to 1`  = quo(am == 1),
  `carb = 1!!`      = quo(carb == 1),
  `hp less than 90` = quo(hp < 90)
)

summary_fun <- function(data) {
  summarise(
    data,
    n = dplyr::n(),
    cyl_mean = mean(cyl),
    n_unique_cyl = n_distinct(cyl)
    )
}

results <- apply_filters(data, filters, summary_fun)
#> ℹ Step 1 `initial` ==> TRUE
#> ℹ Step 2 `am equals to 1` ==> am == 1
#> ℹ Step 3 `carb = 1!!` ==> carb == 1
#> ℹ Step 4 `hp less than 90` ==> hp < 90
#> ℹ Step 5 `final` ==> TRUE

results
#> $data
#>                 mpg cyl disp hp drat    wt  qsec vs am gear carb
#> Fiat 128       32.4   4 78.7 66 4.08 2.200 19.47  1  1    4    1
#> Toyota Corolla 33.9   4 71.1 65 4.22 1.835 19.90  1  1    4    1
#> Fiat X1-9      27.3   4 79.0 66 4.08 1.935 18.90  1  1    4    1
#> 
#> $summary
#>            filter rows rows_removed  n cyl_mean n_unique_cyl
#> 1         initial   32           NA 32 6.187500            3
#> 2  am equals to 1   13           19 13 5.076923            3
#> 3      carb = 1!!    4            9  4 4.000000            1
#> 4 hp less than 90    3            1  3 4.000000            1
#> 5           final    3            0  3 4.000000            1
#> 
```
