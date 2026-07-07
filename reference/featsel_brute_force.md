# An iterative process to eliminate variables by confidence interval or sign term

An iterative process to eliminate variables by confidence interval or
sign term

## Usage

``` r
featsel_brute_force(model, level = 0.95, verbose = TRUE)
```

## Arguments

- model:

  model

- level:

  the confidence level used to calculate the confidence interval

- verbose:

  Default set to TRUE.

## Examples

``` r

data("credit_woe")

m <- glm(bad ~ ., family = binomial, data = credit_woe)

m_featsel <- featsel_brute_force(m)
#> ℹ removing `residence_type_woe`.
#> ℹ removing `personal_net_income_woe`.
#> ℹ removing `months_in_residence_woe`.
```
