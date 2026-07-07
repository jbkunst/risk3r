# Shortcut featsel_loss_function_permutations

Shortcut featsel_loss_function_permutations

## Usage

``` r
featsel_loss_function_permutations(
  model,
  stat = function(x) quantile(x, 0.25),
  iterations = 100,
  ...
)
```

## Arguments

- model:

  model

- stat:

  = c("median", "mean", "min", "q25", "q75", "max"),

- iterations:

  Default 100.

- ...:

  Additional arguments for celavi::feature_selection

## Examples

``` r

data("credit_woe")

m <- glm(bad ~ ., family = binomial, data = credit_woe)

m_featsel <- featsel_loss_function_permutations(m, stat = min, iterations = 10)
#> ℹ Using 1 - AUCROC as loss function.
#> ℹ Fitting 1st model using 13 predictor variables.
#> 
#> ── Round #1 ──
#> 
#> ℹ Using `base::identity` as sampler.
#> ℹ Removing 3 variables. Fitting new model with 10 variables.
#> 
#> ── Round #2 ──
#> 
#> ℹ Using `base::identity` as sampler.
```
