# Create a tidy data frame with a correlations and IV

Create a tidy data frame with a correlations and IV

## Usage

``` r
woebin_cor_iv(dt, bins, upper = FALSE, plot = TRUE)
```

## Arguments

- dt:

  A data frame to apply \`scorecard::woebin_ply\` and calculate
  correlations.

- bins:

  An output from \`scorecard::woebin\` to create woe variables

- upper:

  upper

- plot:

  plot

## Examples

``` r

if (FALSE) {
  data(germancredit, package = "scorecard")

  vars <- c(
    "creditability", "duration.in.month", "credit.history",
    "purpose", "status.of.existing.checking.account", "property"
  )

  dat <- germancredit[, vars]

  bins <- woebin2(dat, y = "creditability", stop_limit = 0.0000001)

  woebin_cor_iv(dat, bins)

  datcor <- woebin_cor_iv(dat, bins)

  library(dplyr)

  cor_limit <- 0.15

  datcor %>%
    filter(variable_1 != variable_2) %>%
    mutate(
      cor_conflict = ifelse(abs(cor) > cor_limit, TRUE, FALSE),
      variable_to_remove = ifelse(
        cor_conflict,
        ifelse(iv_variable_1 > iv_variable_2, variable_2, variable_1),
        NA
      )
    )
}
```
