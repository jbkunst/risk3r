# Get scorecard's woe_bin summary

Get scorecard's woe_bin summary

## Usage

``` r
woebin_summary(bins, sort = TRUE)
```

## Arguments

- bins:

  An output from \`scorecard::woebin\` or \`risk3r::woebin2\` functions.

- sort:

  Sort the data frame by information value. Default TRUE.

## Examples

``` r

if (FALSE) {
  data(germancredit, package = "scorecard")

  bins <- woebin2(
    germancredit,
    y = "creditability",
    # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
    no_cores = 0,
    method = "tree"
  )

  bin_summary <- woebin_summary(bins)
  bin_summary

  if (require(dplyr)) {
    dplyr::glimpse(bin_summary)

    dplyr::filter(bin_summary, !monotone, !factor)
  }
}
```
