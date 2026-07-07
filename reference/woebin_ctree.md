# Interface scorecard::woebin for partykiy::ctree

Interface scorecard::woebin for partykiy::ctree

## Usage

``` r
woebin_ctree(
  y,
  x,
  namevar = "variable",
  count_distr_limit = 0.05,
  control = partykit::ctree_control()
)
```

## Arguments

- y:

  A vector of response. Usually 0-1

- x:

  A predictive variable

- namevar:

  a character element

- count_distr_limit:

  The minimum count distribution percentage. Accepted range: 0.01-0.2;
  Defaults to 0.05.

- control:

  a ctree::ctree_control list element

## Examples

``` r

if(FALSE){

data(germancredit, package = "scorecard")

y <- germancredit$creditability

x <- germancredit$duration.in.month

woebin_ctree(y, x, "duration", count_distr_limit = 0.05)

woebin_ctree(y, x, "duration", count_distr_limit = 0.2)

woebin_ctree(
  y,
  x,
  "duration",
  count_distr_limit = 0.05,
  control = partykit::ctree_control(alpha = 0.5)
)

x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA
woebin_ctree(y, x, "duration")

x <- germancredit$purpose
woebin_ctree(y, x, "purpose")

x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA
woebin_ctree(y, x, "purpose_with_na")

}
```
