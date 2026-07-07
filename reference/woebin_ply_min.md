# Minimalistic version of scorecard::woebin_ply

Minimalistic version of scorecard::woebin_ply

## Usage

``` r
woebin_ply_min(variable, bin, value = "woe")
```

## Arguments

- variable:

  A variable to get values asociated to a bin

- bin:

  A component (element) of \`scorecard::woebin\` output

- value:

  The value to return (a column of woebin table), defaults to "woe", can
  be: badprob, bin, etc.

## Examples

``` r

if(FALSE){

data(germancredit, package = "scorecard")

bins <- woebin2(germancredit, y = "creditability", x = "credit.amount")

variable <- head(germancredit$credit.amount, 10)

bin <- bins$credit.amount

woebin_ply_min(variable, bin)

woebin_ply_min(variable, bin, value = "posprob")

}
```
