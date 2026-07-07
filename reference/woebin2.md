# woebin fix for windows

This is a wrapper for scorecard::woebin, this fixes some problem to
replicate the cuts given on windows machines.

## Usage

``` r
woebin2(
  dt,
  y,
  x = NULL,
  var_skip = NULL,
  breaks_list = NULL,
  special_values = NULL,
  stop_limit = 0.1,
  count_distr_limit = 0.05,
  bin_num_limit = 8,
  positive = "bad|1",
  no_cores = NULL,
  print_step = 0L,
  method = "tree",
  save_breaks_list = NULL,
  ignore_const_cols = TRUE,
  ignore_datetime_cols = TRUE,
  check_cate_num = TRUE,
  replace_blank_inf = TRUE,
  control = partykit::ctree_control()
)
```

## Arguments

- dt:

  A data frame with both x (predictor/feature) and y (response/label)
  variables.

- y:

  Name of y variable.

- x:

  Name of x variables. Defaults to NULL. If x is NULL, then all columns
  except y and var_skip are counted as x variables.

- var_skip:

  Name of variables that will skip for binning. Defaults to NULL.

- breaks_list:

  List of break points, Defaults to NULL. If it is not NULL, variable
  binning will based on the provided breaks.

- special_values:

  the values specified in special_values will be in separate bins.
  Defaults to NULL.

- stop_limit:

  Stop binning segmentation when information value gain ratio less than
  the 'stop_limit' if using tree method; or stop binning merge when the
  chi-square of each neighbor bins are larger than the threshold under
  significance level of 'stop_limit' and freedom degree of 1 if using
  chimerge method. Accepted range: 0-0.5; Defaults to 0.1. If it is 'N',
  each x value is a bin.

- count_distr_limit:

  The minimum count distribution percentage. Accepted range: 0.01-0.2;
  Defaults to 0.05.

- bin_num_limit:

  Integer. The maximum number of binning. Defaults to 8.

- positive:

  Value of positive class, defaults to "bad\|1".

- no_cores:

  Number of CPU cores for parallel computation. Defaults to 90 percent
  of total cpu cores.

- print_step:

  A non-negative integer. Defaults to 1. If print_step\>0, print
  variable names by each print_step-th iteration. If print_step=0 or
  no_cores\>1, no message is print.

- method:

  Four methods are provided, "tree" and "chimerge" for optimal binning
  that support both numerical and categorical variables, and 'width' and
  'freq' for equal binning that support numerical variables only.
  Defaults to "tree".

- save_breaks_list:

  A string. The file name to save breaks_list. Defaults to None.

- ignore_const_cols:

  Logical. Ignore constant columns. Defaults to TRUE.

- ignore_datetime_cols:

  Logical. Ignore datetime columns. Defaults to TRUE.

- check_cate_num:

  Logical. Check whether the number of unique values in categorical
  columns larger than 50. It might make the binning process slow if
  there are too many unique categories. Defaults to TRUE.

- replace_blank_inf:

  Logical. Replace blank values with NA and infinite with -1. Defaults
  to TRUE.

- control:

  a ctree::ctree_control list element

## Details

See https://github.com/ShichenXie/scorecard/issues/50 for more details.

## Examples

``` r

if (FALSE) {
  data(germancredit, package = "scorecard")

  bins <- woebin2(
    dt = germancredit,
    y = "creditability",
    # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
    no_cores = 0,
    method = "tree"
  )

  bins

  if (require(scorecard)) {
    library(scorecard)
    options(bin_close_right = TRUE)
  }

  bins <- woebin2(
    dt = germancredit,
    y = "creditability",
    # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
    no_cores = 0,
    method = "tree"
  )

  bins

  bins_ctree <- woebin2(
    dt = germancredit,
    y = "creditability",
    method = "ctree",
    no_cores = 0,
    control = partykit::ctree_control(alpha = 1, maxdepth = 4)
  )

  woebin_summary(bins)
  woebin_summary(bins_ctree)
}
```
