# Scorecard helpers and others funcions

## `woebin_summary`

``` r

bins <- woebin(germancredit, y = "creditability")
#> ℹ Creating woe binning ...
#> ✔ Binning on 1000 rows and 21 columns in 00:00:01

binssummary <- woebin_summary(bins)

binssummary
#> # A tibble: 20 × 15
#>    variable  n_categories      iv       ks   hhi count_distr_max count_distr_min
#>    <chr>            <int>   <dbl>    <dbl> <dbl>           <dbl>           <dbl>
#>  1 status.o…            3 6.39e-1 3.67e- 1 0.333           0.543           0.063
#>  2 credit.h…            4 2.92e-1 1.80e- 1 0.25            0.53            0.088
#>  3 duration…            5 2.83e-1 1.92e- 1 0.2             0.399           0.07 
#>  4 savings.…            3 1.91e-1 1.87e- 1 0.333           0.603           0.103
#>  5 credit.a…            5 1.81e-1 1.58e- 1 0.2             0.382           0.05 
#>  6 purpose              3 1.53e-1 1.79e- 1 0.333           0.608           0.112
#>  7 age.in.y…            5 1.30e-1 1.47e- 1 0.2             0.373           0.079
#>  8 property             4 1.13e-1 1.17e- 1 0.25            0.332           0.154
#>  9 present.…            4 8.53e-2 1.20e- 1 0.25            0.339           0.174
#> 10 housing              3 8.33e-2 1.33e- 1 0.333           0.713           0.108
#> 11 other.in…            2 5.76e-2 9.62e- 2 0.5             0.814           0.186
#> 12 installm…            3 2.56e-2 7.71e- 2 0.333           0.476           0.157
#> 13 other.de…            2 1.64e-2 2.67e- 2 0.5             0.948           0.052
#> 14 number.o…            2 1.01e-2 4.81e- 2 0.5             0.633           0.367
#> 15 personal…            4 8.84e-3 3.33e- 2 0.25            0.548           0.05 
#> 16 job                  3 8.10e-3 3.14e- 2 0.333           0.63            0.148
#> 17 telephone            2 6.38e-3 3.90e- 2 0.5             0.596           0.404
#> 18 present.…            4 3.59e-3 2.24e- 2 0.25            0.413           0.13 
#> 19 number.o…            2 4.34e-5 2.38e- 3 0.5             0.845           0.155
#> 20 foreign.…            1 0       6.51e-17 1               1               1    
#> # ℹ 8 more variables: has_missing <lgl>, has_special_values <lgl>,
#> #   monotone <lgl>, factor <lgl>, breaks <list>, iv_lbl <fct>, hhi_lbl <fct>,
#> #   distribution <chr>

binssummary %>% 
  select(variable, ks, iv, iv_lbl,
         hhi, hhi_lbl, distribution)
#> # A tibble: 20 × 7
#>    variable                         ks      iv iv_lbl   hhi hhi_lbl distribution
#>    <chr>                         <dbl>   <dbl> <fct>  <dbl> <fct>   <chr>       
#>  1 status.of.existing.checki… 3.67e- 1 6.39e-1 suspi… 0.333 high c… "▇▁▆"       
#>  2 credit.history             1.80e- 1 2.92e-1 medium 0.25  modera… "▂▇▁▅"      
#>  3 duration.in.month          1.92e- 1 2.83e-1 medium 0.2   modera… "▂▇▇▂▂"     
#>  4 savings.account.and.bonds  1.87e- 1 1.91e-1 medium 0.333 high c… "▇▂▃"       
#>  5 credit.amount              1.58e- 1 1.81e-1 medium 0.2   modera… "▆▂▇▅▁"     
#>  6 purpose                    1.79e- 1 1.53e-1 medium 0.333 high c… "▂▃▇"       
#>  7 age.in.years               1.47e- 1 1.30e-1 medium 0.2   modera… "▅▂▆▂▇"     
#>  8 property                   1.17e- 1 1.13e-1 medium 0.25  modera… "▇▆▇▃"      
#>  9 present.employment.since   1.20e- 1 8.53e-2 weak   0.25  modera… "▆▇▅▆"      
#> 10 housing                    1.33e- 1 8.33e-2 weak   0.333 high c… "▂▇▁"       
#> 11 other.installment.plans    9.62e- 2 5.76e-2 weak   0.5   high c… "▂▇"        
#> 12 installment.rate.in.perce… 7.71e- 2 2.56e-2 weak   0.333 high c… "▆▂▇"       
#> 13 other.debtors.or.guaranto… 2.67e- 2 1.64e-2 unpre… 0.5   high c… "▇▁"        
#> 14 number.of.existing.credit… 4.81e- 2 1.01e-2 unpre… 0.5   high c… "▇▅"        
#> 15 personal.status.and.sex    3.33e- 2 8.84e-3 unpre… 0.25  modera… "▁▅▇▂"      
#> 16 job                        3.14e- 2 8.10e-3 unpre… 0.333 high c… "▃▇▂"       
#> 17 telephone                  3.90e- 2 6.38e-3 unpre… 0.5   high c… "▇▆"        
#> 18 present.residence.since    2.24e- 2 3.59e-3 unpre… 0.25  modera… "▂▆▃▇"      
#> 19 number.of.people.being.li… 2.38e- 3 4.34e-5 unpre… 0.5   high c… "▇▂"        
#> 20 foreign.worker             6.51e-17 0       unpre… 1     high c… ""
```

## `woebin_ply_min`

The `woebin_ply_min` function don’t need a data frame argument.

``` r

variable <- head(germancredit$credit.amount, 6)

bin <- bins$credit.amount

bin
#>         variable         bin count count_distr   neg   pos   posprob
#>           <char>      <char> <int>       <num> <int> <int>     <num>
#> 1: credit.amount [-Inf,1400)   267       0.267   185    82 0.3071161
#> 2: credit.amount [1400,1800)   105       0.105    87    18 0.1714286
#> 3: credit.amount [1800,4000)   382       0.382   287    95 0.2486911
#> 4: credit.amount [4000,9200)   196       0.196   120    76 0.3877551
#> 5: credit.amount [9200, Inf)    50       0.050    21    29 0.5800000
#>            woe       bin_iv  total_iv breaks is_special_values
#>          <num>        <num>     <num> <char>            <lgcl>
#> 1:  0.03366128 0.0003045545 0.1812204   1400             FALSE
#> 2: -0.72823850 0.0468153322 0.1812204   1800             FALSE
#> 3: -0.25830746 0.0241086966 0.1812204   4000             FALSE
#> 4:  0.39053946 0.0319870413 0.1812204   9200             FALSE
#> 5:  1.17007125 0.0780047502 0.1812204    Inf             FALSE

woebin_ply_min(variable, bin)
#> ℹ Converting into woe values ...
#> [1]  0.03366128  0.39053946 -0.25830746  0.39053946  0.39053946  0.39053946

woebin_ply_min(variable, bin, value = "posprob")
#> ℹ Converting into woe values ...
#> [1] 0.3071161 0.3877551 0.2486911 0.3877551 0.3877551 0.3877551
```

## `woebin_cor_iv`

``` r

datcor <- woebin_cor_iv(germancredit, bins)
#> ℹ Converting into woe values ...
#> ✔ Woe transformating on 1000 rows and 20 columns in 00:00:01
#> Warning in stats::cor(x = x, y = y, use = use, method = method): the standard
#> deviation is zero
#> Correlation computed with
#> • Method: 'pearson'
#> • Missing treated using: 'pairwise.complete.obs'
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the corrr package.
#>   Please report the issue at <https://github.com/tidymodels/corrr/issues>.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.

datcor
#> # A tibble: 400 × 7
#>    var1                        var2        r var1_iv var1_rank var2_iv var2_rank
#>    <fct>                       <fct>   <dbl>   <dbl>     <int>   <dbl>     <int>
#>  1 status.of.existing.checkin… stat… NA        0.639         1  0.639          1
#>  2 status.of.existing.checkin… cred…  0.203    0.639         1  0.292          2
#>  3 status.of.existing.checkin… dura…  0.0985   0.639         1  0.283          3
#>  4 status.of.existing.checkin… savi…  0.219    0.639         1  0.191          4
#>  5 status.of.existing.checkin… cred…  0.0916   0.639         1  0.181          5
#>  6 status.of.existing.checkin… purp…  0.133    0.639         1  0.153          6
#>  7 status.of.existing.checkin… age.…  0.129    0.639         1  0.130          7
#>  8 status.of.existing.checkin… prop…  0.0652   0.639         1  0.113          8
#>  9 status.of.existing.checkin… pres…  0.119    0.639         1  0.0853         9
#> 10 status.of.existing.checkin… hous…  0.112    0.639         1  0.0833        10
#> # ℹ 390 more rows

cor_limit <- 0.15

datcor %>%
  filter(var1 != var2) %>%
  mutate(
    cor_conflict = ifelse(abs(r) > cor_limit, TRUE, FALSE),
    variable_to_remove = ifelse(
      cor_conflict,
      ifelse(var1 > var2, var2, var1),
      NA
    )
  )
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `variable_to_remove = ifelse(cor_conflict, ifelse(var1 > var2,
#>   var2, var1), NA)`.
#> Caused by warning in `Ops.factor()`:
#> ! '>' not meaningful for factors
#> # A tibble: 380 × 9
#>    var1            var2       r var1_iv var1_rank var2_iv var2_rank cor_conflict
#>    <fct>           <fct>  <dbl>   <dbl>     <int>   <dbl>     <int> <lgl>       
#>  1 status.of.exis… cred… 0.203    0.639         1  0.292          2 TRUE        
#>  2 status.of.exis… dura… 0.0985   0.639         1  0.283          3 FALSE       
#>  3 status.of.exis… savi… 0.219    0.639         1  0.191          4 TRUE        
#>  4 status.of.exis… cred… 0.0916   0.639         1  0.181          5 FALSE       
#>  5 status.of.exis… purp… 0.133    0.639         1  0.153          6 FALSE       
#>  6 status.of.exis… age.… 0.129    0.639         1  0.130          7 FALSE       
#>  7 status.of.exis… prop… 0.0652   0.639         1  0.113          8 FALSE       
#>  8 status.of.exis… pres… 0.119    0.639         1  0.0853         9 FALSE       
#>  9 status.of.exis… hous… 0.112    0.639         1  0.0833        10 FALSE       
#> 10 status.of.exis… othe… 0.0489   0.639         1  0.0576        11 FALSE       
#> # ℹ 370 more rows
#> # ℹ 1 more variable: variable_to_remove <lgl>
```
