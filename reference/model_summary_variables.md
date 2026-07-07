# Get summary of model

Get summary of model

## Usage

``` r
model_summary_variables(
  model,
  coef_sign = 1,
  limit_significance = 0.05,
  limit_iv = 0.02,
  limit_corr = 0.6,
  limit_vif = 5
)

model_corr_variables(model)

model_vif_variables(model)

model_iv_variables(model)
```

## Arguments

- model:

  model

- coef_sign:

  Sign to compare estimaes.

- limit_significance:

  Limit for Significance.

- limit_iv:

  Limit for Information Value.

- limit_corr:

  Limit for correlation max between variables.

- limit_vif:

  Limit for VIF.

## Examples

``` r

data("credit_woe")

data("credit_woe")

m <- glm(bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe +
   payment_day_woe + sex_woe + profession_code_woe + marital_status_woe,
   family = binomial, data = head(credit_woe, 10000)
   )

model_partials(m)
#> ℹ Fitting and evaluating model with 1 variables: age_woe
#> ℹ Creating woe binning ...
#> ℹ Fitting and evaluating model with 2 variables: age_woe, flag_res_phone_woe
#> ℹ Creating woe binning ...
#> ℹ Fitting and evaluating model with 3 variables: age_woe, flag_res_phone_woe, m...
#> ℹ Creating woe binning ...
#> ℹ Fitting and evaluating model with 4 variables: age_woe, flag_res_phone_woe, m...
#> ℹ Creating woe binning ...
#> ℹ Fitting and evaluating model with 5 variables: age_woe, flag_res_phone_woe, m...
#> ℹ Creating woe binning ...
#> ℹ Fitting and evaluating model with 6 variables: age_woe, flag_res_phone_woe, m...
#> ℹ Creating woe binning ...
#> ℹ Fitting and evaluating model with 7 variables: age_woe, flag_res_phone_woe, m...
#> ℹ Creating woe binning ...
#> # A tibble: 7 × 5
#>   variable                 ks   auc    iv  gini
#>   <fct>                 <dbl> <dbl> <dbl> <dbl>
#> 1 age_woe               0.204 0.624 0.190 0.248
#> 2 flag_res_phone_woe    0.207 0.641 0.248 0.283
#> 3 months_in_the_job_woe 0.223 0.650 0.276 0.301
#> 4 payment_day_woe       0.238 0.656 0.304 0.312
#> 5 sex_woe               0.247 0.659 0.309 0.319
#> 6 profession_code_woe   0.244 0.661 0.329 0.321
#> 7 marital_status_woe    0.244 0.662 0.335 0.324

model_summary_variables(m)
#> Correlation computed with
#> • Method: 'pearson'
#> • Missing treated using: 'pairwise.complete.obs'
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> # A tibble: 8 × 15
#>   term    estimate std.error statistic  p.value correlation_max      iv iv_label
#>   <chr>      <dbl>     <dbl>     <dbl>    <dbl>           <dbl>   <dbl> <fct>   
#> 1 (Inter…   -1.40     0.0262    -53.4  0                NA      NA      NA      
#> 2 age_woe    0.673    0.0736      9.14 6.53e-20          0.465   0.208  medium  
#> 3 flag_r…    0.853    0.0898      9.50 2.19e-21          0.0637  0.0749 weak    
#> 4 months…    0.623    0.0986      6.32 2.64e-10          0.339   0.0948 weak    
#> 5 paymen…    0.780    0.172       4.53 6.04e- 6          0.0640  0.0185 unpredi…
#> 6 sex_woe    0.744    0.158       4.71 2.49e- 6          0.107   0.0265 weak    
#> 7 profes…    0.420    0.120       3.50 4.68e- 4          0.354   0.0537 weak    
#> 8 marita…    0.294    0.0957      3.07 2.14e- 3          0.465   0.0866 weak    
#> # ℹ 7 more variables: vif <dbl>, vif_label <fct>, dummy_significance <lgl>,
#> #   dummy_sign <lgl>, dummy_iv <lgl>, dummy_correlation <lgl>, dummy_vif <lgl>

model_corr_variables(m)
#> Correlation computed with
#> • Method: 'pearson'
#> • Missing treated using: 'pairwise.complete.obs'
#> # A tibble: 49 × 3
#>    term                  term2                  cor
#>    <fct>                 <fct>                <dbl>
#>  1 age_woe               age_woe            NA     
#>  2 flag_res_phone_woe    age_woe             0.0633
#>  3 months_in_the_job_woe age_woe             0.339 
#>  4 payment_day_woe       age_woe             0.0415
#>  5 sex_woe               age_woe             0.107 
#>  6 profession_code_woe   age_woe             0.354 
#>  7 marital_status_woe    age_woe             0.465 
#>  8 age_woe               flag_res_phone_woe  0.0633
#>  9 flag_res_phone_woe    flag_res_phone_woe NA     
#> 10 months_in_the_job_woe flag_res_phone_woe  0.0437
#> # ℹ 39 more rows

model_vif_variables(m)
#> # A tibble: 7 × 3
#>   term                    vif vif_label
#>   <fct>                 <dbl> <fct>    
#> 1 age_woe                1.47 low (<5) 
#> 2 flag_res_phone_woe     1.01 low (<5) 
#> 3 months_in_the_job_woe  1.13 low (<5) 
#> 4 payment_day_woe        1.01 low (<5) 
#> 5 sex_woe                1.02 low (<5) 
#> 6 profession_code_woe    1.09 low (<5) 
#> 7 marital_status_woe     1.27 low (<5) 

model_iv_variables(m)
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> ℹ Creating woe binning ...
#> ℹ The option bin_close_right was set to FALSE.
#> # A tibble: 7 × 3
#>   term                      iv iv_label    
#>   <chr>                  <dbl> <fct>       
#> 1 age_woe               0.208  medium      
#> 2 flag_res_phone_woe    0.0749 weak        
#> 3 months_in_the_job_woe 0.0948 weak        
#> 4 payment_day_woe       0.0185 unpredictive
#> 5 sex_woe               0.0265 weak        
#> 6 profession_code_woe   0.0537 weak        
#> 7 marital_status_woe    0.0866 weak        
```
