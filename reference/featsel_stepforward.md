# Feature selection vis stepwise forward

Feature selection vis stepwise forward

## Usage

``` r
featsel_stepforward(model, ...)
```

## Arguments

- model:

  model

- ...:

  Additional arguments for stats::step

## Examples

``` r

data("credit_woe")

m <- glm(bad ~ ., family = binomial, data = credit_woe)

featsel_stepforward(m)
#> Start:  AIC=49385.21
#> bad ~ 1
#> 
#>                           Df Deviance   AIC
#> + age_woe                  1    47900 47904
#> + marital_status_woe       1    48640 48644
#> + months_in_the_job_woe    1    48706 48710
#> + flag_res_phone_woe       1    48771 48775
#> + profession_code_woe      1    48921 48925
#> + area_code_res_phone_woe  1    49055 49059
#> + personal_net_income_woe  1    49072 49076
#> + sex_woe                  1    49176 49180
#> + payment_day_woe          1    49210 49214
#> + quant_add_cards_woe      1    49243 49247
#> + months_in_residence_woe  1    49251 49255
#> + residence_type_woe       1    49326 49330
#> + id_client_woe            1    49350 49354
#> <none>                          49383 49385
#> 
#> Step:  AIC=47903.96
#> bad ~ age_woe
#> 
#>                           Df Deviance   AIC
#> + flag_res_phone_woe       1    47391 47397
#> + area_code_res_phone_woe  1    47581 47587
#> + months_in_the_job_woe    1    47720 47726
#> + payment_day_woe          1    47756 47762
#> + marital_status_woe       1    47789 47795
#> + sex_woe                  1    47790 47796
#> + profession_code_woe      1    47794 47800
#> + personal_net_income_woe  1    47857 47863
#> + id_client_woe            1    47867 47873
#> + quant_add_cards_woe      1    47871 47877
#> + months_in_residence_woe  1    47898 47904
#> <none>                          47900 47904
#> + residence_type_woe       1    47900 47906
#> - age_woe                  1    49383 49385
#> 
#> Step:  AIC=47396.59
#> bad ~ age_woe + flag_res_phone_woe
#> 
#>                           Df Deviance   AIC
#> + months_in_the_job_woe    1    47226 47234
#> + payment_day_woe          1    47263 47271
#> + profession_code_woe      1    47293 47301
#> + marital_status_woe       1    47294 47302
#> + sex_woe                  1    47301 47309
#> + area_code_res_phone_woe  1    47320 47328
#> + id_client_woe            1    47358 47366
#> + quant_add_cards_woe      1    47379 47387
#> + personal_net_income_woe  1    47380 47388
#> <none>                          47391 47397
#> + months_in_residence_woe  1    47389 47397
#> + residence_type_woe       1    47390 47398
#> - flag_res_phone_woe       1    47900 47904
#> - age_woe                  1    48771 48775
#> 
#> Step:  AIC=47233.74
#> bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe
#> 
#>                           Df Deviance   AIC
#> + payment_day_woe          1    47091 47101
#> + profession_code_woe      1    47116 47126
#> + sex_woe                  1    47122 47132
#> + marital_status_woe       1    47143 47153
#> + area_code_res_phone_woe  1    47151 47161
#> + id_client_woe            1    47193 47203
#> + quant_add_cards_woe      1    47221 47231
#> <none>                          47226 47234
#> + months_in_residence_woe  1    47225 47235
#> + residence_type_woe       1    47226 47236
#> + personal_net_income_woe  1    47226 47236
#> - months_in_the_job_woe    1    47391 47397
#> - flag_res_phone_woe       1    47720 47726
#> - age_woe                  1    48148 48154
#> 
#> Step:  AIC=47101.32
#> bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe + 
#>     payment_day_woe
#> 
#>                           Df Deviance   AIC
#> + sex_woe                  1    46981 46993
#> + profession_code_woe      1    46990 47002
#> + marital_status_woe       1    47009 47021
#> + area_code_res_phone_woe  1    47020 47032
#> + id_client_woe            1    47058 47070
#> + quant_add_cards_woe      1    47088 47100
#> <none>                          47091 47101
#> + months_in_residence_woe  1    47091 47103
#> + residence_type_woe       1    47091 47103
#> + personal_net_income_woe  1    47091 47103
#> - payment_day_woe          1    47226 47234
#> - months_in_the_job_woe    1    47263 47271
#> - flag_res_phone_woe       1    47569 47577
#> - age_woe                  1    47988 47996
#> 
#> Step:  AIC=46993.36
#> bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe + 
#>     payment_day_woe + sex_woe
#> 
#>                           Df Deviance   AIC
#> + profession_code_woe      1    46876 46890
#> + marital_status_woe       1    46897 46911
#> + area_code_res_phone_woe  1    46908 46922
#> + id_client_woe            1    46948 46962
#> + quant_add_cards_woe      1    46964 46978
#> + personal_net_income_woe  1    46978 46992
#> <none>                          46981 46993
#> + months_in_residence_woe  1    46981 46995
#> + residence_type_woe       1    46981 46995
#> - sex_woe                  1    47091 47101
#> - payment_day_woe          1    47122 47132
#> - months_in_the_job_woe    1    47169 47179
#> - flag_res_phone_woe       1    47434 47444
#> - age_woe                  1    47794 47804
#> 
#> Step:  AIC=46890.37
#> bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe + 
#>     payment_day_woe + sex_woe + profession_code_woe
#> 
#>                           Df Deviance   AIC
#> + marital_status_woe       1    46791 46807
#> + area_code_res_phone_woe  1    46809 46825
#> + id_client_woe            1    46843 46859
#> + quant_add_cards_woe      1    46863 46879
#> <none>                          46876 46890
#> + personal_net_income_woe  1    46875 46891
#> + residence_type_woe       1    46876 46892
#> + months_in_residence_woe  1    46876 46892
#> - profession_code_woe      1    46981 46993
#> - sex_woe                  1    46990 47002
#> - payment_day_woe          1    47009 47021
#> - months_in_the_job_woe    1    47077 47089
#> - flag_res_phone_woe       1    47321 47333
#> - age_woe                  1    47454 47466
#> 
#> Step:  AIC=46806.62
#> bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe + 
#>     payment_day_woe + sex_woe + profession_code_woe + marital_status_woe
#> 
#>                           Df Deviance   AIC
#> + area_code_res_phone_woe  1    46729 46747
#> + id_client_woe            1    46758 46776
#> + quant_add_cards_woe      1    46784 46802
#> <none>                          46791 46807
#> + months_in_residence_woe  1    46790 46808
#> + personal_net_income_woe  1    46790 46808
#> + residence_type_woe       1    46790 46808
#> - marital_status_woe       1    46876 46890
#> - profession_code_woe      1    46897 46911
#> - sex_woe                  1    46906 46920
#> - payment_day_woe          1    46923 46937
#> - months_in_the_job_woe    1    46976 46990
#> - age_woe                  1    47113 47127
#> - flag_res_phone_woe       1    47223 47237
#> 
#> Step:  AIC=46746.8
#> bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe + 
#>     payment_day_woe + sex_woe + profession_code_woe + marital_status_woe + 
#>     area_code_res_phone_woe
#> 
#>                           Df Deviance   AIC
#> + id_client_woe            1    46697 46717
#> + quant_add_cards_woe      1    46722 46742
#> <none>                          46729 46747
#> + months_in_residence_woe  1    46728 46748
#> + personal_net_income_woe  1    46728 46748
#> + residence_type_woe       1    46729 46749
#> - area_code_res_phone_woe  1    46791 46807
#> - marital_status_woe       1    46809 46825
#> - profession_code_woe      1    46829 46845
#> - sex_woe                  1    46846 46862
#> - payment_day_woe          1    46858 46874
#> - months_in_the_job_woe    1    46918 46934
#> - flag_res_phone_woe       1    46949 46965
#> - age_woe                  1    47060 47076
#> 
#> Step:  AIC=46716.9
#> bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe + 
#>     payment_day_woe + sex_woe + profession_code_woe + marital_status_woe + 
#>     area_code_res_phone_woe + id_client_woe
#> 
#>                           Df Deviance   AIC
#> + quant_add_cards_woe      1    46690 46712
#> <none>                          46697 46717
#> + months_in_residence_woe  1    46696 46718
#> + personal_net_income_woe  1    46697 46719
#> + residence_type_woe       1    46697 46719
#> - id_client_woe            1    46729 46747
#> - area_code_res_phone_woe  1    46758 46776
#> - marital_status_woe       1    46777 46795
#> - profession_code_woe      1    46797 46815
#> - sex_woe                  1    46814 46832
#> - payment_day_woe          1    46827 46845
#> - months_in_the_job_woe    1    46886 46904
#> - flag_res_phone_woe       1    46917 46935
#> - age_woe                  1    47029 47047
#> 
#> Step:  AIC=46712.06
#> bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe + 
#>     payment_day_woe + sex_woe + profession_code_woe + marital_status_woe + 
#>     area_code_res_phone_woe + id_client_woe + quant_add_cards_woe
#> 
#>                           Df Deviance   AIC
#> <none>                          46690 46712
#> + months_in_residence_woe  1    46689 46713
#> + residence_type_woe       1    46690 46714
#> + personal_net_income_woe  1    46690 46714
#> - quant_add_cards_woe      1    46697 46717
#> - id_client_woe            1    46722 46742
#> - area_code_res_phone_woe  1    46751 46771
#> - marital_status_woe       1    46764 46784
#> - profession_code_woe      1    46787 46807
#> - sex_woe                  1    46814 46834
#> - payment_day_woe          1    46819 46839
#> - months_in_the_job_woe    1    46871 46891
#> - flag_res_phone_woe       1    46903 46923
#> - age_woe                  1    47013 47033
#> 
#> Call:  glm(formula = bad ~ age_woe + flag_res_phone_woe + months_in_the_job_woe + 
#>     payment_day_woe + sex_woe + profession_code_woe + marital_status_woe + 
#>     area_code_res_phone_woe + id_client_woe + quant_add_cards_woe, 
#>     family = binomial(link = logit), data = model$data)
#> 
#> Coefficients:
#>             (Intercept)                  age_woe       flag_res_phone_woe  
#>                 -1.4031                   0.5898                   0.6771  
#>   months_in_the_job_woe          payment_day_woe                  sex_woe  
#>                  0.5859                   0.8879                   0.8167  
#>     profession_code_woe       marital_status_woe  area_code_res_phone_woe  
#>                  0.5165                   0.3703                   0.4890  
#>           id_client_woe      quant_add_cards_woe  
#>                  1.0047                   0.2452  
#> 
#> Degrees of Freedom: 49693 Total (i.e. Null);  49683 Residual
#> Null Deviance:       49380 
#> Residual Deviance: 46690     AIC: 46710
```
