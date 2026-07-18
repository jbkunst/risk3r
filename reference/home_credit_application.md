# Home Credit application training data

Training application data from the Home Credit Default Risk competition.
The data are credited to Home Credit and the competition's official data
page on Kaggle. The copy used to build this package is a third-party
mirror published by the Hugging Face user \`cantalapiedra\`; it is
provided as a convenient download location and is not the official
source.

## Usage

``` r
home_credit_application
```

## Format

A `data frame` with 307,511 observations and 122 variables:

- sk_id_curr:

  ID of loan in our sample

- target:

  Target variable (1 - client with payment difficulties: he/she had late
  payment more than X days on at least one of the first Y installments
  of the loan in our sample, 0 - all other cases)

- name_contract_type:

  Identification if loan is cash or revolving

- code_gender:

  Gender of the client

- flag_own_car:

  Flag if the client owns a car

- flag_own_realty:

  Flag if client owns a house or flat

- cnt_children:

  Number of children the client has

- amt_income_total:

  Income of the client

- amt_credit:

  Credit amount of the loan

- amt_annuity:

  Loan annuity

- amt_goods_price:

  For consumer loans it is the price of the goods for which the loan is
  given

- name_type_suite:

  Who was accompanying client when he was applying for the loan

- name_income_type:

  Client income type (for example, business, employment, or maternity
  leave).

- name_education_type:

  Level of highest education the client achieved

- name_family_status:

  Family status of the client

- name_housing_type:

  What is the housing situation of the client (renting, living with
  parents, ...)

- region_population_relative:

  Normalized population of region where client lives (higher number
  means the client lives in more populated region)

- days_birth:

  Client's age in days at the time of application

- days_employed:

  How many days before the application the person started current
  employment

- days_registration:

  How many days before the application did client change his
  registration

- days_id_publish:

  How many days before the application did client change the identity
  document with which he applied for the loan

- own_car_age:

  Age of client's car

- flag_mobil:

  Did client provide mobile phone (1=YES, 0=NO)

- flag_emp_phone:

  Did client provide work phone (1=YES, 0=NO)

- flag_work_phone:

  Did client provide home phone (1=YES, 0=NO)

- flag_cont_mobile:

  Was mobile phone reachable (1=YES, 0=NO)

- flag_phone:

  Did client provide home phone (1=YES, 0=NO)

- flag_email:

  Did client provide email (1=YES, 0=NO)

- occupation_type:

  What kind of occupation does the client have

- cnt_fam_members:

  How many family members does client have

- region_rating_client:

  Our rating of the region where client lives (1,2,3)

- region_rating_client_w_city:

  Our rating of the region where client lives with taking city into
  account (1,2,3)

- weekday_appr_process_start:

  On which day of the week did the client apply for the loan

- hour_appr_process_start:

  Approximately at what hour did the client apply for the loan

- reg_region_not_live_region:

  Flag if client's permanent address does not match contact address
  (1=different, 0=same, at region level)

- reg_region_not_work_region:

  Flag if client's permanent address does not match work address
  (1=different, 0=same, at region level)

- live_region_not_work_region:

  Flag if client's contact address does not match work address
  (1=different, 0=same, at region level)

- reg_city_not_live_city:

  Flag if client's permanent address does not match contact address
  (1=different, 0=same, at city level)

- reg_city_not_work_city:

  Flag if client's permanent address does not match work address
  (1=different, 0=same, at city level)

- live_city_not_work_city:

  Flag if client's contact address does not match work address
  (1=different, 0=same, at city level)

- organization_type:

  Type of organization where client works

- ext_source_1:

  Normalized score from external data source

- ext_source_2:

  Normalized score from external data source

- ext_source_3:

  Normalized score from external data source

- apartments_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- basementarea_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- years_beginexpluatation_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- years_build_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- commonarea_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- elevators_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- entrances_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- floorsmax_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- floorsmin_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- landarea_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- livingapartments_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- livingarea_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- nonlivingapartments_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- nonlivingarea_avg:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- apartments_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- basementarea_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- years_beginexpluatation_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- years_build_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- commonarea_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- elevators_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- entrances_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- floorsmax_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- floorsmin_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- landarea_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- livingapartments_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- livingarea_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- nonlivingapartments_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- nonlivingarea_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- apartments_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- basementarea_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- years_beginexpluatation_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- years_build_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- commonarea_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- elevators_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- entrances_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- floorsmax_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- floorsmin_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- landarea_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- livingapartments_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- livingarea_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- nonlivingapartments_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- nonlivingarea_medi:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- fondkapremont_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- housetype_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- totalarea_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- wallsmaterial_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- emergencystate_mode:

  Normalized information about building where the client lives, What is
  average (\_AVG suffix), modus (\_MODE suffix), median (\_MEDI suffix)
  apartment size, common area, living area, age of building, number of
  elevators, number of entrances, state of the building, number of floor

- obs_30_cnt_social_circle:

  How many observation of client's social surroundings with observable
  30 DPD (days past due) default

- def_30_cnt_social_circle:

  How many observation of client's social surroundings defaulted on 30
  DPD (days past due)

- obs_60_cnt_social_circle:

  How many observation of client's social surroundings with observable
  60 DPD (days past due) default

- def_60_cnt_social_circle:

  How many observation of client's social surroundings defaulted on 60
  (days past due) DPD

- days_last_phone_change:

  How many days before application did client change phone

- flag_document_2:

  Did client provide document 2

- flag_document_3:

  Did client provide document 3

- flag_document_4:

  Did client provide document 4

- flag_document_5:

  Did client provide document 5

- flag_document_6:

  Did client provide document 6

- flag_document_7:

  Did client provide document 7

- flag_document_8:

  Did client provide document 8

- flag_document_9:

  Did client provide document 9

- flag_document_10:

  Did client provide document 10

- flag_document_11:

  Did client provide document 11

- flag_document_12:

  Did client provide document 12

- flag_document_13:

  Did client provide document 13

- flag_document_14:

  Did client provide document 14

- flag_document_15:

  Did client provide document 15

- flag_document_16:

  Did client provide document 16

- flag_document_17:

  Did client provide document 17

- flag_document_18:

  Did client provide document 18

- flag_document_19:

  Did client provide document 19

- flag_document_20:

  Did client provide document 20

- flag_document_21:

  Did client provide document 21

- amt_req_credit_bureau_hour:

  Number of enquiries to Credit Bureau about the client one hour before
  application

- amt_req_credit_bureau_day:

  Number of enquiries to Credit Bureau about the client one day before
  application (excluding one hour before application)

- amt_req_credit_bureau_week:

  Number of enquiries to Credit Bureau about the client one week before
  application (excluding one day before application)

- amt_req_credit_bureau_mon:

  Number of enquiries to Credit Bureau about the client one month before
  application (excluding one week before application)

- amt_req_credit_bureau_qrt:

  Number of enquiries to Credit Bureau about the client 3 month before
  application (excluding one month before application)

- amt_req_credit_bureau_year:

  Number of enquiries to Credit Bureau about the client one day year
  (excluding last 3 months before application)

## Source

Official source: Home Credit Default Risk competition on Kaggle,
<https://www.kaggle.com/competitions/home-credit-default-risk/data>.
Third-party copy used to build the package:
<https://huggingface.co/cantalapiedra/poc_scoring_fair/resolve/main/application_train.csv?download=true>.

## References

Home Credit Default Risk competition,
<https://www.kaggle.com/competitions/home-credit-default-risk>.
