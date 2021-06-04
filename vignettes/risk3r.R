## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=FALSE, message=FALSE-----------------------------------------------
# general
library(dplyr)

# main
library(risk3r)
library(scorecard)

# descriptive & premodelling
library(skimr)
library(moments)
library(rsample)

# post modelling
library(performance)
library(broom)

## -----------------------------------------------------------------------------
PARAMETERS <- list(
  SEED = 12345,
  PARALLEL = TRUE,
  P_TRAIN = 0.8,
  P_CONCENTRATION = .95,
  IV_MIN = 0.02, # 0.2 WEAK >=
  COUNT_DISTR_LIMIT = 0.05,
  POINTS0 = 600,
  PDO     = 20,
  ODDS0   = 1/50
)

## -----------------------------------------------------------------------------
custom_skim <- skim_with(
  numeric = sfl(
    median = purrr::partial(median, na.rm = TRUE),
    skewness = purrr::partial(moments::skewness, na.rm = TRUE),
    kurtosis = purrr::partial(moments::kurtosis, na.rm = TRUE),
    count_distr_max = risk3r::count_distr_max,
    n_unique = skimr::n_unique
    ),
  factor = sfl(
    hhi = risk3r::hhi,
    hhi_lbl = purrr::compose(risk3r::hhi, risk3r::hhi_label, .dir = "forward"),
    count_distr_max = risk3r::count_distr_max
    ),
  append = TRUE
  )

# custom_skim(data %>% select(1:10))


## -----------------------------------------------------------------------------
data <- readRDS(here::here("data-raw/cd.rds"))

data <- data %>% 
  select(1:100, where(is.character)) %>% 
  sample_n(100000)

data %>% 
  select(1:10) %>% 
  glimpse()

data <- data %>% 
  mutate(
    bad_good = ifelse(bad_good == 1, "bad", "good"),
    bad_good = factor(bad_good, levels = c("bad", "good"))
    ) %>% 
  mutate(across(where(is.character), as.factor))

dim(data)

## -----------------------------------------------------------------------------
describe <- custom_skim(data)

# use as.list to export skimr (risk3r:::as.list.skim_df)
# writexl::write_xlsx(as.list(describe), here::here("data-raw/describe.xlsx"))

## -----------------------------------------------------------------------------
var_to_remove_near_0_var <- skimr::partition(describe) %>% 
  purrr::map_df(select, variable = skim_variable, n_unique, count_distr_max) %>% 
  tibble::as_tibble() %>% 
  filter(n_unique == 1 | count_distr_max >= PARAMETERS$P_CONCENTRATION)

var_to_remove_near_0_var %>% 
  arrange(variable)

## -----------------------------------------------------------------------------
scrd_filter <- scorecard::var_filter(
  data, 
  # next 2 parameter are needed even we want only due identical_limit
  y = "bad_good", iv_limit = 0,
  missing_limit = 1,
  identical_limit =  PARAMETERS$P_CONCENTRATION,
  return_rm_reason = TRUE
  )

scrd_filter$rm %>% 
  as_tibble() %>% 
  filter(!is.na(rm_reason)) %>% 
  arrange(variable)

## -----------------------------------------------------------------------------
data <- data %>% 
  select(-all_of(var_to_remove_near_0_var$variable))

## -----------------------------------------------------------------------------
data <- data %>% 
  select(-time, -id)            

## -----------------------------------------------------------------------------
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(PARAMETERS$SEED)

data_split <- rsample::initial_split(data, prop = PARAMETERS$P_TRAIN)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

## -----------------------------------------------------------------------------
train_data %>% count(bad_good) %>% mutate(n/sum(n))
test_data %>% count(bad_good) %>% mutate(n/sum(n))

## ---- message=FALSE-----------------------------------------------------------
binssums <- purrr::map_df(c("tree", "chimerge"), function(m = "chimerge"){
  
  message(m)
  
  bins <- risk3r::woebin2(
    train_data, 
    y = "bad_good", 
    count_distr_limit = PARAMETERS$COUNT_DISTR_LIMIT,
    method = m
    )
  
   woebin_summary(bins) %>% 
     mutate(method = m, .before = 1)
  
})


x <- train_data$mra_dac_rec_30_03
y <- train_data$bad_good


## -----------------------------------------------------------------------------
binssums %>%
  count(method)

binssums %>%
  count(variable, iv) %>% 
  count(n)


binssums %>% 
  filter(variable == max(variable)) %>% 
  select(1:4)

## -----------------------------------------------------------------------------
# bins2 <- woebin2(dtrain, y = "bad", method = "ctree")

## -----------------------------------------------------------------------------
describe <- custom_skim(data)

# use as.list to export skimr (risk3r:::as.list.skim_df)
# writexl::write_xlsx(as.list(describe), here::here("data-raw/describe.xlsx"))

## -----------------------------------------------------------------------------
var_to_remove_near_0_var <- skimr::partition(describe) %>% 
  purrr::map_df(select, variable = skim_variable, n_unique, count_distr_max) %>% 
  tibble::as_tibble() %>% 
  filter(n_unique == 1 | count_distr_max >= PARAMETERS$P_CONCENTRATION)

var_to_remove_near_0_var %>% 
  arrange(variable)

## -----------------------------------------------------------------------------
bins <- scorecard::woebin(
  train_data, 
  y = "bad_good", 
  count_distr_limit = PARAMETERS$COUNT_DISTR_LIMIT
  )

train_data$monte_advance_mean_01 %>% is.na() %>% table()
bins$monte_advance_mean_01

## -----------------------------------------------------------------------------
scrd_filter <- scorecard::var_filter(
  data, 
  # next 2 parameter are needed even we want only due identical_limit
  y = "bad_good", iv_limit = 0,
  missing_limit = 1,
  identical_limit =  PARAMETERS$P_CONCENTRATION,
  return_rm_reason = TRUE
  )

scrd_filter$rm %>% 
  as_tibble() %>% 
  filter(!is.na(rm_reason)) %>% 
  arrange(variable)

## -----------------------------------------------------------------------------
data <- data %>% 
  select(-all_of(var_to_remove_near_0_var$variable))

## -----------------------------------------------------------------------------
data <- data %>% 
  select(-time, -id)            

## -----------------------------------------------------------------------------
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(PARAMETERS$SEED)

data_split <- rsample::initial_split(data, prop = PARAMETERS$P_TRAIN)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

## -----------------------------------------------------------------------------
train_data %>% count(bad_good) %>% mutate(n/sum(n))
test_data %>% count(bad_good) %>% mutate(n/sum(n))

## -----------------------------------------------------------------------------
dbiv <- woebin_summary(bins)

dbiv

glimpse(dbiv)

## -----------------------------------------------------------------------------
dbiv %>% 
  filter(
    iv > 0.02,  # A >= weak iv value
    !factor,    # is numeric
    !monotone   # is not monotonic
    )

## -----------------------------------------------------------------------------
bins <- scorecard::woebin(
  train_data, 
  y = "bad_good", 
  count_distr_limit = PARAMETERS$COUNT_DISTR_LIMIT
  )

train_data$monte_advance_mean_01 %>% is.na() %>% table()
bins$monte_advance_mean_01

## -----------------------------------------------------------------------------
dbiv <- woebin_summary(bins)

dbiv

glimpse(dbiv)

## -----------------------------------------------------------------------------
dbiv %>% 
  filter(
    iv > 0.02,  # A >= weak iv value
    !factor,    # is numeric
    !monotone   # is not monotonic
    )

## -----------------------------------------------------------------------------
scrd_filter <- scorecard::var_filter(
  data, 
  # next 2 parameter are needed even we want only due identical_limit
  y = "bad_good", iv_limit = 0,
  missing_limit = 1,
  identical_limit =  PARAMETERS$P_CONCENTRATION,
  return_rm_reason = TRUE
  )

scrd_filter$rm %>% 
  as_tibble() %>% 
  filter(!is.na(rm_reason)) %>% 
  arrange(variable)

## -----------------------------------------------------------------------------
data <- data %>% 
  select(-all_of(var_to_remove_near_0_var$variable))

## -----------------------------------------------------------------------------
data <- data %>% 
  select(-time, -id)            

## -----------------------------------------------------------------------------
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(PARAMETERS$SEED)

data_split <- rsample::initial_split(data, prop = PARAMETERS$P_TRAIN)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

## -----------------------------------------------------------------------------
train_data %>% count(bad_good) %>% mutate(n/sum(n))
test_data %>% count(bad_good) %>% mutate(n/sum(n))

