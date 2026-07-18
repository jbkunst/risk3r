library(tidyverse)


# credit ------------------------------------------------------------------
credit <- readr::read_csv("https://raw.githubusercontent.com/jbkunst/riskr/master/data/credit.csv")

glimpse(credit)

usethis::use_data(credit, overwrite = TRUE)

# credit woe --------------------------------------------------------------
woes <- scorecard::woebin(credit, "bad")

credit_woe <- scorecard::woebin_ply(credit, woes)

credit_woe <- dplyr::select_if(credit_woe, is.numeric)

credit_woe <- tibble::as_tibble(credit_woe)



usethis::use_data(woes, overwrite = TRUE)

usethis::use_data(credit_woe, overwrite = TRUE)


# hmeq --------------------------------------------------------------------
hmeq <- readr::read_csv("http://www.creditriskanalytics.net/uploads/1/9/5/1/19511601/hmeq.csv")

hmeq <- rename_with(hmeq, str_to_lower)

glimpse(hmeq)

usethis::use_data(hmeq, overwrite = TRUE)


# home_credit_application -------------------------------------------------
# Official source: Home Credit Default Risk competition on Kaggle.
# This third-party Hugging Face mirror is used only for convenient download.
home_credit_application <- readr::read_csv(
  "https://huggingface.co/cantalapiedra/poc_scoring_fair/resolve/main/application_train.csv?download=true",
  show_col_types = FALSE
) |>
  dplyr::rename_with(stringr::str_to_lower)

stopifnot(identical(dim(home_credit_application), c(307511L, 122L)))

glimpse(home_credit_application)

usethis::use_data(home_credit_application, overwrite = TRUE)
