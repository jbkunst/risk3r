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
