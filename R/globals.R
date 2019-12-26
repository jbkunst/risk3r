# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887/3
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "bin",
      "count_distr",
      "is_special_values",
      "iv",
      "total_iv",
      "variable"
    )
  )
}


