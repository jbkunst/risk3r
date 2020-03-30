# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887/3
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "bin",
      "count_distr",
      "is_special_values",
      "iv",
      "total_iv",
      "variable",
      #
      "bad",
      "badprob",
      "bins",
      "good",
      "label",
      "value",
      "total_badprob",
      "type2",
      "type",
      "count",
      "value_label",
      #
      "variable_1",
      "variable_2",
      "cor",
      #
      "node"
    )
  )
}


