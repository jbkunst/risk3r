#' Get response and predictors names from model
#' @param model model
#' @export
reponse_and_predictors_names <- function(model) {
  response <- as.character(as.formula(model))[2]

  predictors <- as.character(as.formula(model))[3]
  predictors <- unlist(strsplit(predictors, "\\s+\\+\\s+"))

  list(
    response = response,
    predictors = predictors
  )
}


#' Get formula from reponse and predictors names
#' @param response response
#' @param predictors predictors
#' @export
formula_from_reponse_and_predictors_names <- function(response, predictors) {
  stopifnot(
    is.character(response),
    length(response) == 1,
    is.character(predictors)
  )

  f <- paste0(predictors, collapse = " + ")
  f <- paste0(response, " ~ ", f)

  as.formula(f)
}
