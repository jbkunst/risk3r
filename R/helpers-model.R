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

#' A broom::tidy for glm modelos with terms and confidence intervals
#' @param model model.
#' @param level the confidence level required.
#' @param show_intercept A logical value to indicate to show or hide the `(Intercept)`.
#' @export
model_terms_and_ci <- function(model, level = 0.95, show_intercept = FALSE){

  dmod <- broom::tidy(model)

  dconf <- stats::confint.default(model, level = level) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    tibble::as_tibble() %>%
    purrr::set_names(c("term", "lower", "upper"))

  dconf <- dplyr::left_join(dmod, dconf, by = "term")

  dconf

  if(!show_intercept){
    dconf <- dconf %>%
      dplyr::filter(.data$term != "(Intercept)")
  }

  dconf

}
