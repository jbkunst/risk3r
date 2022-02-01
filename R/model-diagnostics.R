#' Calculate predictive metrics for glm models
#'
#' @param model model
#' @param newdata Optional data frame
#' @examples
#'
#' N <- 10000
#' predicted <- runif(N)
#' actual <- rbinom(N, size = 1, prob = predicted)
#'
#' daux <- data.frame(actual = actual, predicted = predicted)
#' m <- glm(actual ~ predicted, family = binomial, data = daux)
#'
#' model_metrics(m)
#' model_metrics(m, newdata = head(daux, 100))
#'
#' @importFrom stats predict.glm predict
#' @export
model_metrics <- function(model, newdata = NULL) {

  r_n_p <- reponse_and_predictors_names(model)

  yvar <- r_n_p[["response"]]

  if (is.null(newdata)) {
    mm <- metrics(model$data[[yvar]], model$fitted.values)
  } else {
    stopifnot(is.data.frame(newdata))

    mm <- metrics(
      dplyr::pull(newdata, yvar),
      predict(model, newdata = newdata)
    )
  }

  mm
}

#' Get predictive indicator for partial models given a model
#'
#' @param model model
#' @param newdata Optional data frame
#' @param verbose verbose
#' @examples
#'
#' data("credit_woe")
#'
#' m <- glm(bad ~ ., family = binomial, data = head(credit_woe, 10000))
#' m <- featsel_stepforward(m)
#'
#' model_partials(m)
#'
#' model_partials(m, newdata = tail(credit_woe, 10000))
#'
#' @importFrom stats binomial glm
#' @export
model_partials <- function(model, newdata = NULL, verbose = TRUE) {
  r_n_p <- reponse_and_predictors_names(model)

  yvar <- r_n_p[["response"]]
  xvars <- r_n_p[["predictors"]]

  dfmetrics <- purrr::map_df(1:length(xvars), function(nv = 7) {
    var <- xvars[nv]
    new_xvars <- xvars[1:nv]

    new_f <- formula_from_reponse_and_predictors_names(yvar, new_xvars)

    if (verbose) {
      msg <- stringr::str_glue("Fitting and evaluating model with { nv } variables: { new_f }")

      msg <- stringr::str_trunc(msg, getOption("width"))

      message(msg)
    }

    fit <- glm(new_f, family = binomial(), data = model$data)

    out <- model_metrics(fit)

    if (!is.null(newdata)) {
      out2 <- model_metrics(fit, newdata = newdata)
      out2 <- dplyr::mutate(out2, sample = "test", .before = 1)

      out <- dplyr::mutate(out, sample = "train", .before = 1)
      out <- dplyr::bind_rows(out, out2)
    }

    out <- dplyr::mutate(out, variable = var, .before = 1)
  })

  if (!is.null(newdata)) {
    dfmetrics <- dplyr::mutate(
      dfmetrics,
      sample = forcats::fct_inorder(.data$sample)
    )
  }

  dfmetrics <- dplyr::mutate(
    dfmetrics,
    variable = forcats::fct_inorder(.data$variable)
  )

  # class(dfmetrics) <- c("model_partials", class(dfmetrics))

  dfmetrics
}


#' Get summary of model
#'
#' @param model model
#' @examples
#'
#' data("credit_woe")
#'
#' m <- glm(bad ~ ., family = binomial, data = head(credit_woe, 10000))
#' m <- featsel_stepforward(m)
#'
#' model_partials(m)
#'
#' model_summary_variables(m)
#'
#' model_corr_variables(m)
#'
#' model_vif_variables(m)
#'
#' model_iv_variables(m)
#'
#' @param coef_sign Sign to compare estimaes.
#' @param limit_significance Limit for Significance.
#' @param limit_iv Limit for Information Value.
#' @param limit_corr Limit for correlation max between variables.
#' @param limit_vif Limit for VIF.
#' @export
model_summary_variables <- function(model,
                                    coef_sign  = 1,
                                    limit_significance = 0.05,
                                    limit_iv           = 0.02,
                                    limit_corr         = 0.60,
                                    limit_vif          = 5.00){

  dmod <- broom::tidy(model)

  dcor <- model_corr_variables(model)
  dcor <- dcor %>%
    dplyr::group_by(.data$term) %>%
    dplyr::summarise(correlation_max = max(.data$cor, na.rm = TRUE))

  dvif <- model_vif_variables(model)

  div <- model_iv_variables(model)

  dmodtot <- list(dmod, dcor, div, dvif) %>%
    purrr::reduce(full_join, by = "term")

  dmodtot <- dmodtot %>%
    mutate(
      dummy_significance = .data$p.value < limit_significance,
      dummy_sign         = sign(.data$estimate) == sign(coef_sign),
      dummy_iv           = .data$iv > limit_iv,
      dummy_correlation  = .data$correlation_max < limit_corr,
      dummy_vif          = .data$vif < limit_vif,

      dummy_sign = ifelse(.data$term == "(Intercept)", NA, .data$dummy_sign)

    )

  dmodtot

}

#' @rdname model_summary_variables
#' @export
model_corr_variables <- function(model){

  varsnms <- reponse_and_predictors_names(model)$predictors

  term_lvls <- broom::tidy(model) %>%
    dplyr::filter(.data$term != "(Intercept)")  %>%
    dplyr::pull(.data$term)

  dcor <- model$data %>%
    dplyr::select(dplyr::all_of(varsnms)) %>%
    corrr::correlate() %>%
    tidyr::gather("term2", "cor", -.data$term) %>%
    dplyr::mutate(
      term = factor(.data$term, levels = term_lvls),
      term2 = factor(.data$term2, levels = term_lvls),
    )

  dcor

}

#' @rdname model_summary_variables
#' @export
model_vif_variables <- function(model){

  term_lvls <- broom::tidy(model) %>%
    dplyr::filter(.data$term != "(Intercept)")  %>%
    dplyr::pull(.data$term)

  # car::vif(model)
  # performance::check_collinearity(model)
  # plot(performance::check_collinearity(model))
  # car::vif(model) %>% as.data.frame()
  # performance::check_collinearity(model)
  # scorecard::vif(model)
  # performance::check_collinearity(model) %>% class()
  # performance:::plot.check_collinearity
  # see:::plot.see_check_collinearity
  # see:::.plot_diag_vif

  vif_brks <- c(-Inf, 5, 10, Inf)
  vif_lbls <- c("low (<5)", "moderate (<10)","high (>= 10)")

  dvif <- scorecard::vif(model) %>%
    # vif(model) %>%
    as.data.frame() %>%
    # tibble::rownames_to_column("term") %>%
    dplyr::as_tibble() %>%
    purrr::set_names(c("term", "vif")) %>%
    dplyr::mutate(
      term = factor(.data$term, levels = term_lvls),
      vif_label = vif_label(.data$vif)
    )

  dvif

}

#' @rdname model_summary_variables
#' @export
model_iv_variables <- function(model){

  response_var <- reponse_and_predictors_names(model)$response

  term_lvls <- broom::tidy(model) %>%
    dplyr::filter(.data$term != "(Intercept)")  %>%
    dplyr::pull(.data$term)

  div <- purrr::map_df(term_lvls, function(variable){

    df <- as_tibble(
      data.frame(
        y = model$data[[response_var]],
        x = as.character(model$data[[variable]])
        )
      )

    bin <- quiet(scorecard::woebin(df, "y", "x", breaks_list = list(x = unique(df$x))))

    out <- bin[["x"]][["total_iv"]][[1]]

    tibble(term = variable, iv = out)

  })

  div <- div %>%
    mutate(iv_label = iv_label(.data$iv))

  div

}


