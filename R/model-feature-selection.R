#' Feature selection via glmnet
#'
#' This is a wrapper for `glmnet::cv.glmnet` and return A `glm` object using
#' the selected variables from `glmnet::cv.glmnet`.
#'
#' @return A `glm` object using the selected variables from `glmnet::cv.glmnet`.#'
#'
#' @param model model
#' @param S From https://glmnet.stanford.edu/articles/glmnet.html#quick-start: `lambda.min` is
#'   the value of λ that gives minimum mean cross-validated error, while `lambda.1se` is the
#'   value of λ that gives the most regularized model such that the cross-validated error
#'   is within one standard error of the minimum.
#' @param plot plot
#' @param seed seed
#' @param trace.it trace.it p
#' @param type.measue type.measue
#' @param ... Additional argumentos for glmnet::cv.glmnet
#'
#' @examples
#'
#' data("credit_woe")
#'
#' m <- glm(bad ~ ., family = binomial, data = credit_woe)
#'
#' featsel_glmnet(m)
#'
#' @importFrom glmnet cv.glmnet
#' @importFrom stringr str_remove_all
#' @importFrom stats coef
#' @export
featsel_glmnet <- function(model, S = "lambda.1se", plot = TRUE, seed = 123,
                           trace.it = 1, type.measue = "auc", ...) {

  r_n_p <- reponse_and_predictors_names(model)

  yvar <- r_n_p[["response"]]
  xvars <- r_n_p[["predictors"]]

  # fit to get order
  fit <- glmnet::glmnet(
    x = as.matrix(as.data.frame(model$data)[, xvars]),
    y = model$data[[yvar]],
    family = model$family$family
  )

  fit

  dm <- as.matrix(fit$beta)
  dm <- as.data.frame(dm)
  dm <- tibble::rownames_to_column(dm, "variable")
  dm <- tibble::as_tibble(dm)
  dm <- tidyr::gather(dm, "L1_Norm", "coefficient", -.data$variable)
  dm <- dplyr::mutate(
    dm,
    L1_Norm = stringr::str_remove_all(.data$L1_Norm, "s"),
    L1_Norm = as.numeric(.data$L1_Norm)
  )
  dm <- dplyr::filter(dm, .data$coefficient != 0)
  dm <- dplyr::group_by(dm, .data$variable)
  dm <- dplyr::arrange(dm, .data$L1_Norm)
  dm <- dplyr::filter(dm, dplyr::row_number() == 1)
  dm <- dplyr::ungroup(dm)
  dm <- dplyr::mutate(dm, position = dplyr::row_number())
  dm <- dplyr::select(dm, "variable", "position")

  # cv fit
  set.seed(seed)

  cvfit <- glmnet::cv.glmnet(
    x = as.matrix(as.data.frame(model$data)[, xvars]),
    y = model$data[[yvar]],
    family = model$family$family,
    trace.it = trace.it,
    type.measure = type.measue,
    ...
  )

  cvfit

  if (plot) {
    graphics::plot(fit, xvar = "norm")
    graphics::plot(fit, xvar = "lambda")
    graphics::plot(cvfit, sign.lambda = 1)
  }

  # cvfit$lambda.min
  # log(cvfit$lambda.min)
  #
  # cvfit$lambda.1se
  # log(cvfit$lambda.1se)
  #
  # glmnet:::coef.cv.glmnet(cvfit, s = "lambda.min")
  # glmnet:::coef.cv.glmnet(cvfit, s = "lambda.1se")

  dcoefs <- stats::coef(cvfit, s = S)
  dcoefs <- as.matrix(dcoefs)

  colnames(dcoefs) <- "coefficient"

  dcoefs <- as.data.frame(dcoefs)
  dcoefs <- tibble::rownames_to_column(dcoefs, var = "variable")
  dcoefs <- tibble::as_tibble(dcoefs)
  dcoefs <- dplyr::left_join(dcoefs, dm, by = "variable")
  dcoefs <- dplyr::mutate(dcoefs, position = ifelse(is.na(.data$position), Inf, .data$position))
  dcoefs <- dplyr::filter(dcoefs, .data$coefficient != 0, .data$variable != "(Intercept)")
  dcoefs <- dplyr::arrange(dcoefs, .data$position)

  xnewvars <- dplyr::pull(dcoefs, .data$variable)
  xnewvars <- paste0(xnewvars, collapse = " + ")

  formula <- paste0(yvar, " ~ ", xnewvars)

  model_fs_glmnet <- glm(
    as.formula(formula),
    data = model$data,
    family = binomial(link = logit)
  )

  model_fs_glmnet

}

#' Feature selection vis stepwise forward
#'
#' @param model model
#' @param ... Additional arguments for stats::step
#' @importFrom stats step
#'
#' @examples
#'
#' data("credit_woe")
#'
#' m <- glm(bad ~ ., family = binomial, data = credit_woe)
#'
#' featsel_stepforward(m)
#'
#' @export
featsel_stepforward <- function(model, ...) {

  r_n_p <- reponse_and_predictors_names(model)

  yvar <- r_n_p[["response"]]

  model_null <- glm(
    as.formula(paste0(yvar, " ~ 1")),
    data = model$data,
    family = binomial(link = logit)
  )

  model_full <- as.formula(model)

  model_step <- step(
    model_null,
    scope = list(upper = model_full, lower = as.formula(model_null)),
    direction = "both",
    ...
  )

  model_step

}

#' Shortcut featsel_loss_function_permutations
#'
#' @param model model
#' @param stat = c("median", "mean", "min", "q25", "q75", "max"),
#' @param iterations Default 100.
#' @param ... Additional arguments for celavi::feature_selection
#'
#' @examples
#'
#' data("credit_woe")
#'
#' m <- glm(bad ~ ., family = binomial, data = credit_woe)
#'
#' m_featsel <- featsel_loss_function_permutations(m, stat = min, iterations = 10)
#'
#' @export
#' @importFrom celavi feature_selection
featsel_loss_function_permutations <- function(
  model,
  stat = function(x) quantile(x, 0.25),
  iterations = 100,
  ...) {

  fs <- celavi::feature_selection(
    glm,
    model$data,
    response = risk3r::reponse_and_predictors_names(model)[["response"]],
    stat = stat,
    iterations = iterations,
    predict_function = predict.glm,
    # function accepts specific argument for the fit function!
    family  = binomial,
    ...
  )

  # plot(fs)

  model_lss_prmt <- attr(fs, "final_fit")

  model_lss_prmt

}


#' An iterative process to eliminate variables by confidence interval or sign term
#'
#' @param model model
#' @param level the confidence level used to calculate the confidence interval
#' @param verbose Default set to TRUE.
#'
#' @examples
#'
#' data("credit_woe")
#'
#' m <- glm(bad ~ ., family = binomial, data = credit_woe)
#'
#' m_featsel <- featsel_brute_force(m)
#'
#' @export
#' @importFrom celavi feature_selection
featsel_brute_force <- function(model, level = 0.95, verbose = TRUE) {

  while (TRUE) {

    # print(gg_model_coef(model))

    dconf <- model_terms_and_ci(model, level = level)
    dconf <- dplyr::arrange(dconf, .data$estimate)

    vars_sign_problems <- dplyr::filter(dconf, .data$estimate < 0)
    vars_sign_problems <- dplyr::pull(vars_sign_problems, .data$term)

    vars_conf_problems <- dplyr::filter(dconf, .data$lower < 0 & .data$upper > 0)
    vars_conf_problems <- dplyr::pull(vars_conf_problems, .data$term)

    vars_with_problems <- unique(c(vars_conf_problems, vars_sign_problems))

    if(length(vars_with_problems) == 0){

      break

    }

    var_to_rm <- vars_with_problems[1]

    cli::cli_alert_info("removing `{var_to_rm}`.")

    r_n_p <- reponse_and_predictors_names(model)

    r_n_p$predictors <- setdiff(r_n_p$predictors, var_to_rm)

    f <- formula_from_reponse_and_predictors_names(
      r_n_p$response,
      r_n_p$predictors
    )

    model <- glm(
      f,
      family = binomial,
      data = dplyr::select(model$data, as.character(unlist(r_n_p)))
      )

  }

  model

}
