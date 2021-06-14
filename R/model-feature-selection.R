#' Feature selection via glmnet
#'
#' @param model model
#' @param S S
#' @param plot plot
#' @param seed seed
#' @param trace.it trace.it
#' @param type.measue type.measue
#' @param ... Additional argumentos for glmnet::cv.glmnet
#'
#' @examples
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
  dm <- tibble::as.tibble(dm)
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
  dm <- dplyr::select(dm, .data$variable, .data$position)

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
  dcoefs <- tibble::as.tibble(dcoefs)
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

#' Feature selection via iterative
#'
#' @param model model
#' @param ... Additional argumentos for stats::step
#'
#' @examples
#' @importFrom stats step
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
#' @param ... Additional argumentos for stats::step
#'
#' @examples
#' @importFrom stats step
#' @export
featsel_loss_function_permutations <- function(model,
                                               stat = c("median", "mean", "min", "q25", "q75", "max"),
                                               B = 10,
                                               n_sample = NULL,
                                               loss_function = DALEX::loss_one_minus_auc,
                                               verbose = TRUE,
                                               plot = TRUE,
                                               ...) {
  stat <- stat[[1]]

  plots <- list()

  new_model <- model

  r_n_p <- reponse_and_predictors_names(model)

  yvar <- r_n_p[["response"]]
  new_xvars <- r_n_p[["predictors"]]

  iteration <- 1
  keep_going <- TRUE

  while (keep_going) {
    explainer <- DALEX::explain(
      new_model,
      data = model$data[, new_xvars],
      y = as.numeric(model$data[[yvar]]),
      verbose = verbose,
      label = stringr::str_glue("Round { iteration }"),
      ...
    )

    if (verbose) message(stringr::str_glue("Round { t }", t = iteration))

    feat_imp <- ingredients::feature_importance(
      explainer,
      loss_function = loss_function,
      B = B,
      n_sample = n_sample,
      variables = new_xvars,
      ...
    )

    if (plot) {
      plot(feat_imp)
      plots <- append(plots, list(plot(feat_imp)))
    }

    dsum <- summary_feature_importance_ingredients(feat_imp)

    dsum <- dsum[order(dsum[[paste0("dl_", stat)]], decreasing = TRUE), ]

    # order by metric/stat
    new_xvars <- as.character(dsum[["variable"]])

    # filter
    dsum <- dsum[dsum[[paste0("dl_", stat)]] < attr(dsum, "dropout_loss_full_model_mean"), ]

    xvars_rm <- as.character(dsum[["variable"]])

    if (length(xvars_rm) == 0) {
      if (verbose) message("No more variables removed.")
      break
    }

    if (verbose) {
      message("Removed variables (", length(xvars_rm), ") are: ", paste0(xvars_rm, sep = ", "))
    }

    new_xvars <- setdiff(new_xvars, xvars_rm)

    new_f <- formula_from_reponse_and_predictors_names(yvar, new_xvars)
    new_model <- glm(new_f, data = model$data, family = binomial(link = logit))

    iteration <- iteration + 1
  }

  if (plot) attr(new_model, "plots") <- plots

  new_model
}

summary_feature_importance_ingredients <- function(res) {
  # res <- feat_imp

  res <- tibble::as_tibble(as.data.frame(res))

  dl_full_mean <- dplyr::filter(
    res,
    .data$variable == "_full_model_",
    .data$permutation == 0
  )[["dropout_loss"]]

  # check if B == 1
  if (!nrow(dplyr::filter(res, .data$permutation != 0)) == 0) {
    res <- dplyr::filter(res, .data$permutation != 0)
  }

  res <- dplyr::group_by(res, .data$variable)
  res <- dplyr::summarise(
    res,
    B         = dplyr::n(),
    dl_min    = min(.data$dropout_loss),
    dl_q25    = quantile(.data$dropout_loss, probs = 0.25),
    dl_median = quantile(.data$dropout_loss, probs = 0.50),
    dl_mean   = mean(.data$dropout_loss),
    dl_75     = quantile(.data$dropout_loss, probs = 0.75),
    dl_max    = max(.data$dropout_loss),
  )

  # dplyr::filter(res, .data$variable %in% c("_full_model_", "_baseline_")) %>%
  #   filter(dl_mean == dl_full)

  res <- dplyr::filter(res, !.data$variable %in% c("_full_model_", "_baseline_"))

  attr(res, "dropout_loss_full_model_mean") <- dl_full_mean

  res
}
