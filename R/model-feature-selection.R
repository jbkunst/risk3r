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
#'
#'
#' @importFrom glmnet cv.glmnet
#' @importFrom stringr str_remove_all
#' @importFrom stats coef
#' @export
featsel_glmnet <- function(model, S = "lambda.1se", plot = TRUE, seed = 123,
                           trace.it = 1, type.measue = "auc", ...) {

  yvar  <- as.character(as.formula(model))[2]
  xvars <- as.character(as.formula(model))[3]
  xvars <- unlist(strsplit(xvars, "\\s+\\+\\s+"))

  # fit to get order
  fit <- glmnet::glmnet(
    x = as.matrix(as.data.frame(model$data)[,xvars]),
    y = model$data[[yvar]],
    family = "binomial"
  )

  fit

  dm <- as.matrix(fit$beta)
  dm <- as.data.frame(dm)
  dm <- tibble::rownames_to_column(dm, "variable")
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
    x = as.matrix(as.data.frame(model$data)[,xvars]),
    y = model$data[[yvar]],
    family = "binomial",
    trace.it = trace.it,
    type.measure = type.measue,
    ...
  )

  cvfit

  if(plot) {

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

#' Shortcut for step forward
#'
#' @param model model
#' @param ... Additional argumentos for stats::step
#'
#' @examples
#'
#' @importFrom stats step
#' @export
featsel_stepforward <- function(model, ...) {

  yvar  <- as.character(as.formula(model))[2]

  model_null <- glm(
    as.formula(paste0(yvar, " ~ 1")),
    data = model$data,
    family = binomial(link = logit)
  )

  model_full <- as.formula(model)

  model_step <- step(
    model_null,
    scope = list(upper = model_full, lower = ~1),
    direction = "both",
    ...
  )

  model_step

}
