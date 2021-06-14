#' Calculate predictive metrics for glm models
#'
#' @param model model
#' @param newdata Optional data frame
#' @examples
#'
#' N <- 10000
#'
#' predicted <- runif(N)
#'
#' actual <- rbinom(N, size = 1, prob = predicted)
#'
#' daux <- data.frame(actual = actual, predicted = predicted)
#'
#' m <- glm(actual ~ predicted, family = binomial, data = daux)
#'
#' model_metrics(m)
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
#' @param verbose verbose#'
#' @examples
#' TRUE
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

  class(dfmetrics) <- c("model_partials", class(dfmetrics))

  dfmetrics
}


#' Plot model_partials output
#'
#' @param x Result from model_partials function
#' @param ... Optional arguments for ggplot2::geom_line
#'
#' @examples
#' TRUE
#' @method plot model_partials
#' @importFrom utils hasName
#' @export
plot.model_partials <- function(x, ...) {

  # stopifnot(attr(dfmetrics, "function") == "model_partials")

  if (hasName(x, "sample")) {
    dfg <- tidyr::gather(x, "key", "value", -.data$variable, -.data$sample)
    mapng <- ggplot2::aes(.data$variable, .data$value, group = .data$sample, color = .data$sample)
  } else {
    dfg <- tidyr::gather(x, "key", "value", -.data$variable)
    mapng <- ggplot2::aes(.data$variable, .data$value, group = .data$key)
  }

  ggplot2::ggplot(dfg) +
    ggplot2::geom_line(mapping = mapng, ...) +
    ggplot2::facet_wrap(ggplot2::vars(.data$key), ncol = 1, scales = "free_y")
}
