#' Calculate predictive metrics for glm models
#'
#' @examples
#'
#' library(scorecard)
#' data("germancredit")
#'
#' dt    <-  var_filter(germancredit, "creditability")
#' bins  <-  woebin(dt, "creditability")
#' dtwoe <-  woebin_ply(dt, bins)
#' model <- glm(creditability ~ ., family = binomial(), data = dtwoe)
#'
#' model_metrics(model)
#'
#' model_metrics(model, newdata = head(dtwoe, 100))
#'
#' @importFrom stats predict.glm
#' @export
model_metrics <- function(model, newdata = NULL) {

  yvar  <- as.character(as.formula(model))[2]
  # newdata <- dplyr::sample_frac(dt_woe_list[[2]], 0.3)

  if(is.null(newdata)) {

    mm <- metrics(model$data[[yvar]], model$fitted.values)

  } else {

    stopifnot(is.data.frame(newdata))

    mm <- metrics(
      dplyr::pull(newdata, yvar),
      predict.glm(model, newdata = newdata)
    )

  }

  mm

}

#'
#' @title Get predictive indicator for partial models given a model
#'
#' @examples
#'
#' library(scorecard)
#' data("germancredit")
#'
#' dt    <-  var_filter(germancredit, "creditability")
#' bins  <-  woebin(dt, "creditability")
#' dtwoe <-  woebin_ply(dt, bins)
#' model <- glm(creditability ~ ., family = binomial(), data = dtwoe)
#'
#' model_partials(model)
#'
#' @importFrom stats binomial glm
#' @export
model_partials <- function(model, newdata = NULL) {

  yvar  <- as.character(as.formula(model))[2]
  xvars <- as.character(as.formula(model))[3]
  xvars <- unlist(strsplit(xvars, "\\s+\\+\\s+"))

  dfmetrics <- purrr::map_df(1:length(xvars), function(nv = 3){

    var <- xvars[nv]

    f <- paste0(xvars[1:nv], collapse = " + ")
    f <- paste0(yvar, " ~ ", f)
    f

    fit <- glm(as.formula(f), family = binomial(), data = model$data)

    out <-  model_metrics(fit)

    if(!is.null(newdata)) {

      out2 <- model_metrics(fit, newdata = newdata)
      out2 <- dplyr::mutate(out2, sample = "test", .before = 1)

      out <- dplyr::mutate(out, sample = "train", .before = 1)
      out <- dplyr::bind_rows(out, out2)

    }

    out <- dplyr::mutate(out, variable = var, .before = 1)

  })

  if(!is.null(newdata)) {
    dfmetrics <- dplyr::mutate(
      dfmetrics,
      sample = forcats::fct_inorder(.data$sample)
    )
  }

  dfmetrics <- dplyr::mutate(
    dfmetrics,
    variable = forcats::fct_inorder(.data$variable)
  )

  # attr(dfmetrics, "function") <- "model_partials"

  class(dfmetrics) <- c("model_partials", class(dfmetrics))

  dfmetrics

}

#' Plot model_partials output
#'
#' @examples
#'
#' library(scorecard)
#' data("germancredit")
#'
#' dt    <-  var_filter(germancredit, "creditability")
#' bins  <-  woebin(dt, "creditability")
#' dtwoe <-  woebin_ply(dt, bins)
#' model <- glm(creditability ~ ., family = binomial(), data = dtwoe)
#'
#' dfmetrics <- model_partials(model)
#'
#' plot(dfmetrics)
#'
#' library(ggplot2)
#'
#' plot(dfmetrics)
#'
#' plot(dfmetrics[,-c(4, 5)]) +
#'   geom_text(
#'   aes(variable, value + 0.01, label = scales::percent(value, accuracy = 0.1)),
#'    size = 3
#'    )
#'
#'
#' @importFrom utils hasName
#' @export
plot.model_partials <- function(dfmetrics, ...) {

  # stopifnot(attr(dfmetrics, "function") == "model_partials")

  if(hasName(dfmetrics, "sample")) {

    dfg <- tidyr::gather(dfmetrics, "key", "value", -.data$variable, -.data$sample)
    mapng <- ggplot2::aes(.data$variable, .data$value, group = .data$sample, color = .data$sample)

  } else {

    dfg <- tidyr::gather(dfmetrics, "key", "value", -.data$variable)
    mapng <- ggplot2::aes(.data$variable, .data$value, group = .data$key)

  }

  ggplot2::ggplot(dfg) +
    ggplot2::geom_line(mapping = mapng, ...) +
    ggplot2::facet_wrap(ggplot2::vars(.data$key), ncol = 1, scales = "free_y")

}


#' #' Function to stem model given aux gains
#' #'
#' #' @param model The model to test
#' #' @param trace Show the trace
#' #'
#' #' @export
#' step_auc <- function(model, trace = TRUE){
#'
#'   yvar  <- as.character(as.formula(model))[2]
#'   xvars <- as.character(as.formula(model))[3]
#'   xvars <- unlist(strsplit(xvars, "\\s+\\+\\s+"))
#'
#'   vars_act  <- c(1)
#'   vars_add <- xvars
#'
#'   for(s in 1:length(xvars)) {
#'
#'     mod_act <- paste(vars_act, collapse = "+")
#'     mod_act <- paste(yvar, "~", mod_act)
#'
#'     fit <- glm(as.formula(mod_act), family = binomial(), data = model$data)
#'
#'     if(length(vars_act) == 1) {
#'       actual_auc <- 0.5
#'     } else {
#'       actual_auc <- Metrics::auc(model$data[[yvar]], fit$fitted.values)
#'     }
#'
#'     if(length(vars_act) == 0) break()
#'
#'     fit_metrics <- purrr::map_df(vars_add, function(m = "credit.amount_woe"){
#'
#'       # message(m)
#'
#'       mod_prp <- paste(mod_act, m, sep = " + ")
#'
#'       fitaux <- glm(as.formula(mod_prp), family = binomial(), data = model$data)
#'
#'       tibble::tibble(
#'         var_added = m,
#'         auc = Metrics::auc(model$data[[yvar]], fitaux$fitted.values)
#'       )
#'
#'     })
#'
#'     fit_metrics <- dplyr::arrange(fit_metrics, desc(auc))
#'     fit_metrics <- mutate(fit_metrics, gain_auc = (auc - actual_auc)/auc)
#'
#'     print(fit_metrics)
#'
#'     var_to_add <- dplyr::pull(fit_metrics, var_added)[1]
#'     gain       <- dplyr::pull(fit_metrics, gain_auc)[1]
#'
#'     if(gain <= 0) break()
#'
#'     vars_act  <- c(vars_act, var_to_add)
#'     vars_add  <- setdiff(xvars, var_to_add)
#'
#'   }
#'
#'   fit
#'
#' }
#'
