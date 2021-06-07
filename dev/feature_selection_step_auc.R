#' Function to stem model given aux gains
#'
#' @param model The model to test
#' @param trace Show the trace
#'
#' @export
step_auc <- function(model, trace = TRUE){

  yvar  <- as.character(as.formula(model))[2]
  xvars <- as.character(as.formula(model))[3]
  xvars <- unlist(strsplit(xvars, "\\s+\\+\\s+"))

  vars_act  <- c(1)
  vars_add <- xvars

  for(s in 1:length(xvars)) {

    mod_act <- paste(vars_act, collapse = "+")
    mod_act <- paste(yvar, "~", mod_act)

    fit <- glm(as.formula(mod_act), family = binomial(), data = model$data)

    if(length(vars_act) == 1) {
      actual_auc <- 0.5
    } else {
      actual_auc <- Metrics::auc(model$data[[yvar]], fit$fitted.values)
    }

    if(length(vars_act) == 0) break()

    fit_metrics <- purrr::map_df(vars_add, function(m = "credit.amount_woe"){

      # message(m)

      mod_prp <- paste(mod_act, m, sep = " + ")

      fitaux <- glm(as.formula(mod_prp), family = binomial(), data = model$data)

      tibble::tibble(
        var_added = m,
        auc = Metrics::auc(model$data[[yvar]], fitaux$fitted.values)
      )

    })

    fit_metrics <- dplyr::arrange(fit_metrics, desc(auc))
    fit_metrics <- mutate(fit_metrics, gain_auc = (auc - actual_auc)/auc)

    print(fit_metrics)

    var_to_add <- dplyr::pull(fit_metrics, var_added)[1]
    gain       <- dplyr::pull(fit_metrics, gain_auc)[1]

    if(gain <= 0) break()

    vars_act  <- c(vars_act, var_to_add)
    vars_add  <- setdiff(xvars, var_to_add)

  }

  fit

}
