# library(scorecard)
# data("germancredit")
#
# dt_f = var_filter(germancredit, "creditability")
#
# dt_list = split_df(dt_f, "creditability")
# label_list = lapply(dt_list, function(x) x$creditability)
#
# bins = woebin(dt_list$train, "creditability")
# dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))
#
# # glm ------
# model <- glm(creditability ~ ., family = binomial(), data = dt_woe_list$train)
# vif(model, merge_coef = TRUE)
#
# as.formula(step(model, trace = FALSE))
#
# model_auc <- step_auc(model)
#
# plot_cum_auc(model_auc)

#' Function to stem model given aux gains
#'
#' @param model The model to test
#' @param trace Show the trace
#'
#' @importFrom stats binomial glm
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


plot_cum_auc <- function(model, list_dataframes = NULL) {

  yvar  <- as.character(as.formula(model))[2]
  xvars <- as.character(as.formula(model))[3]
  xvars <- unlist(strsplit(xvars, "\\s+\\+\\s+"))

  formulas <- purrr::map(1:length(xvars), function(nv = 3){

    mod_aux <- paste(xvars[1:nv], collapse = " + ")
    mod_aux <- paste(yvar, " ~ ", mod_aux)

    mod_aux

  })

  df_auc <- purrr::map_df(formulas, function(f = "creditability  ~  status.of.existing.checking.account_woe + duration.in.month_woe + credit.history_woe"){

    fitaux <- glm(as.formula(f), family = binomial(), data = model$data)

    tibble::tibble(
      auc = Metrics::auc(model$data[[yvar]], fitaux$fitted.values)
    )

  })

  df_auc <- tibble::add_column(df_auc, variable = xvars, .before = 1)

  df_auc

}
