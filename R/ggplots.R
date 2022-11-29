#' Auxiliar functions for data plots
#'
#' @param actual actual
#' @param predicted predicted
#'
#' @examples
#'
#' N <- 10000
#' predicted <- runif(N)
#' actual <- rbinom(N, size = 1, prob = predicted)
#'
#' roc_data(actual, predicted)
#' ecdf_data(actual, predicted)
#'
#' @export
roc_data <- function(actual, predicted){

  pred <- ROCR::prediction(predicted, actual)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  droc <- tibble(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))

  droc

}

#' @rdname roc_data
#' @export
ecdf_data <- function(actual, predicted){


  dks <- tibble(actual, predicted) %>%
    dplyr::group_by(.data$actual) %>%
    dplyr::mutate(ecdf = ecdf(.data$predicted)(.data$predicted)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$actual, .data$ecdf)

  dks

}

#' Plot models
#'
#' @param model model
#' @param newdata newdata
#' @param level = 0.95
#' @param upper upper
#' @param show_intercept show_intercept
#' @param colors colors
#' @param verbose verbose
#' @param ... ...
#'
#' @examples
#'
#' data("credit_woe")
#'
#' m <- glm(bad ~ ., family = binomial, data = head(credit_woe, 10000))
#' m <- featsel_stepforward(m, scale = 5, trace = 0)
#'
#' dnew <- tail(credit_woe, 10000)
#'
#' gg_model_roc(m)
#' gg_model_roc(m, newdata = dnew, size = 2)
#'
#' gg_model_ecdf(m)
#' gg_model_ecdf(m, newdata = dnew, size = 2)
#'
#' gg_model_dist(m)
#' gg_model_dist(m, newdata = dnew, alpha = 0.4, color = "transparent")
#'
#' gg_model_calibration(m)
#' gg_model_calibration(m, newdata = dnew, alpha = 0.05, size = 2)
#'
#' gg_model_coef(m)
#'
#' gg_model_corr(m)
#'
#' gg_model_vif(m)
#'
#' gg_model_importance(m)
#'
#' gg_model_partials(m)
#'
#' gg_model_coef(m) + ggplot2::coord_flip() + ggplot2::scale_y_discrete(limits = rev)
#' gg_model_vif(m)  + ggplot2::coord_flip() + ggplot2::scale_x_discrete(limits = rev)
#' gg_model_corr(m, upper = TRUE)
#' gg_model_corr(m, upper = TRUE) +
#'  ggplot2::coord_flip() +
#'  ggplot2::scale_y_discrete(limits = rev) +
#'  ggplot2::scale_x_discrete(limits = rev)
#'
#' @export
gg_model_roc <- function(model, newdata = NULL, ...) {

  r_n_p <- reponse_and_predictors_names(model)
  yvar <- r_n_p[["response"]]

  dfroc <- roc_data(model$data[[yvar]], model$fitted.values)

  dfroc <- dplyr::mutate(dfroc, sample = "train")

  if (!is.null(newdata)) {

    dfroc_test <- roc_data(
      dplyr::pull(newdata, yvar),
      predict(model, newdata = newdata)
    )

    dfroc_test <- dplyr::mutate(dfroc_test, sample = "test")

    dfroc <- bind_rows(
      dfroc,
      dfroc_test
    )

    dfroc <- dplyr::mutate(dfroc, sample = forcats::fct_inorder(sample))

  }

  ggplot2::ggplot(dfroc) +

    ggplot2::geom_path(
      data = data.frame(x = c(0, 1), y = c(0, 1)),
      ggplot2::aes(.data$x, .data$y),
      colour = "gray"
      ) +

    ggplot2::geom_line(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = .data$sample),
      ...
      ) +

    ggplot2::labs(
      x = "False positive rate (FPR)",
      y = "True positive rate (TPR)"
    )

}

#' @rdname gg_model_roc
#' @export
gg_model_ecdf <- function(model, newdata = NULL, ...) {

  r_n_p <- reponse_and_predictors_names(model)
  yvar <- r_n_p[["response"]]

  dfks <- ecdf_data(model$data[[yvar]], model$fitted.values)

  dfks <- dplyr::mutate(dfks, sample = "train")

  if (!is.null(newdata)) {

    dfks_test <- ecdf_data(
      dplyr::pull(newdata, yvar),
      predict(model, newdata = newdata, type = "response")
    )

    dfks_test <- dplyr::mutate(dfks_test, sample = "test")

    dfks <- bind_rows(
      dfks,
      dfks_test
    )

    dfks <- dplyr::mutate(dfks, sample = forcats::fct_inorder(sample))

  }

  dfks <- dplyr::mutate(dfks, actual = as.character(.data$actual))

  p <- ggplot2::ggplot(dfks) +

    ggplot2::geom_line(
      mapping = ggplot2::aes(x = .data$predicted, y = .data$ecdf, color = .data$actual),
      ...
    ) +

    ggplot2::labs(
      x = "False positive rate (FPR)",
      y = "True positive rate (TPR)"
    )

  if (!is.null(newdata)) {

    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$sample))

  }

  p

}

#' @rdname gg_model_roc
#' @export
gg_model_dist <- function(model, newdata = NULL, alpha = 0.5, ...) {

  r_n_p <- reponse_and_predictors_names(model)
  yvar <- r_n_p[["response"]]

  ddist <- tibble(
    actual = model$data[[yvar]],
    predicted = model$fitted.values
    )

  ddist <- dplyr::mutate(ddist, sample = "train")

  if (!is.null(newdata)) {

    ddist_test <- tibble(
      actual = dplyr::pull(newdata, yvar),
      predicted = predict(model, newdata = newdata, type = "response")
      )

    ddist_test <- dplyr::mutate(ddist_test, sample = "test")

    ddist <- bind_rows(
      ddist,
      ddist_test
    )

    ddist <- dplyr::mutate(ddist, sample = forcats::fct_inorder(sample))

  }

  ddist <- dplyr::mutate(ddist, actual = as.character(.data$actual))

  p <- ggplot2::ggplot(ddist) +

    ggplot2::geom_density(
      mapping = ggplot2::aes(x = .data$predicted, fill = .data$actual),
      alpha = alpha,
      ...
    )


  if (!is.null(newdata)) {

    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$sample))

  }

  p

}

#' @rdname gg_model_roc
#' @importFrom stats confint
#' @export
gg_model_coef <- function(model, level = 0.95, show_intercept = FALSE, ...) {

  dmod <- broom::tidy(model)

  term_lvls <- broom::tidy(model) %>%
    dplyr::filter(.data$term != "(Intercept)")  %>%
    dplyr::pull(.data$term)

  # stats::confint(model, level = level)
  # https://stackoverflow.com/questions/35629374/confint-with-glm-stats-very-very-slow

  dconf <- stats::confint.default(model, level = level) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    tibble::as_tibble() %>%
    purrr::set_names(c("term", "lower", "upper"))

  dconf <- dplyr::left_join(dmod, dconf, by = "term")

  if(!show_intercept) {
    dconf <-  dplyr::filter(dconf, .data$term != "(Intercept)")
  }

  dconf <- dplyr::mutate(dconf, term = factor(.data$term, levels = term_lvls))

  ggplot2::ggplot(dconf, ggplot2::aes(y = forcats::fct_rev(.data$term))) +

    ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = "gray") +

    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
      ...
      ) +

    ggplot2::geom_point(
      ggplot2::aes(x = .data$estimate),
      ...
      ) +

    ggplot2::labs(
      caption = scales::percent(level)
    )

}

#' @rdname gg_model_roc
#' @importFrom stats cor
#' @export
gg_model_corr <- function(model, upper = FALSE, ...) {

  dcor <- model_corr_variables(model)

  if(upper) {

    dcor <- dplyr::filter(dcor, as.numeric(.data$term) > as.numeric(.data$term2))
  }

  dcor

  ggplot2::ggplot(
    dcor,
    ggplot2::aes(
      x = .data$term,
      y = forcats::fct_rev(.data$term2))
    ) +

    ggplot2::geom_tile(ggplot2::aes(fill = cor)) +

    ggplot2::scale_fill_continuous(
      name = "Correlations",
      limits = c(-1, 1),
      label = scales::percent,
      na.value = "gray90",
      breaks = seq(-1, 1, by = 0.5)
    ) +

    # ggplot2::geom_text(
    #   ggplot2::aes(label = round(.data$cor * 100)),
    #   ...
    #   ) +

    ggplot2::labs()

}

#' @rdname gg_model_roc
#' @export
gg_model_vif <- function(model, colors = c("#3aaf85", "#1b6ca8", "#cd201f"), ...) {

  dvif <- model_vif_variables(model)

  vif_brks <- c(-Inf, 5, 10, Inf)

  drect <- tibble(
    vif_label = factor(levels(dvif$vif_label), levels = levels(dvif$vif_label)),
    ymin = na.omit(dplyr::lag(vif_brks)),
    ymax = na.omit(dplyr::lead(vif_brks)),
  )

  ggplot2::ggplot(dvif)  +

    ggplot2::geom_rect(
      data = drect,
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax, fill = .data$vif_label),
      xmin = -Inf,
      xmax = Inf,
      alpha = 0.25
    ) +

    ggplot2::geom_col(
      ggplot2::aes(x = .data$term, y = .data$vif, fill = .data$vif_label)
    ) +

    ggplot2::scale_fill_manual(name = NULL, values = colors)

}

#' @rdname gg_model_roc
#' @param ... Additional arguments for celavi::variable_importance.
#' @importFrom celavi variable_importance
#' @export
gg_model_importance <- function(model, ...) {

  term_lvls <- broom::tidy(model) %>%
    dplyr::filter(.data$term != "(Intercept)")  %>%
    dplyr::pull(.data$term)

  r_n_p <- reponse_and_predictors_names(model)

  yvar <- r_n_p[["response"]]
  xvars <- r_n_p[["predictors"]]

  vi <- celavi::variable_importance(
    model,
    data = model$data,
    response = yvar,
    predict_function = stats::predict.glm,
  )


  plot(vi) + ggplot2::theme_get()


}


#' @rdname gg_model_roc
#' @param bins bins
#'
#' @examples
#'
#' data("credit_woe")
#'
#' m <- glm(bad ~ ., family = binomial, data = head(credit_woe, 10000))
#' m <- featsel_stepforward(m, scale = 5, trace = 0)
#'
#' bins <- scorecard::woebin(credit, "bad", no_cores = 1)
#'
#' gg_model_importance2(m, bins)
#' gg_model_importance2(m, bins, fill = "gray60", width = 0.5)
#'
#' @export
gg_model_importance2 <- function(model, bins, ...) {

  term_lvls <- broom::tidy(model) %>%
    dplyr::filter(.data$term != "(Intercept)")  %>%
    dplyr::pull(.data$term)

  scr <- scorecard::scorecard(bins, model)

  dscr <- scr %>%
    dplyr::bind_rows() %>%
    dplyr::filter(.data$variable != "basepoints") %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarise(w = max(.data$points) - min(.data$points)) %>%
    dplyr::mutate(w_norm = .data$w/sum(.data$w)) %>%
    dplyr::mutate(
      variable = stringr::str_c(.data$variable, "_woe"),
      variable = factor(.data$variable, levels = term_lvls)
      )

  ggplot2::ggplot(dscr) +

    ggplot2::geom_col(
      ggplot2::aes(.data$variable, .data$w_norm),
      ...
    ) +

    ggplot2::labs(y = "normalized weight")

}

# @method plot model_partials # plot methods
#' @importFrom utils hasName
#' @rdname gg_model_roc
#' @export
gg_model_partials <- function(model, newdata = NULL, verbose = TRUE, ...) {

  x <- model_partials(model, newdata = newdata, verbose = verbose)

  # stopifnot(attr(dfmetrics, "function") == "model_partials")

  if (hasName(x, "sample")) {

    dfg <- tidyr::gather(x, "key", "value", -.data$variable, -.data$sample)

    mapng <- ggplot2::aes(
      .data$variable,
      .data$value,
      group = .data$sample,
      color = .data$sample
      )
  } else {

    dfg <- tidyr::gather(x, "key", "value", -.data$variable)

    mapng <- ggplot2::aes(
      .data$variable,
      .data$value,
      group = .data$key
      )
  }

  ggplot2::ggplot(dfg) +
    ggplot2::geom_line(mapping = mapng, ...) +
    ggplot2::facet_wrap(ggplot2::vars(.data$key), ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(limits = c(0, NA))

}

#' @rdname gg_model_roc
#' @param alpha alpha
#' @param color color
#' @param size size
#' @param alpha_smooth alpha_smooth
#' @param color_smooth color_smooth
#' @param size_smooth size_smooth
#' @export
gg_model_calibration <- function(model, newdata = NULL,
                                 alpha = 0.01,
                                 color = "black",
                                 size  = 1,
                                 alpha_smooth = 0.50,
                                 color_smooth = "#3366FF",
                                 size_smooth = 1,
                                 ...) {

  r_n_p <- reponse_and_predictors_names(model)
  yvar <- r_n_p[["response"]]

  dcal <- tibble(
    actual = model$data[[yvar]],
    predicted = model$fitted.values
  )

  dcal <- dplyr::mutate(dcal, sample = "train")

  if (!is.null(newdata)) {

    dcal_test <- tibble(
      actual = dplyr::pull(newdata, yvar),
      predicted = predict(model, newdata = newdata, type = "response")
    )

    dcal_test <- dplyr::mutate(dcal_test, sample = "test")

    dcal <- bind_rows(
      dcal,
      dcal_test
    )

    dcal <- dplyr::mutate(dcal, sample = forcats::fct_inorder(sample))

  }

  dcal <- dplyr::mutate(dcal, actual = as.character(.data$actual))

  p <- ggplot2::ggplot(
    dcal,
    mapping = ggplot2::aes(
      x = .data$predicted,
      y = as.numeric(as.factor(.data$actual)) - 1
      ),
    ...) +

    ggplot2::geom_point(
      alpha = alpha,
      color = color,
      size  = size,
      ...) +

    ggplot2::geom_smooth(
      alpha = alpha_smooth,
      color = color_smooth,
      size  = size_smooth,
      ...)  +

    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1))

  if (!is.null(newdata)) {

    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$sample))

  }

  p

}



#' Plot woes
#'
#' @param woes woes
#' @param variable variable
#' @param color_bar color_bar
#' @param color_line color_line
#' @param ... ...
#'
#' @examples
#'
#' data("woes")
#'
#' pps <- gg_woes(woes)
#'
#' if(require(patchwork)){
#'
#'   purrr::reduce(pps, `+`)  &
#'      ggplot2::theme(
#'        axis.text.x = ggplot2::element_text(size = 7),
#'        axis.text.y = ggplot2::element_text(size = 7)
#'        )
#'
#' }
#'
#' gg_woes(woes, variable = "woe")
#' gg_woes(woes, variable = "bin_iv")
#'
#'
#' @export
#' @importFrom patchwork get_dim
gg_woes <- function(woes,
                    variable = "posprob",
                    color_bar = "gray75",
                    color_line = "#e22d36",
                    ...) {

  pvars <- woes %>%
    purrr::map(function(dwoe){

      # dwoe <- woes[[2]]

      namevar <- dplyr::first(dwoe[["variable"]])
      iv      <- dplyr::first(dwoe[["total_iv"]])

      ttle <- stringr::str_glue(
        "{ namevar } (IV = { ivfmt })",
        ivfmt  = scales::percent(iv, accuracy = 0.01)
        )

      message(ttle)

      dwoe <- dplyr::mutate(dwoe, bin = forcats::fct_inorder(.data$bin))

      dwoe[["var"]] <- dplyr::pull(dplyr::select(dwoe, dplyr::any_of(variable)))

      ggplot2::ggplot(dwoe, ggplot2::aes(x = .data$bin)) +

        ggplot2::geom_col(
          ggplot2::aes(y = .data$count_distr),
          fill = color_bar,
          width = 0.3
          ) +

        ggplot2::geom_line(
          ggplot2::aes(y = .data$var),
          size = 1.2,
          group = 1,
          color = color_line
        ) +

        ggplot2::geom_point(
          ggplot2::aes(y = .data$var),
          size = 3,
          fill = color_line,
          color = "white",
          shape = 21
        ) +

        ggplot2::scale_y_continuous(labels = scales::percent, limits = c(NA, NA)) +

        ggplot2::labs(
          subtitle = ttle,
          x = NULL,
          y = NULL
        )

    })

  invisible(patchwork::get_dim(ggplot2::ggplot()))

  pvars

}


# p1 <- gg_model_ks(m) +
#   ggplot2::labs(subtitle = "No facet")
# p1
#
# p2 <- gg_model_ks(m, newdata = head(daux, 100)) +
#   ggforce::facet_wrap_paginate(ggplot2::vars(.data$sample), nrow = 1, ncol = 1, page = 1) +
#   ggplot2::theme(
#     strip.background      = ggplot2::element_blank(),
#     strip.text.x          = ggplot2::element_blank()
#   )  +
#   ggplot2::labs(subtitle = "Facet + facet_wrap_paginate")
# p2
#
# library(patchwork)
#
# p1 + p2



