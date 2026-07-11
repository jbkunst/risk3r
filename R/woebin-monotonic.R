#' Simple monotonic retry wrapper around scorecard::woebin()
#'
#' `woebin_monotonic_scd_retry()` is a small wrapper around
#' [scorecard::woebin()]. It follows a simple retry strategy: for numeric
#' variables, retry `scorecard::woebin()` by increasing `count_distr_limit` until
#' `0.2`; if that is not enough, reduce `bin_num_limit`.
#'
#' This is not an optimal binning algorithm. It only attempts to make numeric
#' bins monotonic by making the binning coarser. Missing values are kept as a
#' separate bin and are excluded from the monotonicity check.
#'
#' @param dt A data frame with both x variables and the y variable.
#' @param y Name of the response variable.
#' @param x Optional character vector with predictor variables. If `NULL`, all
#'   variables except `y` and `var_skip` are used.
#' @param var_skip Optional character vector with variables to skip.
#' @param breaks_list Optional list of break points passed to
#'   [scorecard::woebin()].
#' @param special_values Optional list of special values passed to
#'   [scorecard::woebin()].
#' @param missing_join Optional argument passed to [scorecard::woebin()].
#' @param stop_limit,count_distr_limit,bin_num_limit,positive,no_cores,print_step,method,ignore_const_cols,ignore_datetime_cols,check_cate_num,replace_blank_inf
#'   Arguments passed to [scorecard::woebin()].
#' @param save_as Optional path to save the output as an RDS file.
#' @param ... Additional arguments passed to [scorecard::woebin()].
#' @param count_distr_step Numeric step used to increase `count_distr_limit`
#'   when a numeric variable is not monotonic.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A list compatible with the output of [scorecard::woebin()].
#' @export
#'
#' @examples
#' if (FALSE) {
#' data(germancredit, package = "scorecard")
#'
#' bins <- woebin_monotonic_scd_retry(
#'   germancredit,
#'   y = "creditability",
#'   x = c("duration.in.month", "credit.amount", "age.in.years"),
#'   no_cores = 1,
#'   verbose = FALSE
#' )
#'
#' woebin_summary(bins)
#' }
woebin_monotonic_scd_retry <- function(
  dt,
  y,
  x = NULL,
  var_skip = NULL,
  breaks_list = NULL,
  special_values = NULL,
  missing_join = NULL,
  stop_limit = 0.1,
  count_distr_limit = 0.05,
  bin_num_limit = NULL,
  positive = "bad|1",
  no_cores = 2,
  print_step = 0L,
  method = "tree",
  ignore_const_cols = TRUE,
  ignore_datetime_cols = TRUE,
  check_cate_num = TRUE,
  replace_blank_inf = TRUE,
  save_as = NULL,
  ...,
  count_distr_step = 0.025,
  verbose = TRUE
) {
  vars <- if (is.null(x)) setdiff(names(dt), y) else x
  vars <- vars[vars %in% names(dt)]
  vars <- setdiff(vars, var_skip)

  vars_numericas <- vars[
    vapply(dt[vars], function(z) is.numeric(z) || is.integer(z), logical(1))
  ]
  vars_categoricas <- setdiff(vars, vars_numericas)

  bins_categoricas <- list()
  bins_numericas <- list()

  if (length(vars_categoricas) > 0) {
    args_categoricas <- list(
      dt = dt,
      y = y,
      x = vars_categoricas,
      var_skip = NULL,
      breaks_list = breaks_list,
      special_values = special_values,
      missing_join = missing_join,
      stop_limit = stop_limit,
      count_distr_limit = count_distr_limit,
      positive = positive,
      no_cores = no_cores,
      print_step = print_step,
      method = method,
      ignore_const_cols = ignore_const_cols,
      ignore_datetime_cols = ignore_datetime_cols,
      check_cate_num = check_cate_num,
      replace_blank_inf = replace_blank_inf,
      save_as = NULL,
      ...
    )

    if (!is.null(bin_num_limit)) args_categoricas$bin_num_limit <- bin_num_limit

    invisible(capture.output(
      bins_categoricas <- suppressWarnings(
        suppressMessages(do.call(scorecard::woebin, args_categoricas))
      )
    ))
  }

  for (variable in vars_numericas) {
    if (isTRUE(verbose)) cli::cli_inform("Variable: {variable}")

    count_distr_var <- count_distr_limit
    bin_num_var <- bin_num_limit
    last_bin <- NULL

    repeat {
      bin_summary <- NULL
      breaks_variable <- if (is.null(breaks_list)) {
        NULL
      } else {
        breaks_list[intersect(names(breaks_list), variable)]
      }
      special_values_variable <- if (is.null(special_values)) {
        NULL
      } else {
        special_values[intersect(names(special_values), variable)]
      }

      args_variable <- list(
        dt = dt,
        y = y,
        x = variable,
        var_skip = NULL,
        breaks_list = breaks_variable,
        special_values = special_values_variable,
        missing_join = missing_join,
        stop_limit = stop_limit,
        count_distr_limit = count_distr_var,
        positive = positive,
        no_cores = no_cores,
        print_step = print_step,
        method = method,
        ignore_const_cols = ignore_const_cols,
        ignore_datetime_cols = ignore_datetime_cols,
        check_cate_num = check_cate_num,
        replace_blank_inf = replace_blank_inf,
        save_as = NULL,
        ...
      )

      if (!is.null(bin_num_var)) args_variable$bin_num_limit <- bin_num_var

      invisible(capture.output(
        bin_variable <- suppressWarnings(
          suppressMessages(do.call(scorecard::woebin, args_variable))
        )
      ))

      if (!is.null(bin_variable) && !is.null(bin_variable[[variable]])) {
        bin_summary <- risk3r::woebin_summary(bin_variable, sort = FALSE)
        bin_summary$monotone <- woebin_monotonic_bin_is_monotone(
          bin_variable[[variable]]
        )

        last_bin <- bin_variable[[variable]]

        if (is.null(bin_num_var)) {
          bin_num_var <- max(2L, bin_summary$n_categories[1])
        }
      }

      if (!is.null(bin_summary) && isTRUE(bin_summary$monotone[1])) {
        bins_numericas[[variable]] <- last_bin
        if (isTRUE(verbose)) cli::cli_alert_success("{variable}: monotone=TRUE.")
        break
      } else if (count_distr_var < 0.2) {
        count_distr_var <- min(count_distr_var + count_distr_step, 0.2)
        if (isTRUE(verbose)) {
          cli::cli_inform(
            "{variable}: no monotona, count_distr_limit = {count_distr_var}"
          )
        }
        next
      } else if (!is.null(bin_num_var) && bin_num_var > 2) {
        bin_num_var <- bin_num_var - 1L
        count_distr_var <- count_distr_limit
        if (isTRUE(verbose)) {
          cli::cli_inform("{variable}: no monotona, bin_num_limit = {bin_num_var}")
        }
        next
      } else {
        if (!is.null(last_bin)) {
          bins_numericas[[variable]] <- last_bin
          if (isTRUE(verbose)) {
            cli::cli_alert_warning(
              "{variable}: queda para revision; no se encontro monotonia."
            )
          }
        }

        break
      }
    }
  }

  bins <- c(bins_numericas, bins_categoricas)

  if (!is.null(save_as)) saveRDS(bins, file = save_as)

  bins
}

#' Check whether one WOE bin table is monotonic
#'
#' Checks whether `posprob` from one [scorecard::woebin()] bin table is
#' monotonic. Missing and special-value bins are excluded, and flat steps are
#' allowed.
#'
#' @param bin A single data frame from a [scorecard::woebin()] output list.
#'
#' @return A logical value.
#' @export
woebin_monotonic_bin_is_monotone <- function(bin) {
  posprob <- bin |>
    dplyr::filter(
      .data$breaks != "missing",
      !.data$is_special_values
    ) |>
    dplyr::pull(.data$posprob)

  is_monotone(posprob)
}

is_monotone <- function(x, tolerance = 1e-12) {
  if (any(is.na(x))) {
    message("Some values are NA, returning NA")
    return(NA)
  }

  if (length(x) <= 2L) return(TRUE)

  dx <- diff(x)
  all(dx >= -tolerance) || all(dx <= tolerance)
}
