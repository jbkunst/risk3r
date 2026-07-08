#' Simple monotonic retry wrapper around scorecard::woebin()
#'
#' `woebin_monotonic_retry()` is a small wrapper around
#' [scorecard::woebin()]. It does not solve an optimal binning problem and it
#' does not try to choose the best monotonic cuts. It simply retries numeric
#' variables with coarser settings until `posprob` is monotonic or the retry
#' path is exhausted.
#'
#' The retry path is intentionally basic: increase `count_distr_limit` up to
#' `0.2`; if that does not work, reduce `bin_num_limit`; if that still does not
#' work, keep the last valid binning for analyst review.
#'
#' Categorical variables are processed with regular [scorecard::woebin()].
#' Missing values are kept as a separate bin and are excluded from the
#' monotonicity check.
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
#' bins <- woebin_monotonic_retry(
#'   germancredit,
#'   y = "creditability",
#'   x = c("duration.in.month", "credit.amount", "age.in.years"),
#'   no_cores = 1,
#'   verbose = FALSE
#' )
#'
#' woebin_summary(bins)
#' }
woebin_monotonic_retry <- function(
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
  stopifnot(is.data.frame(dt))
  stopifnot(is.character(y), length(y) == 1L, y %in% names(dt))
  stopifnot(is.numeric(count_distr_step), count_distr_step > 0)

  vars <- if (is.null(x)) setdiff(names(dt), y) else x
  vars <- vars[vars %in% names(dt)]
  vars <- setdiff(vars, var_skip)

  vars_numeric <- vars[vapply(dt[vars], function(z) is.numeric(z) || is.integer(z), logical(1))]
  vars_categorical <- setdiff(vars, vars_numeric)

  bins_categorical <- list()
  bins_numeric <- list()

  if (length(vars_categorical) > 0L) {
    if (isTRUE(verbose)) {
      cli::cli_inform("Binning categorical variables with scorecard::woebin().")
    }

    args_categorical <- list(
      dt = dt,
      y = y,
      x = vars_categorical,
      var_skip = NULL,
      breaks_list = risk3r_filter_named_list(breaks_list, vars_categorical),
      special_values = risk3r_filter_named_list(special_values, vars_categorical),
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

    if (!is.null(missing_join)) args_categorical$missing_join <- missing_join
    if (!is.null(bin_num_limit)) args_categorical$bin_num_limit <- bin_num_limit

    bins_categorical <- risk3r_quiet_do_call(scorecard::woebin, args_categorical)
  }

  for (variable in vars_numeric) {
    if (isTRUE(verbose)) cli::cli_inform("Variable: {variable}")

    count_distr_var <- count_distr_limit
    bin_num_var <- bin_num_limit
    last_bin <- NULL

    repeat {
      breaks_variable <- risk3r_filter_named_list(breaks_list, variable)
      special_values_variable <- risk3r_filter_named_list(special_values, variable)

      args_variable <- list(
        dt = dt,
        y = y,
        x = variable,
        var_skip = NULL,
        breaks_list = breaks_variable,
        special_values = special_values_variable,
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

      if (!is.null(missing_join)) args_variable$missing_join <- missing_join
      if (!is.null(bin_num_var)) args_variable$bin_num_limit <- bin_num_var

      bin_variable <- risk3r_quiet_do_call(scorecard::woebin, args_variable)
      bin_summary <- NULL

      if (!is.null(bin_variable) && !is.null(bin_variable[[variable]])) {
        last_bin <- bin_variable[[variable]]
        bin_summary <- risk3r::woebin_summary(bin_variable, sort = FALSE)
        bin_summary$monotone <- woebin_monotonic_bin_is_monotone(last_bin)

        if (is.null(bin_num_var)) {
          bin_num_var <- max(2L, bin_summary$n_categories[1])
        }
      }

      if (!is.null(bin_summary) && isTRUE(bin_summary$monotone[1])) {
        bins_numeric[[variable]] <- last_bin
        if (isTRUE(verbose)) cli::cli_alert_success("{variable}: monotone=TRUE.")
        break
      }

      if (count_distr_var < 0.2) {
        count_distr_var <- min(count_distr_var + count_distr_step, 0.2)
        if (isTRUE(verbose)) {
          cli::cli_inform("{variable}: not monotonic, count_distr_limit = {count_distr_var}")
        }
        next
      }

      if (!is.null(bin_num_var) && bin_num_var > 2L) {
        bin_num_var <- bin_num_var - 1L
        count_distr_var <- count_distr_limit
        if (isTRUE(verbose)) {
          cli::cli_inform("{variable}: not monotonic, bin_num_limit = {bin_num_var}")
        }
        next
      }

      if (!is.null(last_bin)) {
        bins_numeric[[variable]] <- last_bin
        if (isTRUE(verbose)) {
          cli::cli_alert_warning("{variable}: left for review; no monotonic binning was found.")
        }
      }

      break
    }
  }

  bins <- c(bins_numeric, bins_categorical)

  if (!is.null(save_as)) saveRDS(bins, file = save_as)

  bins
}

#' Check whether one WOE bin table is monotonic
#'
#' Checks whether `posprob` from one `scorecard::woebin()` bin table is
#' monotonic. The `missing` bin is excluded from the check.
#'
#' @param bin A single data frame from a `scorecard::woebin()` output list.
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' if (FALSE) {
#' data(germancredit, package = "scorecard")
#' bins <- scorecard::woebin(germancredit, y = "creditability", x = "credit.amount")
#' woebin_monotonic_bin_is_monotone(bins$credit.amount)
#' }
woebin_monotonic_bin_is_monotone <- function(bin) {
  stopifnot(is.data.frame(bin))

  posprob <- bin |>
    dplyr::filter(.data$breaks != "missing") |>
    dplyr::pull(.data$posprob)

  risk3r_is_monotone(posprob)
}

risk3r_is_monotone <- function(x) {
  if (any(is.na(x))) {
    message("Some values are NA, returning NA.")
    return(NA)
  }

  if (length(x) <= 1L) return(TRUE)

  dx <- diff(x)

  all(dx >= 0) || all(dx <= 0)
}

risk3r_filter_named_list <- function(x, vars) {
  if (is.null(x)) return(NULL)

  vars <- intersect(names(x), vars)
  if (length(vars) == 0L) return(NULL)

  x[vars]
}

risk3r_quiet_do_call <- function(what, args) {
  out <- NULL
  invisible(utils::capture.output(
    out <- suppressWarnings(suppressMessages(do.call(what, args)))
  ))
  out
}
