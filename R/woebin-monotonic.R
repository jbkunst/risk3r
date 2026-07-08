#' Monotonic WOE binning with scorecard retries
#'
#' `woebin_monotonic()` is a pragmatic wrapper around [scorecard::woebin()].
#' Categorical variables are sent to `scorecard::woebin()` unchanged. Numeric
#' variables are processed one by one and retried with stricter binning settings
#' until the selected metric is monotonic or the retry limits are reached.
#'
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
#' @param missing_join Optional argument passed to [scorecard::woebin()] when not
#'   `NULL`.
#' @param stop_limit,count_distr_limit,bin_num_limit,positive,no_cores,print_step,method,ignore_const_cols,ignore_datetime_cols,check_cate_num,replace_blank_inf
#'   Arguments passed to [scorecard::woebin()].
#' @param save_as Optional path to save the output as an RDS file.
#' @param ... Additional arguments passed to [scorecard::woebin()].
#' @param count_distr_step Numeric step used to increase `count_distr_limit`
#'   when a numeric variable is not monotonic.
#' @param count_distr_limit_max Maximum `count_distr_limit` to try.
#' @param bin_num_limit_min Minimum `bin_num_limit` to try when reducing the
#'   number of bins.
#' @param monotonic_metric Column used to check monotonicity. Defaults to
#'   `"posprob"`.
#' @param strict Logical. If `TRUE`, adjacent bins must be strictly increasing or
#'   strictly decreasing. If `FALSE`, flat adjacent bins are allowed.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A list compatible with the output of [scorecard::woebin()].
#' @export
#'
#' @examples
#' if (FALSE) {
#' data(germancredit, package = "scorecard")
#'
#' bins <- woebin_monotonic(
#'   germancredit,
#'   y = "creditability",
#'   x = c("duration.in.month", "credit.amount", "age.in.years"),
#'   no_cores = 1,
#'   verbose = FALSE
#' )
#'
#' woebin_summary(bins)
#' }
woebin_monotonic <- function(
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
  count_distr_limit_max = 0.2,
  bin_num_limit_min = 2L,
  monotonic_metric = "posprob",
  strict = FALSE,
  verbose = TRUE
) {
  stopifnot(is.data.frame(dt))
  stopifnot(is.character(y), length(y) == 1L, y %in% names(dt))
  stopifnot(is.numeric(count_distr_step), count_distr_step > 0)
  stopifnot(is.numeric(count_distr_limit_max), count_distr_limit_max <= 0.2)
  stopifnot(is.numeric(bin_num_limit_min), bin_num_limit_min >= 2)

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

    bins_categorical <- risk3r_woebin_monotonic_call(
      dt = dt,
      y = y,
      x = vars_categorical,
      var_skip = NULL,
      breaks_list = risk3r_filter_named_list(breaks_list, vars_categorical),
      special_values = risk3r_filter_named_list(special_values, vars_categorical),
      missing_join = missing_join,
      stop_limit = stop_limit,
      count_distr_limit = count_distr_limit,
      bin_num_limit = bin_num_limit,
      positive = positive,
      no_cores = no_cores,
      print_step = print_step,
      method = method,
      ignore_const_cols = ignore_const_cols,
      ignore_datetime_cols = ignore_datetime_cols,
      check_cate_num = check_cate_num,
      replace_blank_inf = replace_blank_inf,
      quiet = TRUE,
      ...
    )
  }

  for (variable in vars_numeric) {
    if (isTRUE(verbose)) cli::cli_inform("Variable: {variable}")

    count_distr_var <- count_distr_limit
    bin_num_var <- bin_num_limit
    last_bin <- NULL
    has_manual_breaks <- FALSE

    repeat {
      breaks_variable <- risk3r_filter_named_list(breaks_list, variable)
      special_values_variable <- risk3r_filter_named_list(special_values, variable)
      has_manual_breaks <- !is.null(breaks_variable)

      bin_variable <- risk3r_woebin_monotonic_call(
        dt = dt,
        y = y,
        x = variable,
        var_skip = NULL,
        breaks_list = breaks_variable,
        special_values = special_values_variable,
        missing_join = missing_join,
        stop_limit = stop_limit,
        count_distr_limit = count_distr_var,
        bin_num_limit = bin_num_var,
        positive = positive,
        no_cores = no_cores,
        print_step = print_step,
        method = method,
        ignore_const_cols = ignore_const_cols,
        ignore_datetime_cols = ignore_datetime_cols,
        check_cate_num = check_cate_num,
        replace_blank_inf = replace_blank_inf,
        quiet = TRUE,
        ...
      )

      monotone <- FALSE

      if (!is.null(bin_variable) && !is.null(bin_variable[[variable]])) {
        last_bin <- bin_variable[[variable]]

        monotone <- woebin_monotonic_bin_is_monotone(
          last_bin,
          metric = monotonic_metric,
          strict = strict
        )

        if (is.null(bin_num_var)) {
          n_non_missing_bins <- sum(as.character(last_bin$breaks) != "missing")
          bin_num_var <- max(as.integer(bin_num_limit_min), n_non_missing_bins)
        }
      }

      if (isTRUE(monotone)) {
        bins_numeric[[variable]] <- last_bin
        if (isTRUE(verbose)) cli::cli_alert_success("{variable}: monotone=TRUE.")
        break
      }

      if (isTRUE(has_manual_breaks)) {
        bins_numeric[[variable]] <- last_bin
        if (isTRUE(verbose)) {
          cli::cli_alert_warning("{variable}: manual breaks supplied; monotonicity was not forced.")
        }
        break
      }

      if (count_distr_var < count_distr_limit_max) {
        count_distr_var <- min(count_distr_var + count_distr_step, count_distr_limit_max)
        if (isTRUE(verbose)) {
          cli::cli_inform("{variable}: not monotonic, count_distr_limit = {count_distr_var}")
        }
        next
      }

      if (!is.null(bin_num_var) && bin_num_var > bin_num_limit_min) {
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
          cli::cli_alert_warning("{variable}: left for review; monotonicity was not found.")
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
#' Checks whether a column from one `scorecard::woebin()` bin table is monotonic.
#' The `missing` bin is excluded from the check.
#'
#' @param bin A single data frame from a `scorecard::woebin()` output list.
#' @param metric Column used to check monotonicity. Defaults to `"posprob"`.
#' @param strict Logical. If `TRUE`, adjacent bins must be strictly increasing or
#'   strictly decreasing. If `FALSE`, flat adjacent bins are allowed.
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
woebin_monotonic_bin_is_monotone <- function(bin, metric = "posprob", strict = FALSE) {
  stopifnot(is.data.frame(bin))
  stopifnot(is.character(metric), length(metric) == 1L, metric %in% names(bin))

  keep <- as.character(bin$breaks) != "missing"
  value <- bin[[metric]][keep]

  risk3r_is_monotone(value, strict = strict)
}

risk3r_is_monotone <- function(x, strict = FALSE) {
  if (any(is.na(x))) return(NA)

  if (length(x) <= 1L) return(TRUE)

  dx <- diff(x)

  if (isTRUE(strict)) {
    return(all(dx > 0) || all(dx < 0))
  }

  all(dx >= 0) || all(dx <= 0)
}

risk3r_filter_named_list <- function(x, vars) {
  if (is.null(x)) return(NULL)

  vars <- intersect(names(x), vars)
  if (length(vars) == 0L) return(NULL)

  x[vars]
}

risk3r_woebin_monotonic_call <- function(
  dt,
  y,
  x,
  var_skip,
  breaks_list,
  special_values,
  missing_join,
  stop_limit,
  count_distr_limit,
  bin_num_limit,
  positive,
  no_cores,
  print_step,
  method,
  ignore_const_cols,
  ignore_datetime_cols,
  check_cate_num,
  replace_blank_inf,
  quiet,
  ...
) {
  args <- list(
    dt = dt,
    y = y,
    x = x,
    var_skip = var_skip,
    breaks_list = breaks_list,
    special_values = special_values,
    stop_limit = stop_limit,
    count_distr_limit = count_distr_limit,
    positive = positive,
    no_cores = no_cores,
    print_step = print_step,
    method = method,
    ignore_const_cols = ignore_const_cols,
    ignore_datetime_cols = ignore_datetime_cols,
    check_cate_num = check_cate_num,
    replace_blank_inf = replace_blank_inf
  )

  if (!is.null(missing_join)) args$missing_join <- missing_join
  if (!is.null(bin_num_limit)) args$bin_num_limit <- bin_num_limit

  args <- c(args, list(...))

  if (isTRUE(quiet)) {
    out <- NULL
    invisible(utils::capture.output(
      out <- suppressWarnings(suppressMessages(do.call(scorecard::woebin, args)))
    ))
    return(out)
  }

  do.call(scorecard::woebin, args)
}
