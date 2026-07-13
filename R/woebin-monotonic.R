#' Monotonic WOE binning with interchangeable numeric engines
#'
#' `woebin_monotonic()` separates numeric and categorical variables. Categorical
#' variables are passed directly to [scorecard::woebin()]. Numeric variables are
#' processed one at a time with the engine selected in `type`.
#'
#' The `"scorecard_retry"` engine retries [scorecard::woebin()] with coarser
#' settings. The `"isoreg"` engine starts from a regular scorecard binning,
#' applies weighted isotonic regression to the observed positive rate of the
#' ordered numeric bins, merges bins belonging to the same isotonic block, and
#' calls [scorecard::woebin()] again with the resulting breaks. Because the
#' isotonic step only merges initial bins, `count_distr_limit` and
#' `bin_num_limit` from the initial scorecard binning remain satisfied.
#'
#' The isotonic direction is not imposed. Increasing and decreasing fits are
#' compared using weighted squared error, and the direction that better fits the
#' observed bin rates is selected. The resulting direction still requires
#' business review.
#'
#' @param dt A data frame containing the response and predictors.
#' @param y Name of the binary response variable.
#' @param x Optional character vector of predictors. If `NULL`, all variables
#'   except `y` and `var_skip` are used.
#' @param var_skip Optional character vector of variables to skip.
#' @param type Numeric monotonic-binning engine: `"scorecard_retry"` or
#'   `"isoreg"`.
#' @param breaks_list,special_values,missing_join,stop_limit,count_distr_limit,bin_num_limit,positive,no_cores,print_step,method,ignore_const_cols,ignore_datetime_cols,check_cate_num,replace_blank_inf
#'   Arguments passed to [scorecard::woebin()].
#' @param save_as Optional path where the resulting list is saved as an RDS.
#' @param ... Additional arguments passed to [scorecard::woebin()].
#' @param count_distr_step Step used by the `"scorecard_retry"` engine when
#'   increasing `count_distr_limit`.
#' @param verbose Logical. Print progress messages.
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
#'   type = "isoreg",
#'   no_cores = 1,
#'   verbose = FALSE
#' )
#' }
woebin_monotonic <- function(
  dt,
  y,
  x = NULL,
  var_skip = NULL,
  type = c("scorecard_retry", "isoreg"),
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
  type <- match.arg(type)

  vars <- if (is.null(x)) setdiff(names(dt), y) else x
  vars <- vars[vars %in% names(dt)]
  vars <- setdiff(vars, var_skip)

  vars_numericas <- vars[
    vapply(dt[vars], function(z) is.numeric(z) || is.integer(z), logical(1))
  ]
  vars_categoricas <- setdiff(vars, vars_numericas)

  common_args <- list(
    y = y,
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

  if (!is.null(bin_num_limit)) {
    common_args$bin_num_limit <- bin_num_limit
  }

  bins_categoricas <- list()

  if (length(vars_categoricas) > 0L) {
    args_categoricas <- c(
      list(dt = dt, x = vars_categoricas, var_skip = NULL),
      common_args
    )

    bins_categoricas <- quiet_woebin(args_categoricas)
  }

  engine <- switch(
    type,
    scorecard_retry = woebin_monotonic_scorecard_retry,
    isoreg = woebin_monotonic_isoreg
  )

  bins_numericas <- list()

  for (variable in vars_numericas) {
    if (isTRUE(verbose)) {
      cli::cli_inform("Variable: {variable}; type: {type}")
    }

    variable_args <- list(
      dt = dt,
      y = y,
      x = variable,
      breaks = list_element(breaks_list, variable),
      special_values = list_element(special_values, variable),
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
      ...,
      count_distr_step = count_distr_step,
      verbose = verbose
    )

    bin_variable <- do.call(engine, variable_args)

    if (!is.null(bin_variable)) {
      bins_numericas[[variable]] <- bin_variable
    }
  }

  bins <- c(bins_numericas, bins_categoricas)

  if (!is.null(save_as)) {
    saveRDS(bins, file = save_as)
  }

  bins
}

#' Monotonic numeric binning by retrying scorecard
#'
#' Numeric engine used by [woebin_monotonic()]. It repeatedly calls
#' [scorecard::woebin()], first increasing `count_distr_limit` up to `0.2` and
#' then reducing `bin_num_limit`, until the observed positive rates are
#' monotonic or the retry path is exhausted.
#'
#' @inheritParams woebin_monotonic
#' @param x Name of one numeric predictor.
#' @param breaks Optional break vector for `x`.
#' @param special_values Optional special-value vector for `x`.
#'
#' @return One bin table compatible with an element of
#'   [scorecard::woebin()].
#' @export
woebin_monotonic_scorecard_retry <- function(
  dt,
  y,
  x,
  breaks = NULL,
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
  ...,
  count_distr_step = 0.025,
  verbose = TRUE
) {
  count_distr_var <- count_distr_limit
  bin_num_var <- bin_num_limit
  last_bin <- NULL

  repeat {
    args <- woebin_one_variable_args(
      dt = dt, y = y, x = x, breaks = breaks,
      special_values = special_values, missing_join = missing_join,
      stop_limit = stop_limit, count_distr_limit = count_distr_var,
      bin_num_limit = bin_num_var, positive = positive, no_cores = no_cores,
      print_step = print_step, method = method,
      ignore_const_cols = ignore_const_cols,
      ignore_datetime_cols = ignore_datetime_cols,
      check_cate_num = check_cate_num,
      replace_blank_inf = replace_blank_inf, ...
    )

    bin_result <- quiet_woebin(args)
    bin <- bin_result[[x]]

    if (!is.null(bin)) {
      last_bin <- bin

      if (is.null(bin_num_var)) {
        bin_num_var <- max(2L, sum(monotonic_regular_rows(bin)))
      }

      if (isTRUE(woebin_monotonic_bin_is_monotone(bin))) {
        if (isTRUE(verbose)) cli::cli_alert_success("{x}: monotone=TRUE.")
        return(bin)
      }
    }

    if (count_distr_var < 0.2) {
      count_distr_var <- min(count_distr_var + count_distr_step, 0.2)
      if (isTRUE(verbose)) {
        cli::cli_inform("{x}: not monotonic; count_distr_limit = {count_distr_var}")
      }
    } else if (!is.null(bin_num_var) && bin_num_var > 2L) {
      bin_num_var <- bin_num_var - 1L
      count_distr_var <- count_distr_limit
      if (isTRUE(verbose)) {
        cli::cli_inform("{x}: not monotonic; bin_num_limit = {bin_num_var}")
      }
    } else {
      if (!is.null(last_bin) && isTRUE(verbose)) {
        cli::cli_alert_warning(
          "{x}: no monotonic solution found; returning the last valid binning."
        )
      }
      return(last_bin)
    }
  }
}

#' Monotonic numeric binning by weighted isotonic merging
#'
#' Numeric engine used by [woebin_monotonic()]. It first obtains a regular
#' [scorecard::woebin()] result, then applies weighted isotonic regression to
#' the ordered non-special, non-missing bins. Increasing and decreasing fits are
#' compared by weighted squared error. Adjacent bins in the same selected
#' isotonic block are merged and the final scorecard-compatible table is rebuilt
#' by calling [scorecard::woebin()] with the derived breaks.
#'
#' This engine does not force an economic direction. It chooses the direction
#' that best fits the observed rates; the analyst must still validate whether
#' that direction makes business sense.
#'
#' @inheritParams woebin_monotonic_scorecard_retry
#'
#' @return One bin table compatible with an element of
#'   [scorecard::woebin()].
#' @export
woebin_monotonic_isoreg <- function(
  dt,
  y,
  x,
  breaks = NULL,
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
  ...,
  count_distr_step = 0.025,
  verbose = TRUE
) {
  initial_args <- woebin_one_variable_args(
    dt = dt, y = y, x = x, breaks = breaks,
    special_values = special_values, missing_join = missing_join,
    stop_limit = stop_limit, count_distr_limit = count_distr_limit,
    bin_num_limit = bin_num_limit, positive = positive, no_cores = no_cores,
    print_step = print_step, method = method,
    ignore_const_cols = ignore_const_cols,
    ignore_datetime_cols = ignore_datetime_cols,
    check_cate_num = check_cate_num,
    replace_blank_inf = replace_blank_inf, ...
  )

  initial_result <- quiet_woebin(initial_args)
  initial_bin <- initial_result[[x]]

  if (is.null(initial_bin)) return(NULL)

  regular <- initial_bin[monotonic_regular_rows(initial_bin), , drop = FALSE]
  if (nrow(regular) <= 1L) return(initial_bin)

  rates <- regular$posprob
  weights <- regular$count

  fit_increasing <- weighted_pava(rates, weights, increasing = TRUE)
  fit_decreasing <- weighted_pava(rates, weights, increasing = FALSE)

  error_increasing <- sum(weights * (rates - fit_increasing$fitted)^2)
  error_decreasing <- sum(weights * (rates - fit_decreasing$fitted)^2)

  selected <- if (error_increasing <= error_decreasing) fit_increasing else fit_decreasing
  direction <- if (error_increasing <= error_decreasing) "increasing" else "decreasing"

  if (length(unique(selected$group)) == nrow(regular)) {
    if (isTRUE(verbose)) {
      cli::cli_alert_success("{x}: already monotonic ({direction}).")
    }
    return(initial_bin)
  }

  upper_bounds <- vapply(regular$bin, numeric_bin_upper_bound, numeric(1))
  group_end <- !duplicated(selected$group, fromLast = TRUE)
  keep_boundary <- group_end & selected$group != max(selected$group)
  new_breaks <- upper_bounds[keep_boundary]
  new_breaks <- sort(unique(new_breaks[is.finite(new_breaks)]))

  final_args <- woebin_one_variable_args(
    dt = dt, y = y, x = x, breaks = new_breaks,
    special_values = special_values, missing_join = missing_join,
    stop_limit = stop_limit, count_distr_limit = count_distr_limit,
    bin_num_limit = bin_num_limit, positive = positive, no_cores = no_cores,
    print_step = print_step, method = method,
    ignore_const_cols = ignore_const_cols,
    ignore_datetime_cols = ignore_datetime_cols,
    check_cate_num = check_cate_num,
    replace_blank_inf = replace_blank_inf, ...
  )

  final_result <- quiet_woebin(final_args)
  final_bin <- final_result[[x]]

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "{x}: isotonic merge complete ({direction}); {nrow(regular)} -> {sum(monotonic_regular_rows(final_bin))} bins."
    )
  }

  final_bin
}

#' Simple monotonic retry wrapper around scorecard::woebin
#'
#' This function is kept as a backwards-compatible wrapper. New code should use
#' [woebin_monotonic()] with `type = "scorecard_retry"`.
#'
#' @inheritParams woebin_monotonic
#' @return A list compatible with the output of [scorecard::woebin()].
#' @export
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
  woebin_monotonic(
    dt = dt, y = y, x = x, var_skip = var_skip,
    type = "scorecard_retry", breaks_list = breaks_list,
    special_values = special_values, missing_join = missing_join,
    stop_limit = stop_limit, count_distr_limit = count_distr_limit,
    bin_num_limit = bin_num_limit, positive = positive, no_cores = no_cores,
    print_step = print_step, method = method,
    ignore_const_cols = ignore_const_cols,
    ignore_datetime_cols = ignore_datetime_cols,
    check_cate_num = check_cate_num,
    replace_blank_inf = replace_blank_inf, save_as = save_as, ...,
    count_distr_step = count_distr_step, verbose = verbose
  )
}

#' Check whether one WOE bin table is monotonic
#'
#' Checks whether `posprob` is monotonic across the ordered regular numeric
#' bins. Missing and special-value bins are excluded.
#'
#' @param bin A single data frame from a [scorecard::woebin()] output list.
#' @return A logical value.
#' @export
woebin_monotonic_bin_is_monotone <- function(bin) {
  posprob <- bin$posprob[monotonic_regular_rows(bin)]
  is_monotone(posprob)
}

is_monotone <- function(x) {
  if (anyNA(x)) return(NA)
  length(x) <= 1L || all(diff(x) >= 0) || all(diff(x) <= 0)
}

weighted_pava <- function(y, weights, increasing = TRUE) {
  stopifnot(
    length(y) == length(weights), length(y) > 0L,
    all(is.finite(y)), all(is.finite(weights)), all(weights > 0)
  )

  values <- if (increasing) y else -y
  block_value <- values
  block_weight <- weights
  block_start <- seq_along(values)
  block_end <- seq_along(values)
  i <- 1L

  while (i < length(block_value)) {
    if (block_value[i] <= block_value[i + 1L]) {
      i <- i + 1L
      next
    }

    merged_weight <- block_weight[i] + block_weight[i + 1L]
    merged_value <- (
      block_value[i] * block_weight[i] +
        block_value[i + 1L] * block_weight[i + 1L]
    ) / merged_weight

    block_value[i] <- merged_value
    block_weight[i] <- merged_weight
    block_end[i] <- block_end[i + 1L]
    block_value <- block_value[-(i + 1L)]
    block_weight <- block_weight[-(i + 1L)]
    block_start <- block_start[-(i + 1L)]
    block_end <- block_end[-(i + 1L)]
    if (i > 1L) i <- i - 1L
  }

  fitted <- numeric(length(y))
  group <- integer(length(y))

  for (j in seq_along(block_value)) {
    idx <- block_start[j]:block_end[j]
    fitted[idx] <- block_value[j]
    group[idx] <- j
  }

  if (!increasing) fitted <- -fitted
  list(fitted = fitted, group = group)
}

numeric_bin_upper_bound <- function(bin) {
  bin <- gsub("\\s+", "", as.character(bin))
  inner <- sub("^[\\[(]", "", bin)
  inner <- sub("[\\])]$", "", inner)
  parts <- strsplit(inner, ",", fixed = TRUE)[[1L]]

  if (length(parts) != 2L) {
    stop("Unable to parse numeric scorecard bin: ", bin, call. = FALSE)
  }

  upper <- parts[2L]
  if (upper %in% c("Inf", "+Inf")) return(Inf)
  if (upper == "-Inf") return(-Inf)

  value <- suppressWarnings(as.numeric(upper))
  if (is.na(value)) {
    stop("Unable to parse numeric scorecard upper bound: ", upper, call. = FALSE)
  }
  value
}

monotonic_regular_rows <- function(bin) {
  is_missing <- bin$bin == "missing" | bin$breaks == "missing"
  is_special <- if ("is_special_values" %in% names(bin)) {
    !is.na(bin$is_special_values) & bin$is_special_values
  } else {
    rep(FALSE, nrow(bin))
  }
  !is_missing & !is_special
}

woebin_one_variable_args <- function(
  dt, y, x, breaks, special_values, missing_join, stop_limit,
  count_distr_limit, bin_num_limit, positive, no_cores, print_step, method,
  ignore_const_cols, ignore_datetime_cols, check_cate_num,
  replace_blank_inf, ...
) {
  args <- list(
    dt = dt, y = y, x = x, var_skip = NULL,
    breaks_list = if (is.null(breaks)) NULL else stats::setNames(list(breaks), x),
    special_values = if (is.null(special_values)) NULL else stats::setNames(list(special_values), x),
    missing_join = missing_join, stop_limit = stop_limit,
    count_distr_limit = count_distr_limit, positive = positive,
    no_cores = no_cores, print_step = print_step, method = method,
    ignore_const_cols = ignore_const_cols,
    ignore_datetime_cols = ignore_datetime_cols,
    check_cate_num = check_cate_num,
    replace_blank_inf = replace_blank_inf, save_as = NULL, ...
  )

  if (!is.null(bin_num_limit)) args$bin_num_limit <- bin_num_limit
  args
}

quiet_woebin <- function(args) {
  result <- NULL
  invisible(capture.output(
    result <- suppressWarnings(
      suppressMessages(do.call(scorecard::woebin, args))
    )
  ))
  result
}

list_element <- function(x, name) {
  if (is.null(x) || is.null(names(x)) || !name %in% names(x)) return(NULL)
  x[[name]]
}
