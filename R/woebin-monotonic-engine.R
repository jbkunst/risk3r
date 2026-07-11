#' Monotonic WOE binning with interchangeable numeric engines
#'
#' `woebin_monotonic()` applies regular [scorecard::woebin()] binning to
#' categorical variables and a selected monotonic engine to numeric variables.
#' Each numeric engine receives one variable and returns one table compatible
#' with an element of the [scorecard::woebin()] output list.
#'
#' The `"scorecard_retry"` engine uses the existing retry/coarsening strategy.
#' The `"isoreg"` engine fits weighted isotonic regressions in both directions,
#' keeps the direction with the smallest weighted squared error, and merges
#' adjacent isotonic blocks until the requested minimum bin share and maximum
#' number of bins are satisfied. Its cut points are then passed to
#' [scorecard::woebin()] to produce the final compatible table.
#'
#' Statistical monotonicity does not guarantee business sense. The selected
#' direction and final bins still require expert review.
#'
#' @param dt A data frame containing the response and predictors.
#' @param y Name of the binary response variable.
#' @param x Optional character vector of predictor names. If `NULL`, all
#'   variables except `y` and `var_skip` are used.
#' @param var_skip Optional character vector of variables to skip.
#' @param type Numeric monotonic engine: `"scorecard_retry"` or `"isoreg"`.
#' @param breaks_list Optional list of manual breaks. Manual breaks take
#'   precedence over the selected numeric engine for that variable.
#' @param special_values Optional list of special values passed to
#'   [scorecard::woebin()].
#' @param missing_join Optional missing-value merge direction passed to
#'   [scorecard::woebin()]. Use `NULL` to keep missing values separate.
#' @param stop_limit,count_distr_limit,bin_num_limit,positive,no_cores,print_step,method,ignore_const_cols,ignore_datetime_cols,check_cate_num,replace_blank_inf
#'   Arguments compatible with [scorecard::woebin()]. For `type = "isoreg"`,
#'   `count_distr_limit` is applied within the regular non-missing,
#'   non-special sample and `bin_num_limit` limits regular numeric bins.
#' @param save_as Optional path used to save the returned list as an RDS file.
#' @param ... Additional arguments passed to [scorecard::woebin()].
#' @param count_distr_step Step used by the `"scorecard_retry"` engine.
#' @param verbose Logical. Print progress messages.
#'
#' @return A named list compatible with [scorecard::woebin()].
#' @export
#'
#' @examples
#' if (FALSE) {
#' data(germancredit, package = "scorecard")
#'
#' bins <- woebin_monotonic(
#'   germancredit,
#'   y = "creditability",
#'   x = c("duration.in.month", "credit.amount", "purpose"),
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
  bin_num_limit = 8,
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

  if (!is.data.frame(dt)) stop("`dt` must be a data frame.", call. = FALSE)
  if (length(y) != 1L || !y %in% names(dt)) {
    stop("`y` must name one column in `dt`.", call. = FALSE)
  }

  vars <- if (is.null(x)) setdiff(names(dt), y) else x
  vars <- vars[vars %in% names(dt)]
  vars <- setdiff(vars, c(y, var_skip))

  vars_numeric <- vars[
    vapply(dt[vars], function(z) is.numeric(z) || is.integer(z), logical(1))
  ]
  vars_categorical <- setdiff(vars, vars_numeric)

  params <- list(
    breaks_list = breaks_list,
    special_values = special_values,
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
    count_distr_step = count_distr_step,
    verbose = verbose,
    extra_args = list(...)
  )

  bins_categorical <- list()
  if (length(vars_categorical) > 0L) {
    bins_categorical <- woebin_monotonic_scorecard_call(
      dt = dt,
      y = y,
      x = vars_categorical,
      params = params,
      return_list = TRUE
    )
  }

  engine <- switch(
    type,
    scorecard_retry = woebin_monotonic_engine_scorecard_retry,
    isoreg = woebin_monotonic_engine_isoreg
  )

  bins_numeric <- setNames(vector("list", length(vars_numeric)), vars_numeric)
  for (variable in vars_numeric) {
    if (isTRUE(verbose)) cli::cli_inform("Variable: {variable}")
    bins_numeric[[variable]] <- engine(dt = dt, y = y, x = variable, params = params)
  }

  bins <- c(bins_numeric, bins_categorical)
  bins <- bins[intersect(vars, names(bins))]

  if (!is.null(save_as)) saveRDS(bins, file = save_as)

  bins
}

woebin_monotonic_engine_scorecard_retry <- function(dt, y, x, params) {
  args <- c(
    list(
      dt = dt,
      y = y,
      x = x,
      var_skip = NULL,
      breaks_list = woebin_monotonic_list_element(params$breaks_list, x),
      special_values = woebin_monotonic_list_element(params$special_values, x),
      missing_join = params$missing_join,
      stop_limit = params$stop_limit,
      count_distr_limit = params$count_distr_limit,
      bin_num_limit = params$bin_num_limit,
      positive = params$positive,
      no_cores = params$no_cores,
      print_step = params$print_step,
      method = params$method,
      ignore_const_cols = params$ignore_const_cols,
      ignore_datetime_cols = params$ignore_datetime_cols,
      check_cate_num = params$check_cate_num,
      replace_blank_inf = params$replace_blank_inf,
      save_as = NULL,
      count_distr_step = params$count_distr_step,
      verbose = params$verbose
    ),
    params$extra_args
  )

  do.call(woebin_monotonic_scd_retry, args)[[x]]
}

woebin_monotonic_engine_isoreg <- function(dt, y, x, params) {
  breaks_variable <- woebin_monotonic_list_element(params$breaks_list, x)

  if (!is.null(breaks_variable)) {
    bin <- woebin_monotonic_scorecard_call(
      dt = dt,
      y = y,
      x = x,
      params = params,
      breaks_list = breaks_variable
    )

    if (isTRUE(params$verbose) && !isTRUE(woebin_monotonic_bin_is_monotone(bin))) {
      cli::cli_alert_warning("{x}: manual breaks are not monotonic; they were kept unchanged.")
    }

    return(bin)
  }

  x_values <- dt[[x]]
  y_values <- woebin_monotonic_binary_response(dt[[y]], params$positive)

  if (isTRUE(params$replace_blank_inf)) x_values[is.infinite(x_values)] <- -1

  special_values <- woebin_monotonic_list_element(params$special_values, x)
  special_numeric <- woebin_monotonic_numeric_special_values(special_values)

  regular <- !is.na(x_values) & !is.na(y_values)
  if (length(special_numeric) > 0L) regular <- regular & !(x_values %in% special_numeric)

  if (!any(regular)) {
    if (isTRUE(params$verbose)) {
      cli::cli_alert_warning("{x}: no regular observations available for isotonic binning.")
    }

    return(woebin_monotonic_scorecard_call(
      dt = dt,
      y = y,
      x = x,
      params = params,
      special_values = special_values
    ))
  }

  levels <- woebin_monotonic_level_summary(
    x = x_values[regular],
    y = y_values[regular]
  )

  increasing <- woebin_monotonic_isoreg_candidate(
    levels = levels,
    increasing = TRUE,
    count_distr_limit = params$count_distr_limit,
    bin_num_limit = params$bin_num_limit
  )
  decreasing <- woebin_monotonic_isoreg_candidate(
    levels = levels,
    increasing = FALSE,
    count_distr_limit = params$count_distr_limit,
    bin_num_limit = params$bin_num_limit
  )

  candidate <- woebin_monotonic_choose_candidate(increasing, decreasing)
  breaks <- woebin_monotonic_block_breaks(levels$value, candidate$blocks)

  bin <- woebin_monotonic_scorecard_call(
    dt = dt,
    y = y,
    x = x,
    params = params,
    breaks_list = stats::setNames(list(breaks), x),
    special_values = special_values
  )

  attr(bin, "monotonic_method") <- "isoreg"
  attr(bin, "monotonic_direction") <- candidate$direction
  attr(bin, "isoreg_weighted_sse") <- candidate$sse
  attr(bin, "isoreg_breaks") <- breaks

  if (isTRUE(params$verbose)) {
    cli::cli_alert_success(
      "{x}: monotone=TRUE; direction={candidate$direction}; regular bins={nrow(candidate$blocks)}."
    )
  }

  bin
}

woebin_monotonic_scorecard_call <- function(
  dt,
  y,
  x,
  params,
  breaks_list = params$breaks_list,
  special_values = params$special_values,
  return_list = FALSE
) {
  args <- c(
    list(
      dt = dt,
      y = y,
      x = x,
      var_skip = NULL,
      breaks_list = breaks_list,
      special_values = special_values,
      missing_join = params$missing_join,
      stop_limit = params$stop_limit,
      count_distr_limit = params$count_distr_limit,
      positive = params$positive,
      no_cores = params$no_cores,
      print_step = params$print_step,
      method = params$method,
      ignore_const_cols = params$ignore_const_cols,
      ignore_datetime_cols = params$ignore_datetime_cols,
      check_cate_num = params$check_cate_num,
      replace_blank_inf = params$replace_blank_inf,
      save_as = NULL
    ),
    params$extra_args
  )

  if (!is.null(params$bin_num_limit)) args$bin_num_limit <- params$bin_num_limit
  if (is.null(args$print_info)) args$print_info <- FALSE

  output <- NULL
  invisible(capture.output(
    output <- suppressWarnings(suppressMessages(do.call(scorecard::woebin, args)))
  ))

  if (isTRUE(return_list)) return(output)
  output[[x]]
}

woebin_monotonic_list_element <- function(x, variable) {
  if (is.null(x) || is.null(names(x)) || !variable %in% names(x)) return(NULL)
  x[variable]
}

woebin_monotonic_binary_response <- function(y, positive) {
  result <- rep(NA_integer_, length(y))
  observed <- !is.na(y)
  values <- unique(y[observed])

  if (length(values) != 2L) {
    stop("The response must contain exactly two non-missing classes.", call. = FALSE)
  }

  if (is.logical(y)) {
    result[observed] <- as.integer(y[observed])
    return(result)
  }

  if (is.numeric(y) && all(values %in% c(0, 1))) {
    result[observed] <- as.integer(y[observed])
    return(result)
  }

  result[observed] <- as.integer(grepl(positive, as.character(y[observed])))
  if (length(unique(result[observed])) != 2L) {
    stop("`positive` does not identify exactly one response class.", call. = FALSE)
  }

  result
}

woebin_monotonic_numeric_special_values <- function(special_values) {
  if (is.null(special_values)) return(numeric())

  values <- unlist(special_values, use.names = FALSE)
  values <- unlist(strsplit(as.character(values), "%,%", fixed = TRUE), use.names = FALSE)
  values <- values[!values %in% c("missing", "NA", "")]
  values <- suppressWarnings(as.numeric(values))

  unique(values[!is.na(values)])
}

woebin_monotonic_level_summary <- function(x, y) {
  order_x <- order(x)
  x <- x[order_x]
  y <- y[order_x]

  values <- sort(unique(x))
  index <- match(x, values)
  count <- tabulate(index, nbins = length(values))
  pos <- as.numeric(rowsum(y, index, reorder = FALSE))

  data.frame(
    value = values,
    count = count,
    pos = pos,
    rate = pos / count,
    stringsAsFactors = FALSE
  )
}

woebin_monotonic_isoreg_candidate <- function(
  levels,
  increasing,
  count_distr_limit,
  bin_num_limit
) {
  fit <- woebin_monotonic_pava(
    rate = levels$rate,
    weight = levels$count,
    increasing = increasing
  )

  blocks <- woebin_monotonic_blocks(levels, fit$group)
  blocks <- woebin_monotonic_coarsen_blocks(
    blocks = blocks,
    count_distr_limit = count_distr_limit,
    bin_num_limit = bin_num_limit
  )

  list(
    direction = if (isTRUE(increasing)) "increasing" else "decreasing",
    blocks = blocks,
    sse = sum(blocks$sse),
    n_groups = nrow(blocks)
  )
}

woebin_monotonic_pava <- function(rate, weight, increasing = TRUE, tolerance = 1e-12) {
  rate_work <- if (isTRUE(increasing)) rate else -rate

  means <- rate_work
  weights <- weight
  starts <- seq_along(rate_work)
  ends <- seq_along(rate_work)
  i <- 1L

  while (length(means) > 1L && i < length(means)) {
    if (means[i] > means[i + 1L] + tolerance) {
      weight_new <- weights[i] + weights[i + 1L]
      means[i] <- (
        weights[i] * means[i] + weights[i + 1L] * means[i + 1L]
      ) / weight_new
      weights[i] <- weight_new
      ends[i] <- ends[i + 1L]

      means <- means[-(i + 1L)]
      weights <- weights[-(i + 1L)]
      starts <- starts[-(i + 1L)]
      ends <- ends[-(i + 1L)]

      if (i > 1L) i <- i - 1L
    } else {
      i <- i + 1L
    }
  }

  fitted_work <- rep(means, times = ends - starts + 1L)
  fitted <- if (isTRUE(increasing)) fitted_work else -fitted_work
  group <- cumsum(c(TRUE, abs(diff(fitted)) > tolerance))

  list(
    fitted = fitted,
    group = group,
    sse = sum(weight * (rate - fitted)^2)
  )
}

woebin_monotonic_choose_candidate <- function(increasing, decreasing, tolerance = 1e-12) {
  if (increasing$sse < decreasing$sse - tolerance) return(increasing)
  if (decreasing$sse < increasing$sse - tolerance) return(decreasing)

  if (increasing$n_groups <= decreasing$n_groups) increasing else decreasing
}

woebin_monotonic_blocks <- function(levels, group) {
  groups <- split(seq_len(nrow(levels)), group)

  rows <- lapply(groups, function(index) {
    count <- sum(levels$count[index])
    pos <- sum(levels$pos[index])
    mean <- pos / count

    data.frame(
      first = min(index),
      last = max(index),
      count = count,
      pos = pos,
      mean = mean,
      sse = sum(levels$count[index] * (levels$rate[index] - mean)^2),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

woebin_monotonic_coarsen_blocks <- function(
  blocks,
  count_distr_limit,
  bin_num_limit,
  tolerance = 1e-12
) {
  if (nrow(blocks) <= 1L) return(blocks)

  total_count <- sum(blocks$count)
  min_share <- if (is.null(count_distr_limit)) 0 else count_distr_limit
  max_bins <- if (is.null(bin_num_limit)) Inf else max(1L, as.integer(bin_num_limit))

  repeat {
    small <- blocks$count / total_count < min_share - tolerance
    too_many <- nrow(blocks) > max_bins

    if (!any(small) && !too_many) break
    if (nrow(blocks) <= 1L) break

    candidates <- seq_len(nrow(blocks) - 1L)
    if (any(small)) {
      candidates <- candidates[small[candidates] | small[candidates + 1L]]
    }
    if (length(candidates) == 0L) candidates <- seq_len(nrow(blocks) - 1L)

    costs <- vapply(candidates, function(i) {
      woebin_monotonic_merge_cost(blocks[i, ], blocks[i + 1L, ])
    }, numeric(1))

    merge_at <- candidates[which.min(costs)]
    merged <- woebin_monotonic_merge_blocks(
      blocks[merge_at, ],
      blocks[merge_at + 1L, ]
    )

    before <- if (merge_at > 1L) blocks[seq_len(merge_at - 1L), , drop = FALSE] else NULL
    after <- if (merge_at + 1L < nrow(blocks)) {
      blocks[seq.int(merge_at + 2L, nrow(blocks)), , drop = FALSE]
    } else {
      NULL
    }

    blocks <- do.call(rbind, Filter(Negate(is.null), list(before, merged, after)))
    rownames(blocks) <- NULL
  }

  blocks
}

woebin_monotonic_merge_cost <- function(left, right) {
  count <- left$count + right$count
  mean <- (left$count * left$mean + right$count * right$mean) / count

  left$count * (left$mean - mean)^2 +
    right$count * (right$mean - mean)^2
}

woebin_monotonic_merge_blocks <- function(left, right) {
  count <- left$count + right$count
  pos <- left$pos + right$pos
  mean <- pos / count

  data.frame(
    first = left$first,
    last = right$last,
    count = count,
    pos = pos,
    mean = mean,
    sse = left$sse + right$sse + woebin_monotonic_merge_cost(left, right),
    stringsAsFactors = FALSE
  )
}

woebin_monotonic_block_breaks <- function(values, blocks) {
  if (nrow(blocks) <= 1L) return(numeric())

  vapply(seq_len(nrow(blocks) - 1L), function(i) {
    left <- values[blocks$last[i]]
    right <- values[blocks$first[i + 1L]]
    left + (right - left) / 2
  }, numeric(1))
}
