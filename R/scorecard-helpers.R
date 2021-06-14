# https://stackoverflow.com/a/54136863/829971
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

inline_bar <- function(x = NULL, width = 5, add_percent = TRUE, ...) {

  # x <- runif(1)
  # x <- runif(2)
  stopifnot(
    is.numeric(x),
    # length(x) == 1,
    # x <= 1,
    0 <= x
  )

  b <- round(width * x)
  s <- width - b

  out <- purrr::map2_chr(b, s, function(x, y) {
    z <- rep(c("\U2588", "\U2591"), times = c(x, y))
    z <- paste0(z, collapse = "")
  })

  out

  if (add_percent) {
    out2 <- scales::percent(x, ...)

    out2 <- stringr::str_pad(out2, width = max(nchar(out2)) + 1, pad = " ")

    out <- paste0(out, out2)
  }

  out
}

#' @importFrom skimr inline_hist
inline_hist_aux <- function(counts = c(267, 105, 382, 196, 50)) {
  if (length(counts) == 1) {
    return("")
  }

  x <- rep(1:length(counts), counts)
  skimr::inline_hist(x, length(counts))
}

bin_ks <- function(bin) {

  # bin <- bins[[1]]

  bin <- dplyr::select(bin, .data$neg, .data$pos, .data$woe)

  bin <- tidyr::gather(bin, "y", "count", -.data$woe)

  bin <- tidyr::uncount(bin, .data$count, .remove = TRUE)

  bin[["count"]] <- NULL

  bin <- tibble::as_tibble(bin)

  bin <- dplyr::mutate(bin, y = as.numeric(as.factor(.data$y)) - 1)

  # count(bin, woe, y)
  # count(bin, y)
  # mod <- glm(as.formula("y ~ woe"), data = bin, family = "binomial")

  # ks(bin[["y"]], predict(mod))

  ks(bin[["y"]], bin[["woe"]])
}

bin_pretty <- function(bin) {
  bin <- tibble::as_tibble(bin)

  bin <- dplyr::mutate(
    bin,
    count_distr2 = inline_bar(.data$count_distr),
    posprob2 = inline_bar(.data$posprob)
  )

  bin
}

#' woebin fix for windows
#'
#' This is a wrapper for scorecard::woebin, this fixes some problem
#'  to replicate the cuts given on windows machines.
#'
#' See https://github.com/ShichenXie/scorecard/issues/50 for more details.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Defaults to NULL. If x is NULL, then all columns except y and var_skip are counted as x variables.
#' @param var_skip Name of variables that will skip for binning. Defaults to NULL.
#' @param breaks_list List of break points, Defaults to NULL. If it is not NULL, variable binning will based on the provided breaks.
#' @param special_values the values specified in special_values will be in separate bins. Defaults to NULL.
#' @param stop_limit Stop binning segmentation when information value gain ratio less than the 'stop_limit' if using tree method; or stop binning merge when the chi-square of each neighbor bins are larger than the threshold under significance level of 'stop_limit' and freedom degree of 1 if using chimerge method. Accepted range: 0-0.5; Defaults to 0.1. If it is 'N', each x value is a bin.
# 'qchisq(1-stoplimit, 1)'
#' @param count_distr_limit The minimum count distribution percentage. Accepted range: 0.01-0.2; Defaults to 0.05.
#' @param bin_num_limit Integer. The maximum number of binning. Defaults to 8.
#' @param positive Value of positive class, defaults to "bad|1".
#' @param no_cores Number of CPU cores for parallel computation. Defaults to 90 percent of total cpu cores.
#' @param print_step A non-negative integer. Defaults to 1. If print_step>0, print variable names by each print_step-th iteration. If print_step=0 or no_cores>1, no message is print.
#' @param method Four methods are provided, "tree" and "chimerge" for optimal binning that support both numerical and categorical variables, and 'width' and 'freq' for equal binning that support numerical variables only. Defaults to "tree".
#' @param save_breaks_list A string. The file name to save breaks_list. Defaults to None.
#' @param ignore_const_cols Logical. Ignore constant columns. Defaults to TRUE.
#' @param ignore_datetime_cols Logical. Ignore datetime columns. Defaults to TRUE.
#' @param check_cate_num Logical. Check whether the number of unique values in categorical columns larger than 50. It might make the binning process slow if there are too many unique categories. Defaults to TRUE.
#' @param replace_blank_inf Logical. Replace blank values with NA and infinite with -1. Defaults to TRUE.
#' @param control a ctree::ctree_control list element
# @param ... Additional parameters.
#'
#' @examples
#'
#' if (FALSE) {
#'   data(germancredit, package = "scorecard")
#'
#'   bins <- woebin2(
#'     dt = germancredit,
#'     y = "creditability",
#'     # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
#'     no_cores = 0,
#'     method = "tree"
#'   )
#'
#'   bins
#'
#'   if (require(scorecard)) {
#'     library(scorecard)
#'     options(bin_close_right = TRUE)
#'   }
#'
#'   bins <- woebin2(
#'     dt = germancredit,
#'     y = "creditability",
#'     # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
#'     no_cores = 0,
#'     method = "tree"
#'   )
#'
#'   bins
#'
#'   bins_ctree <- woebin2(
#'     dt = germancredit,
#'     y = "creditability",
#'     method = "ctree",
#'     no_cores = 0,
#'     control = partykit::ctree_control(alpha = 1, maxdepth = 4)
#'   )
#'
#'   woebin_summary(bins)
#'   woebin_summary(bins_ctree)
#' }
#' @importFrom scorecard woebin
#' @importFrom purrr map pluck
#'
#' @export
woebin2 <- function(dt, y, x = NULL,
                    var_skip = NULL, breaks_list = NULL,
                    special_values = NULL, stop_limit = 0.1, count_distr_limit = 0.05,
                    bin_num_limit = 8, positive = "bad|1", no_cores = NULL,
                    print_step = 0L, method = "tree", save_breaks_list = NULL,
                    ignore_const_cols = TRUE, ignore_datetime_cols = TRUE,
                    check_cate_num = TRUE, replace_blank_inf = TRUE,
                    # bin_close_right = TRUE,
                    control = partykit::ctree_control()) {

  # dt = germancredit
  # y = "creditability"
  # x = c("credit.amount", "housing", "duration.in.month", "purpose")
  # x <- NULL
  # var_skip = NULL; breaks_list = NULL;
  # special_values = NULL; stop_limit = 0.1; count_distr_limit = 0.05;
  # bin_num_limit = 8; positive = "bad|1"; no_cores = NULL;
  # print_step = 0L; method = "tree"; save_breaks_list = NULL;
  # ignore_const_cols = TRUE; ignore_datetime_cols = TRUE;
  # check_cate_num = TRUE; replace_blank_inf = TRUE

  # default <- getOption("bin_close_right")
  # options(bin_close_right = TRUE)


  if (method %in% c("tree", "chimerge")) {
    bins1 <- woebin(
      dt, y,
      x = x, var_skip = var_skip, breaks_list = breaks_list,
      special_values = special_values, stop_limit = stop_limit, count_distr_limit = count_distr_limit,
      bin_num_limit = bin_num_limit, positive = positive, no_cores = no_cores,
      print_step = print_step, method = method, save_breaks_list = save_breaks_list,
      ignore_const_cols = ignore_const_cols, ignore_datetime_cols = ignore_datetime_cols,
      check_cate_num = check_cate_num, replace_blank_inf = replace_blank_inf
    )

    brks_lst <- purrr::map(bins1, purrr::pluck, "breaks")

    bins <- woebin(dt, y, x = x, breaks_list = brks_lst)

    if (!identical(bins1, bins)) message("Differences between bins")
  } else if (method == "ctree") {
    if (!is.null(x)) {
      dt <- dplyr::select(dt, y, x)
    }

    x_var <- utils::getFromNamespace("x_variable", "scorecard")

    xs <- x_var(dt, y, x, var_skip = var_skip)

    tbl <- tibble::tibble(
      x = as.list(dplyr::select(dt, dplyr::all_of(xs))),
      variable = xs
    )

    bins <- purrr::pmap(tbl, function(x, variable) {
      message("tree: ", variable)

      woebin_ctree(
        y = dplyr::pull(dt, y),
        x = x,
        namevar = variable,
        count_distr_limit = count_distr_limit,
        control = control
      )
    })
  }

  # options(bin_close_right = default)

  bins <- map(bins, bin_pretty)

  bins
}

#' Interface scorecard::woebin for partykiy::ctree
#'
#' @param y A vector of response. Usually 0-1
#' @param x A predictive variable
#' @param namevar a character element
#' @param count_distr_limit The minimum count distribution percentage. Accepted range: 0.01-0.2; Defaults to 0.05.
#' @param control a ctree::ctree_control list element
#'
#' @examples
#'
#' data(germancredit, package = "scorecard")
#'
#' y <- germancredit$creditability
#'
#' x <- germancredit$duration.in.month
#'
#' woebin_ctree(y, x, "duration", count_distr_limit = 0.05)
#'
#' woebin_ctree(y, x, "duration", count_distr_limit = 0.2)
#'
#' woebin_ctree(
#'   y,
#'   x,
#'   "duration",
#'   count_distr_limit = 0.05,
#'   control = partykit::ctree_control(alpha = 0.5)
#' )
#'
#' x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA
#' woebin_ctree(y, x, "duration")
#'
#' x <- germancredit$purpose
#' woebin_ctree(y, x, "purpose")
#'
#' x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA
#' woebin_ctree(y, x, "purpose_with_na")
#' @importFrom stats var as.formula complete.cases setNames
#' @export
woebin_ctree <- function(y, x, namevar = "variable", count_distr_limit = 0.05,
                         control = partykit::ctree_control()) {


  # x <- dtrain$flag_contact_phone
  # y <- dtrain$bad

  # data(germancredit, package = "scorecard")
  # germancredit <- tibble::tibble(germancredit)
  # y <- germancredit$creditability
  # x <- germancredit$duration.in.month
  # x <- germancredit$credit.amount
  # x <- germancredit$purpose

  # x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA

  d <- tibble::tibble(y = y, x = x)
  d <- dplyr::mutate_if(d, is.character, as.factor)
  d <- dplyr::filter(d, stats::complete.cases(d))

  # minbucket given count_distr_limit
  mb <- ceiling(count_distr_limit * nrow(d))
  control$minbucket <- mb

  tree <- partykit::ctree(stats::as.formula("y ~ x"), data = d, control = control)

  # plot(tree)
  predparty <- utils::getFromNamespace("predict.party", "partykit")

  d <- dplyr::mutate(d, node = predparty(object = tree, newdata = d, type = "node"))

  out <- dplyr::select(d, -.data$y)
  out <- dplyr::group_by(out, .data$node)

  if (is.factor(dplyr::pull(d, .data$x))) {
    out <- dplyr::distinct(out)
    out <- dplyr::summarise(out, x = paste0(x, collapse = "%,%"))
    out <- dplyr::pull(out, .data$x)
  } else if (is.numeric(dplyr::pull(d, .data$x))) {
    out <- dplyr::select(d, -.data$y)
    out <- dplyr::group_by(out, .data$node)
    out <- dplyr::summarise(out, x = max(.data$x))
    # out <- dplyr::summarise(out, x = min(.data$x))
    out <- dplyr::pull(out, .data$x)
    out <- out[-length(out)]
    # out <- out[-1]
    # out <- out + 1/1e6
    out <- c(out, Inf)
    out <- unique(out)
  }

  bin <- quiet(
    scorecard::woebin(
      dplyr::rename(tibble::tibble(y = y, x = x), !!namevar := x),
      y = "y",
      x = namevar,
      breaks_list = setNames(list(out), namevar),
      no_cores = 0,
    )
  )

  out <- c(0, Inf)
  out <- c(0)
  out <- c(Inf)
  out <- c(0, 1, Inf)

  out
  scorecard::woebin(
    dplyr::rename(tibble::tibble(y = y, x = x), !!namevar := x),
    y = "y",
    x = namevar,
    breaks_list = setNames(list(out), namevar),
    no_cores = 0,
  )


  bin[[namevar]]
}

#'
#' Get scorecard's woe_bin summary
#'
#' @param bins An output from `scorecard::woebin` or `risk3r::woebin2` functions.
#' @param sort Sort the data frame by information value. Default TRUE.
#'
#' @examples
#'
#' if (FALSE) {
#'   data(germancredit, package = "scorecard")
#'
#'   bins <- woebin2(
#'     germancredit,
#'     y = "creditability",
#'     # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
#'     no_cores = 0,
#'     method = "tree"
#'   )
#'
#'   bin_summary <- woebin_summary(bins)
#'   bin_summary
#'
#'   if (require(dplyr)) {
#'     dplyr::glimpse(bin_summary)
#'
#'     dplyr::filter(bin_summary, !monotone, !factor)
#'   }
#' }
#' @importFrom dplyr bind_rows group_by mutate summarize n as_tibble arrange desc
#' @importFrom rlang .data :=
#' @export
woebin_summary <- function(bins, sort = TRUE) {
  is_strictly_monotone <- function(x) {
    if (any(is.na(x))) {
      message("Some value are NA, returning NA")
      return(NA)
    }

    if (length(x) == 1) {
      return(TRUE)
    }

    dim(table(sign(diff(x)))) == 1
  }

  is_bin_factor_character <- function(bin) {
    all(stringr::str_detect(bin, "[a-zA-Z]"))
  }

  dbiv <- dplyr::bind_rows(bins)

  dbiv <- dplyr::as_tibble(dbiv)

  dbiv <- dplyr::group_by(dbiv, .data$variable)

  dbiv <- dplyr::summarize(
    dbiv,
    n_categories = dplyr::n(),
    iv = unique(.data$total_iv),
    hhi = hhi(.data$count_distr),
    count_distr_max = max(.data$count_distr),
    count_distr_min = min(.data$count_distr),
    has_missing = any(.data$bin == "missing"),
    has_special_values = any(.data$is_special_values),
    factor = is_bin_factor_character(.data$bin),
    distribution = inline_hist_aux(.data$count),
    breaks = list(.data$breaks)
  )

  # KS
  dks <- purrr::map_dbl(bins, bin_ks)
  dks <- tibble::tibble(variable = names(dks), ks = as.vector(dks))

  dbiv <- dplyr::left_join(dbiv, dks, by = "variable")

  # monotone
  dmn <- purrr::map(bins, dplyr::select, .data$bin, .data$posprob, .data$breaks)
  dmn <- purrr::map(dmn, dplyr::filter, .data$breaks != "missing")
  dmn <- purrr::map(dmn, dplyr::pull, .data$posprob)
  dmn <- purrr::map_lgl(dmn, is_strictly_monotone)
  dmn <- tibble::tibble(variable = names(dmn), monotone = as.vector(dmn))

  dbiv <- dplyr::left_join(dbiv, dmn, by = "variable")

  # labels
  dbiv <- dplyr::mutate(
    dbiv,
    iv_lbl  = iv_label(.data$iv),
    hhi_lbl = hhi_label(.data$hhi)
  )

  # relocate
  dbiv <- dplyr::relocate(dbiv, .data$distribution, .after = dplyr::last_col())
  dbiv <- dplyr::relocate(dbiv, .data$ks, .after = .data$iv)
  dbiv <- dplyr::relocate(dbiv, .data$monotone, .before = .data$factor)
  # dbiv <- dplyr::mutate(dbiv, `iv bar` = inline_bar(.data$iv))

  if (sort) dbiv <- dplyr::arrange(dbiv, dplyr::desc(.data$iv))

  # glimpse(dbiv)

  dbiv
}

#'
#' Minimalistic version of scorecard::woebin_ply
#'
#' @param variable A variable to get values asociated to a bin
#' @param bin A component (element) of `scorecard::woebin` output
#' @param value The value to return (a column of woebin table), defaults to "woe", can be: badprob, bin, etc.
#'
#' @examples
#'
#' data(germancredit, package = "scorecard")
#'
#' bins <- woebin2(germancredit, y = "creditability", x = "credit.amount")
#'
#' variable <- head(germancredit$credit.amount, 10)
#'
#' bin <- bins$credit.amount
#'
#' woebin_ply_min(variable, bin)
#'
#' woebin_ply_min(variable, bin, value = "posprob")
#' @importFrom scorecard woebin_ply
#' @importFrom purrr set_names
#' @export
woebin_ply_min <- function(variable, bin, value = "woe") {
  namevar <- bin[[1]][[1]]

  stopifnot(is.data.frame(bin))

  daux <- purrr::set_names(
    data.frame(variable),
    namevar
  )

  daux2 <- quiet(scorecard::woebin_ply(daux, list(bin)))

  daux <- dplyr::bind_cols(daux, daux2)

  if (value == "woe") {
    return(daux[[2]])
  }

  daux <- dplyr::left_join(
    daux,
    dplyr::select(bin, "woe", value),
    by = purrr::set_names("woe", paste0(namevar, "_woe"))
  )

  daux[[value]]
}

#'
#' Create a tidy data frame with a correlations and IV
#'
#' @param dt A data frame to apply `scorecard::woebin_ply` and calculate correlations.
#' @param bins An output from `scorecard::woebin` to create woe variables
#' @param upper upper
#' @param plot plot
#'
#' @examples
#'
#' if (FALSE) {
#'   data(germancredit, package = "scorecard")
#'
#'   vars <- c(
#'     "creditability", "duration.in.month", "credit.history",
#'     "purpose", "status.of.existing.checking.account", "property"
#'   )
#'
#'   dat <- germancredit[, vars]
#'
#'   bins <- woebin2(dat, y = "creditability", stop_limit = 0.0000001)
#'
#'   woebin_cor_iv(dat, bins)
#'
#'   datcor <- woebin_cor_iv(dat, bins)
#'
#'   library(dplyr)
#'
#'   cor_limit <- 0.15
#'
#'   datcor %>%
#'     filter(variable_1 != variable_2) %>%
#'     mutate(
#'       cor_conflict = ifelse(abs(cor) > cor_limit, TRUE, FALSE),
#'       variable_to_remove = ifelse(
#'         cor_conflict,
#'         ifelse(iv_variable_1 > iv_variable_2, variable_2, variable_1),
#'         NA
#'       )
#'     )
#' }
#' @importFrom stringr str_remove
#' @importFrom tidyselect any_of
#' @export
#'
woebin_cor_iv <- function(dt, bins, upper = FALSE, plot = TRUE) {
  woebinsum <- woebin_summary(bins, sort = TRUE)
  woebinsum <- dplyr::select(woebinsum, .data$variable, .data$iv)
  woebinsum <- dplyr::mutate(woebinsum, rank = dplyr::row_number())

  datwoe <- as_tibble(scorecard::woebin_ply(dt, bins))

  datwoe_nms <- stringr::str_subset(names(datwoe), "_woe$")
  woebin_nms <- stringr::str_c(names(bins), "_woe")

  if (length(setdiff(datwoe_nms, woebin_nms))) {
    message("Some variables come in woe form but don't exists in the bin")
    message(
      paste0(
        stringr::str_remove(setdiff(datwoe_nms, woebin_nms), "_woe$"),
        collapse = ", "
      )
    )
  }

  if (length(setdiff(woebin_nms, datwoe_nms))) {
    message("Some variables are no in the data frame:")
    message(
      paste0(
        stringr::str_remove(setdiff(woebin_nms, datwoe_nms), "_woe$"),
        collapse = ", "
      )
    )
  }

  datwoe <- dplyr::select(datwoe, dplyr::any_of(woebin_nms))
  datwoe <- dplyr::rename_with(
    datwoe,
    .fn = stringr::str_remove_all,
    .cols = tidyselect::everything(),
    pattern = "_woe$"
  )

  # correlation part
  dcorrs <- corrr::correlate(datwoe)

  if (plot) corrr::rplot(dcorrs)

  dcorrs <- corrr::stretch(dcorrs)

  dcorrs <- dplyr::left_join(dcorrs, woebinsum, by = c("x" = "variable"))
  dcorrs <- dplyr::left_join(dcorrs, woebinsum, by = c("y" = "variable"), suffix = c("_x", "_y"))

  dcorrs <- dplyr::mutate(
    dcorrs,
    x = factor(.data$x, levels = dplyr::pull(woebinsum, .data$variable)),
    y = factor(.data$y, levels = dplyr::pull(woebinsum, .data$variable)),
  )

  dcorrs <- dplyr::rename(
    dcorrs,
    var1 = .data$x,
    var2 = .data$y,
    var1_iv = .data$iv_x,
    var2_iv = .data$iv_y,
    var1_rank = .data$rank_x,
    var2_rank = .data$rank_y
  )

  dcorrs <- dplyr::arrange(dcorrs, .data$var1, .data$var2)

  if (upper) {
    # piorize the best variable in terms of IV
    dcorrs <- dplyr::filter(dcorrs, .data$var1_rank < .data$var2_rank)
  }

  dcorrs
}
#'
#' #'
#' #' Create a tidy data frame from a correlation (cor) output
#' #'
#' #' @param datcor A matrix from cor function
#' #' @param upper Logical. Remove repeated correlations
#' #'
#' #' @examples
#' #'
#' #' data(germancredit, package = "scorecard")
#' #'
#' #' vars <- c("creditability", "duration.in.month", "credit.history", "age.in.years", "purpose")
#' #'
#' #' dat <- germancredit[, vars]
#' #'
#' #' bins <- woebin2(dat, y = "creditability")
#' #'
#' #' datwoe <- scorecard::woebin_ply(dat, bins)
#' #'
#' #' datwoe <- datwoe[, -c("creditability")]
#' #'
#' #' datcor <- cor(datwoe)
#' #'
#' #' datcor
#' #'
#' #' cor_tidy(datcor)
#' #'
#' #' cor_tidy(datcor, FALSE)
#' #'
#' #' @importFrom tidyr gather
#' #' @importFrom tibble rownames_to_column
#' #' @importFrom stats cor
#' #'
#' #' @export
#' cor_tidy <- function(datcor, upper = TRUE) {
#'
#'   # class(datcor)
#'   stopifnot(class(datcor) %in% "matrix")
#'
#'   daux <- as.data.frame(datcor)
#'   daux <- tibble::rownames_to_column(daux, var = "variable_1")
#'   daux <- tidyr::gather(daux, variable_2, cor, -variable_1)
#'
#'   # daux <- dplyr::mutate_if(daux, is.character, factor, levels = rownames(datcor))
#'
#'   if(upper) return(dplyr::filter(daux, as.numeric(variable_1) > as.numeric(variable_2)))
#'
#'   daux
#'
#' }
#'
#'
