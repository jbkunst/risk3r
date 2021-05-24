# https://stackoverflow.com/a/54136863/829971
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' @importFrom skimr inline_hist
inline_hist_aux <- function(counts = c(267, 105, 382, 196, 50)){

  if(length(counts) == 1) {
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

#'
#' woebin fix for windows
#'
#' This function fixes some problem to replicate the cuts given on windows machibes.
#'
#' See https://github.com/ShichenXie/scorecard/issues/50 for more details.#'
#'
#' @examples
#'
#' data(germancredit, package = "scorecard")
#'
#' bins <- woebin2(
#'  dt = germancredit,
#'  y = "creditability",
#'  # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
#'  method = "tree"
#' )
#'
#' bins
#'
#' if(require(scorecard)) {
#'   library(scorecard)
#'   options(bin_close_right = TRUE)
#' }
#'
#' bins <- woebin2(
#'  dt = germancredit,
#'  y = "creditability",
#'  # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
#'  method = "tree"
#' )
#'
#' bins
#'
#' bins_ctree <- woebin2(
#'   dt = germancredit,
#'   y = "creditability",
#'   method = "ctree",
#'   control = partykit::ctree_control(alpha = 1, maxdepth = 4)
#'   )
#'
#' woebin_summary(bins)
#' woebin_summary(bins_ctree)
#'
#'
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
                    bin_close_right = TRUE,
                    control = partykit::ctree_control()
                    ) {

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

  default <- getOption("bin_close_right")
  options(bin_close_right = TRUE)


  if(method %in% c("tree", "chimerge")){

    bins1 <- woebin(
      dt, y, x = x, var_skip = var_skip, breaks_list = breaks_list,
      special_values = special_values, stop_limit = stop_limit, count_distr_limit = count_distr_limit,
      bin_num_limit = bin_num_limit, positive = positive, no_cores = no_cores,
      print_step = print_step, method = method, save_breaks_list = save_breaks_list,
      ignore_const_cols = ignore_const_cols, ignore_datetime_cols = ignore_datetime_cols,
      check_cate_num = check_cate_num, replace_blank_inf = replace_blank_inf
    )

    brks_lst <- purrr::map(bins1, purrr::pluck, "breaks")

    bins <- woebin(dt, y, x = x, breaks_list = brks_lst)

    if(!identical(bins1, bins)) message("Differences between bins")


  } else if(method == "ctree") {

    if (!is.null(x))
      dt <- dplyr::select(dt, y, x)

    xs <- scorecard:::x_variable(dt, y, x, var_skip = var_skip)

    tbl <- tibble::tibble(
      x = as.list(dplyr::select(dt, dplyr::all_of(xs))),
      variable = xs
      )

    bins <- purrr::pmap(tbl, function(x, variable){

      message("tree: ", variable)

      woebin_ctree(
        y = dplyr::pull(dt, y),
        x = x,
        namevar  = variable,
        count_distr_limit = count_distr_limit,
        control = control
        )
    })


  }

  options(bin_close_right = default)

  bins <- map(bins, tibble::as_tibble)

  bins

}

#'
#' Interface scorecard::woebin for partykiy::ctree
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
#'   )
#'
#' x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA
#' woebin_ctree(y, x, "duration")
#'
#' x <- germancredit$purpose
#' woebin_ctree(y, x, "purpose")
#'
#' x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA
#' woebin_ctree(y, x, "purpose_with_na")
#'
#'
#' @importFrom stats var as.formula complete.cases setNames
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

  if(is.factor(dplyr::pull(d, .data$x))) {

    out <- dplyr::distinct(out)
    out <- dplyr::summarise(out, x = paste0(x, collapse = "%,%"))
    out <- dplyr::pull(out, .data$x)

  } else if(is.numeric(dplyr::pull(d, .data$x))){

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

  bin <- quiet(scorecard::woebin(
    dplyr::rename(tibble::tibble(y = y, x = x), !!namevar := x),
    y = "y",
    x = namevar,
    breaks_list = setNames(list(out), namevar)
    ))

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
#' data(germancredit, package = "scorecard")
#'
#' bins <- woebin2(
#'  germancredit,
#'  y = "creditability",
#'  # x = c("credit.amount", "housing", "duration.in.month", "purpose"),
#'  method = "tree"
#' )
#'
#' bin_summary <- woebin_summary(bins)
#' bin_summary
#'
#' if(require(dplyr)){
#'
#'   dplyr::glimpse(bin_summary)
#'
#'   dplyr::filter(bin_summary, !monotone, !factor)
#'
#' }
#'
#' @importFrom dplyr bind_rows group_by mutate summarize n as_tibble arrange desc
#' @importFrom rlang .data :=
#'
#' @export
woebin_summary <- function(bins, sort = TRUE) {

  is_strictly_monotone <- function(x) {

    if(any(is.na(x))) {
      message("Some value are NA, returning NA")
      return(NA)
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
    monotone = is_strictly_monotone(.data$posprob),
    factor = is_bin_factor_character(.data$bin),
    distribution = inline_hist_aux(.data$count)
    )

  dks <- purrr::map_dbl(bins, bin_ks)
  dks <- tibble::tibble(
    variable = names(dks),
    ks = as.vector(dks)
    )

  dbiv <- dplyr::left_join(dbiv, dks, by = "variable")

  dbiv <- dplyr::mutate(
    dbiv,
    iv_lbl  = iv_label(.data$iv),
    hhi_lbl = hhi_label(.data$hhi)
    )

  dbiv <- dplyr::relocate(dbiv, .data$distribution, .after = dplyr::last_col())
  dbiv <- dplyr::relocate(dbiv, .data$ks, .after = .data$iv)

  if(sort) dbiv <- dplyr::arrange(dbiv, dplyr::desc(.data$iv))

  dbiv

}

#'
#' Minimalistic version of woebin_ply
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
#'
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

  if(value == "woe") return(daux[[2]])

  daux <- dplyr::left_join(
    daux,
    dplyr::select(bin, "woe", value),
    by = purrr::set_names("woe", paste0(namevar, "_woe"))
  )

  daux[[value]]

}

#' #'
#' #' Create a tidy data frame with a correlations and IV
#' #'
#' #' @param dt A data frame to apply `scorecard::woebin_ply` and calculate correlations.
#' #' @param bins An output from `scorecard::woebin` to create woe variables
#' #'
#' #' @examples
#' #'
#' #' data(germancredit, package = "scorecard")
#' #'
#' #' vars <- c("creditability", "duration.in.month", "credit.history",
#' #'           "purpose", "status.of.existing.checking.account", "property")
#' #'
#' #' dat <- germancredit[, vars]
#' #'
#' #' bins <- woebin2(dat, y = "creditability", stop_limit = 0.0000001)
#' #'
#' #' woebin_cor_iv(dat, bins)
#' #'
#' #' datcor <- woebin_cor_iv(dat, bins)
#' #'
#' #' library(dplyr)
#' #'
#' #' cor_limit <- 0.15
#' #'
#' #' datcor %>%
#' #'   filter(variable_1 != variable_2) %>%
#' #'   mutate(
#' #'     cor_conflict = ifelse(abs(cor) > cor_limit, TRUE, FALSE),
#' #'     variable_to_remove = ifelse(
#' #'       cor_conflict,
#' #'       ifelse(iv_variable_1 > iv_variable_2, variable_2, variable_1),
#' #'       NA
#' #'    )
#' #' )
#' #'
#' #' @importFrom stringr str_remove
#' #'
#' #' @export
#' #'
#' woebin_cor_iv <- function(dt, bins) {
#'
#'   woebinsum   <- woebin_summary(bins)
#'   woebinsumiv <- dplyr::select(woebinsum, c("variable", "iv"))
#'
#'   datwoe <- scorecard::woebin_ply(dt, bins)
#'   datwoe <- dplyr::select(datwoe, paste0(names(bins), "_woe"))
#'
#'   datcor <- cor_tidy(cor(datwoe), upper = FALSE)
#'   datcor[[1]] <- stringr::str_remove(datcor[[1]], "_woe$")
#'   datcor[[2]] <- stringr::str_remove(datcor[[2]], "_woe$")
#'
#'   datcor <- dplyr::left_join(datcor, woebinsumiv, by = c("variable_1" = "variable"))
#'   datcor <- dplyr::left_join(datcor, woebinsumiv, by = c("variable_2" = "variable"), suffix = c("_variable_1", "_variable_2"))
#'
#'   datcor
#'
#' }
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
#' #'
#' #' Create a breaklist argument unsing ctree method
#' #'
#' #' @param dt A data frame using to create the bins
#' #' @param y Name of y variable.
#' #' @param ctrl A partykit::ctree_control instance
#' #' @param save_trees A logical indicating to return the trees objects or not
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'
#' #' data(germancredit, package = "scorecard")
#' #'
#' #' brks <- ctree_break_list(
#' #'   germancredit,
#' #'   "creditability",
#' #'    ctrl = partykit::ctree_control(alpha = 0.25)
#' #'    )
#' #'
#' #' bins_ctree <- scorecard::woebin(germancredit, "creditability", breaks_list = brks)
#' #'
#' #' bins_ctree$duration.in.month
#' #'
#' #' woebin_summary(bins_ctree)
#' #'
#' #' bins <- woebin2(germancredit, "creditability")
#' #'
#' #' bins$duration.in.month
#' #'
#' #' woebin_summary(bins)
#' #'
#' #' library(dplyr)
#' #'
#' #' full_join(
#' #'   woebin_summary(bins) %>% select(variable, iv),
#' #'   woebin_summary(bins_ctree) %>% select(variable, iv),
#' #'   by = "variable",
#' #'   suffix = c("_woebin", "_ctree"),
#' #'   ) %>%
#' #'   mutate(
#' #'   iv_ctree = round(iv_ctree, 5),
#' #'   iv_woebin = round(iv_woebin, 5),
#' #'   iv_ctree >= iv_woebin,
#' #'   delta = round(abs(iv_ctree - iv_woebin), 4),
#' #'   ) %>%
#' #'   arrange(desc(iv_ctree))
#' #'
#' #' bins_ctree$duration.in.month
#' #' bins$duration.in.month
#' #'
#' #' }
#' #'
#' #' @importFrom stats as.formula
#' #'
#' #' @export
#' #'
#' #'
#' ctree_break_list <- function(dt, y, ctrl = partykit::ctree_control(), save_trees = FALSE) {
#'
#'   # dt <- tibble::as_tibble(germancredit)
#'   # y  <- "creditability"
#'   # ctrl = partykit::ctree_control()
#'   # save_trees <- FALSE
#'
#'   vars <- setdiff(names(dt), y)
#'
#'   predparty <- utils::getFromNamespace("predict.party", "partykit")
#'
#'   brks <- purrr::map(vars, function(var = "duration.in.month"){
#'
#'     message(var)
#'
#'     d <- dplyr::select(dt, var, y)
#'     d <- dplyr::mutate_if(d, is.character, as.factor)
#'     d <- dplyr::filter(d, stats::complete.cases(d))
#'
#'     tri <- partykit::ctree(
#'       as.formula(paste(y, var, sep = " ~ ")),
#'       data = d,
#'       control = ctrl
#'     )
#'
#'     d <- dplyr::mutate(d, node = predparty(object = tri, newdata = d, type = "node"))
#'
#'     if(is.factor(dplyr::pull(d, var))) {
#'
#'       out <- dplyr::select(d, -y)
#'       out <- dplyr::distinct(out)
#'       out <- dplyr::group_by(out, node)
#'       out <- dplyr::summarise_all(out, paste0, collapse = "%,%")
#'       out <- dplyr::pull(out, var)
#'
#'     } else {
#'
#'       out <- dplyr::select(d, -y)
#'       out <- dplyr::group_by(out, node)
#'       out <- dplyr::summarise_all(out, max)
#'       out <- dplyr::pull(out, var)
#'       out <- out[-length(out)]
#'       out <- c(-Inf, out, Inf)
#'       out <- unique(out)
#'
#'     }
#'
#'     if(save_trees) {
#'       outf <- list("breaks" = out, "tree" = tri)
#'     } else {
#'       outf <- list("breaks" = out)
#'     }
#'
#'     outf
#'
#'   })
#'
#'   output <- purrr::map(brks, "breaks")
#'   output <- purrr::set_names(output, vars)
#'
#'   if(save_trees) {
#'
#'     trees <- purrr::map(brks, "tree")
#'     trees <- purrr::set_names(trees, vars)
#'
#'     attr(output, "trees") <- trees
#'
#'   }
#'
#'   output
#'
#' }
#'



# library(dplyr)
#
#
# # bins <- readRDS("~/modelos-provision/data/M2/Segmento 2/1/04_dwoes.rds")
# data(germancredit, package = "scorecard")
#
# bins <- scorecard::woebin(
#   germancredit,
#   y = "creditability",
#   # x = c("credit.amount", "housing", "duration.in.month", "telephone"),
#   method = "tree"
# )
#
# bin <- bins[["credit.amount"]]
#
# binnum_to_tree <- function(bin) {
#
#   varname <- bin[[1]][[1]]
#
#   tri <- bin %>%
#     tibble::as_tibble() %>%
#     dplyr::select(bin, good, bad, breaks) %>%
#     tidyr::gather(key, value, -bin, -breaks) %>%
#     tidyr::uncount(value) %>%
#     dplyr::mutate(breaks = as.numeric(breaks)) %>%
#     dplyr::mutate(breaks = ifelse(is.infinite(breaks), 9999999999999, breaks)) %>%
#     dplyr::mutate_if(is.character, as.factor) %>%
#     dplyr::rename("{ varname }" := breaks) %>%
#     partykit::ctree(
#       as.formula(str_c("key", varname, sep = " ~ ")),
#       data = .,
#       control = partykit::ctree_control(maxdepth = Inf, alpha = 1)
#     )
#
#   tri
#
# }
#
# plot(binnum_to_tree(bin))
#
# bin <- bins[["credit.history"]]
#
#
#
# aux <- function(x = c(53, 53, 53)) {
#   # x <- 100
#   y <- round(x/length(x))
#
#   d <- x[1] - sum(y)
#
#   y[1] <- y[1] + d
#
#   stopifnot(sum(y) == x[1])
#
#   y
#
# }
#
# binnum_to_tree <- function(bin) {
#
#   varname <- bin[[1]][[1]]
#
#   tri <- bin %>%
#     tibble::as_tibble() %>%
#     dplyr::select(good, bad, breaks) %>%
#     dplyr::mutate(breaks2 = map(breaks, ~ unlist(stringr::str_split(.x, "%,%")))) %>%
#     tidyr::unnest(breaks2) %>%
#     group_by(breaks) %>%
#     mutate(good = aux(good), bad  = aux(bad)) %>%
#     ungroup() %>%
#     select(-breaks) %>%
#     tidyr::gather(key, value, -breaks2) %>%
#     tidyr::uncount(value) %>%
#     dplyr::mutate_if(is.character, as.factor) %>%
#     dplyr::rename("{ varname }" := breaks2) %>%
#     partykit::ctree(
#       as.formula(str_c("key", varname, sep = " ~ ")),
#       data = .,
#       control = partykit::ctree_control(maxdepth = Inf, alpha = 0.95)
#     )
#
#   tri
#
# }
#
# bin
# plot(binnum_to_tree(bin), gp = gpar(fontsize = 9))
#
