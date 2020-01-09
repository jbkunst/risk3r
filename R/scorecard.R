#'
#' Get scorecard's woe_bin summary
#'
#' @param bins An output from `scorecard::woebin` function.
#'
#' @examples
#'
#' data(germancredit, package = "scorecard")
#'
#' bins <- scorecard::woebin(
#'  germancredit,
#'  y = "creditability",
#'  x = c("credit.amount", "housing", "duration.in.month"),
#'  method = "tree"
#' )
#'
#' woebin_summary(bins)
#'
#' @importFrom dplyr bind_rows group_by mutate summarize n
#'
#' @export
woebin_summary <- function(bins) {

  dbiv <- dplyr::bind_rows(bins)

  dbiv <- dplyr::group_by(dbiv, variable)

  dbiv <- dplyr::summarize(
    dbiv,
    n_cat = dplyr::n(),
    iv = unique(total_iv),
    hhi = hhi(count_distr),
    count_distr_max = max(count_distr),
    count_distr_min = min(count_distr),
    has_missing = any(bin == "missing"),
    has_special_values = any(is_special_values)
    )

  dbiv <- mutate(
    dbiv,
    iv_lbl  = iv_label(iv),
    hhi_lbl = hhi_label(hhi)
    )

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
#' bins <- scorecard::woebin(germancredit, y = "creditability", x = "credit.amount")
#'
#' variable <- head(germancredit$credit.amount, 10)
#' bin <- bins$credit.amount
#'
#' woebin_ply_min(variable, bin)
#'
#' woebin_ply_min(variable, bin,  value = "badprob")
#'
#' @export
woebin_ply_min <- function(variable, bin, value = "woe") {

  namevar <- bin[[1]][[1]]

  daux <- purrr::set_names(
    data.frame(variable),
    namevar
  )

  daux2 <- scorecard::woebin_ply(daux, list(bin))

  daux <- dplyr::bind_cols(daux, daux2)

  if(value == "woe") return(daux[[2]])

  daux <- dplyr::left_join(
    daux,
    dplyr::select(bin, "woe", value),
    by = purrr::set_names("woe", paste0(namevar, "_woe"))
  )

  daux[[3]]

}

#'
#' Alternative plot for woe_bin
#'
#' @param bins An output from `scorecard::woebin` function.
#'
#' @examples
#'
#' data(germancredit, package = "scorecard")
#'
#' bins <- scorecard::woebin(
#'  germancredit,
#'  y = "creditability",
#'  x = c("credit.amount", "housing", "duration.in.month"),
#'  method = "tree"
#' )
#'
#' scorecard::woebin_plot(bins)
#' woebin_plot2(bins)
#
# library(patchwork)
#
# purrr::reduce(woebin_plot2(bins), `+`) +
#   plot_layout(guides = "collect")
#'
#' @importFrom dplyr bind_rows group_by mutate summarize n
#' @importFrom purrr map
#'
#' @export
woebin_plot2 <- function(bins) {

  purrr::map(bins, bin_plot)

}

#'
#' Plot one bin from
#'
#' @param bin A item from `scorecard::woebin` function.
#' @param texts Logical, show text of values (percentages, bad rates).
#'
#' @examples
#'
#'
#' data(germancredit, package = "scorecard")
#'
#' bins <- scorecard::woebin(
#'   germancredit,
#'   y = "creditability",
#'   x = c("credit.amount", "housing", "duration.in.month"),
#'   method = "tree"
#'   )
#'
#' bin_plot(bins[[2]])
#'
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 ggplot aes
#'
#' @export
bin_plot <- function(bin = bins[[3]], texts = TRUE){
                             # , text.lbl.size = 4,
                          # bar.col = "#4f2d7f", line.col = "#e22d36", line.col2 = "#f2ad4b"){

  # bin <- tibble::as_tibble(bin)
  bin

  # values
  br   <- dplyr::pull(dplyr::summarise(bin, sum(count_distr*badprob)))
  iv   <- unique(dplyr::pull(bin, total_iv))
  var  <- unique(dplyr::pull(bin, variable))
  lvls <- dplyr::pull(bin, bin)

  # dfs
  bin <- dplyr::mutate(bin, bin = factor(bin, levels = lvls))
  bin <- dplyr::mutate(bin, type = "bad rate")

  bing <- tidyr::gather(dplyr::select(bin, bin, good, bad), label, value, -bin)
  bing <- dplyr::mutate(
      bing,
      value = value/sum(value),
      type = "distribution",
      bin = factor(bin, levels = lvls)
      )


  gg <- ggplot2::ggplot() +
    # tasa
    ggplot2::geom_hline(ggplot2::aes(yintercept = br)) +
    # columns
    ggplot2::geom_col(ggplot2::aes(x = bin, y = value, fill = label), data = bing) +
    # line
    ggplot2::geom_line(ggplot2::aes(x = bin, y = badprob, group = 1), data = bin) +
    # extras
    ggplot2::theme()

  gg <- gg +
    ggplot2::labs(
      title = paste(var)
    )

  gg

  # gg + ggplot2::facet_wrap(ggplot2::vars(type))
  #
  #   geom_text(aes(y = count_distr, label = comma(count)), vjust = 1.5,
  #             size = text.lbl.size, color = "gray70", family = "Segoe UI") +
  #   # tasa
  #   geom_text(aes(x = bin, y = br, label = percent(br)), vjust = -1, hjust = 2.5, color = line.col2,
  #             family = "Segoe UI", data = head(w, 1)) +
  #   # lines
  #   geom_line(aes(y = badprob), group = 1, size = 2, color = line.col) +
  #   geom_point(aes(y = badprob), size = 4, fill = "white", shape = 21, color = line.col) +
  #   geom_text(aes(y = badprob, label = percent(badprob)), size = text.lbl.size,
  #             vjust = -1.5, color = "gray50", family = "Segoe UI") +
  #   scale_y_continuous(labels = percent, limits = c(0, ylim_sup), breaks = 0:10/5) +
  #   theme_minimal(base_size = 15, base_family = "Segoe UI") +
  #   labs(
  #     y = "Porcentaje de casos/Tasa de Incumplimiento",
  #     x = NULL,
  #     title = str_c(var, " - IV: ", round(iv, 2))
  #   )

}

#'
#' Create a tidy data frame from a correlation (cor) output
#'
#' @param dat A data frame to apply `scorecard::woebin_ply` and calculate correlations.
#' @param bins An output from `scorecard::woebin` to create woe variables
#'
#' @examples
#'
#' data(germancredit, package = "scorecard")
#'
#' vars <- c("creditability", "duration.in.month", "credit.history",
#'           "purpose", "status.of.existing.checking.account", "property")
#'
#' dat <- germancredit[, vars]
#'
#' bins <- scorecard::woebin(dat, y = "creditability", stop_limit = 0.0000001)
#'
#' woebin_cor(dat, bins)
#'
#' datcor <- woebin_cor(dat, bins)
#'
#' library(dplyr)
#'
#' cor_limit <- 0.15
#'
#' datcor %>%
#'   filter(variable_1 != variable_2) %>%
#'   mutate(
#'     cor_conflict = ifelse(abs(cor) > cor_limit, TRUE, FALSE),
#'     variable_to_remove = ifelse(
#'       cor_conflict,
#'       ifelse(iv_variable_1 > iv_variable_2, variable_2, variable_1),
#'       NA
#'    )
#' )
#'
#' @importFrom stringr str_remove
#'
#' @export
#'
woebin_cor <- function(dat, bins) {

  woebinsum <- woebin_summary(bins)
  woebinsumiv <- dplyr::select(woebinsum, c("variable", "iv"))

  datwoe <- scorecard::woebin_ply(dat, bins)
  datwoe <- dplyr::select(datwoe, paste0(names(bins), "_woe"))

  datcor <- cor_tidy(cor(datwoe), upper = FALSE)
  datcor[[1]] <- stringr::str_remove(datcor[[1]], "_woe$")
  datcor[[2]] <- stringr::str_remove(datcor[[2]], "_woe$")

  datcor <- dplyr::left_join(datcor, woebinsumiv, by = c("variable_1" = "variable"))
  datcor <- dplyr::left_join(datcor, woebinsumiv, by = c("variable_2" = "variable"), suffix = c("_variable_1", "_variable_2"))

  datcor

}

#'
#' Create a tidy data frame from a correlation (cor) output
#'
#' @param datcor A matrix from cor function
#' @param upper Logical. Remove repeated correlations
#'
#' @examples
#'
#' data(germancredit, package = "scorecard")
#'
#' vars <- c("creditability", "duration.in.month", "credit.history", "age.in.years", "purpose")
#'
#' dat <- germancredit[, vars]
#'
#' bins <- scorecard::woebin(dat, y = "creditability")
#'
#' datwoe <- scorecard::woebin_ply(dat, bins)
#'
#' datwoe <- datwoe[, -c("creditability")]
#'
#' datcor <- cor(datwoe)
#'
#' datcor
#'
#' cor_tidy(datcor)
#'
#' cor_tidy(datcor, FALSE)
#'
#' @importFrom tidyr gather
#' @importFrom tibble rownames_to_column
#' @importFrom stats cor
#'
#' @export
cor_tidy <- function(datcor, upper = TRUE) {

  class(datcor)
  stopifnot(class(datcor) %in% "matrix")

  daux <- as.data.frame(datcor)
  daux <- tibble::rownames_to_column(daux, var = "variable_1")
  daux <- tidyr::gather(daux, variable_2, cor, -variable_1)

  # daux <- dplyr::mutate_if(daux, is.character, factor, levels = rownames(datcor))

  if(upper) return(dplyr::filter(daux, as.numeric(variable_1) > as.numeric(variable_2)))

  daux

}




