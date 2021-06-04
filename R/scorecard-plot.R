#' #'
#' #' Plot one bin from
#' #'
#' #' @param bin A item from `scorecard::woebin` function.
#' #' @param labels Logical, show text of values (percentages, bad rates).
#' #' @param labels_geom Function to use when add labels, default \code{ggplot2::geom_label}. Can
#' #'   be \code{ggrepel::geom_label_repel}.
#' #'
#' #'
#' #' @examples
#' #'
#' #' data(germancredit, package = "scorecard")
#' #'
#' #' bins <- scorecard::woebin(
#' #'   germancredit,
#' #'   y = "creditability",
#' #'   x = c("credit.amount", "housing", "duration.in.month"),
#' #'   method = "tree"
#' #'   )
#' #'
#' #' bin_plot(bins[[3]])
#' #'
#' #' scorecard::woebin_plot(bins[3])
#' #'
#' #' gg <- bin_plot(bins[[3]])
#' #'
#' #' gg
#' #'
#' #' gg + ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_y")
#' #'
#' #'
#' #' \dontrun{
#' #' gg <- bin_plot(bins[[3]], labels_geom = ggrepel::geom_label_repel)
#' #'
#' #' gg
#' #'
#' #' require(ggplot2)
#' #' ggplot2::update_geom_defaults("point", list(colour = "black", fill = "white", size = 2, stroke = 2))
#' #' ggplot2::update_geom_defaults("point", list(colour = "black", fill = "white", size = 5, stroke = 5))
#' #'
#' #' gg
#' #'
#' #' gg + ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_y")
#' #'
#' #' }
#' #'
#' #'
#' #' @importFrom forcats fct_inorder
#' #' @importFrom ggplot2 ggplot aes
#' #' @importFrom scales comma percent
#' #' @importFrom stringr str_c
#' #'
#' #' @export
#' bin_plot <- function(bin = bins[[3]], labels = TRUE, labels_geom = ggplot2::geom_label){
#'
#'   # library(extrafont)
#'   # ggplot2::theme_set(ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom"))
#'
#'   # values
#'   bin <- tibble::as_tibble(bin)
#'   iv   <- unique(dplyr::pull(bin, total_iv))
#'   var  <- unique(dplyr::pull(bin, variable))
#'   lvls <- dplyr::pull(bin, bin)
#'
#'   # dfs
#'   bin <- dplyr::mutate(
#'     bin,
#'     bin = factor(bin, levels = lvls),
#'     total_badprob = sum(count_distr*posprob)
#'     )
#'
#'   bing <- tidyr::gather(dplyr::select(bin, bin, pos, neg), label, value, -bin)
#'   bing <- dplyr::mutate(bing, count_distr = value/sum(value))
#'
#'   df <- dplyr::bind_rows(
#'     dplyr::mutate(dplyr::select(bing, bin, value, count_distr, label), type = "count_distr"),
#'     dplyr::mutate(dplyr::select(bin , bin, posprob)                  , type = "badprob", type2 = "bin"),
#'     dplyr::mutate(dplyr::select(bin , bin, posprob = total_badprob)  , type = "badprob", type2 = "total")
#'   )
#'
#'   df <- dplyr::mutate(
#'     df,
#'     bin               = factor(bin, levels = lvls),
#'     value_label       = scales::comma(value),
#'     count_distr_label = scales::percent(count_distr),
#'     badprob_label     = scales::percent(posprob)
#'     )
#'
#'   df
#'
#'   gg <- ggplot2::ggplot() +
#'
#'     # ggplot2::geom_line(
#'     #   ggplot2::aes(bin, badprob, group = type2, color = type2),
#'     #   data = dplyr::filter(df, type == "badprob", type2 == "total")
#'     #   ) +
#'
#'     ggplot2::geom_hline(
#'       ggplot2::aes(yintercept = posprob, group = type2, color = type2),
#'       data = dplyr::filter(df, type == "posprob", type2 == "total")
#'       ) +
#'
#'     ggplot2::geom_col(
#'       ggplot2::aes(bin,  count_distr, fill = label),
#'       data = dplyr::filter(df, type == "count_distr")
#'       ) +
#'
#'     ggplot2::geom_line(
#'       ggplot2::aes(bin, posprob, group = type2, color = type2),
#'       data = dplyr::filter(df, type == "posprob", type2 == "bin")
#'       ) +
#'
#'
#'     ggplot2::geom_point(
#'       ggplot2::aes(bin, posprob, group = type2, color = type2),
#'       data = dplyr::filter(df, type == "posprob", type2 == "bin"),
#'       shape = 21
#'       ) +
#'
#'     ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, NA), breaks = 0:10/10)
#'
#'   gg <- gg +
#'     ggplot2::labs(
#'       title = paste(var),
#'       y = NULL,
#'       x = NULL,
#'       fill = NULL,
#'       colour = NULL,
#'       subtitle = stringr::str_c("IV: ", round(iv, 2))
#'     )
#'
#'   if(labels) {
#'
#'     df2 <- dplyr::bind_rows(
#'       dplyr::mutate(dplyr::select(bin, bin, value = count_distr, count), value_label = scales::comma(count), type = "count_distr"),
#'       dplyr::mutate(dplyr::select(bin, bin, value = posprob), value_label = scales::percent(value), type = "badprob")
#'     )
#'
#'     gg <- gg +
#'       labels_geom(
#'         ggplot2::aes(bin, value, label = value_label),
#'         data = df2
#'       )
#'
#'   }
#'
#'   # ggplot2::last_plot() + ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_y")
#'
#'   # gg +
#'   #   ggplot2::scale_color_manual(values = c(bin = "darkred", total = "darkcyan")) +
#'   #   ggplot2::scale_fill_manual(values = c(good = "gray80", bad = "darkorange4")) +
#'   #   ggplot2::theme()
#'
#'   # ggplot2::last_plot() + ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_y")
#'
#'   # gg +
#'   #   ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_y") +
#'   #   modrpley::scale_fill_ripley_d() +
#'   #   modrpley::scale_color_ripley_d(direction = -1) +
#'   #   ggplot2::theme()
#'
#'
#'   gg
#'
#' }
#'
#' #'
#' #' Alternative plot for woe_bin
#' #'
#' #' @param bins An output from `scorecard::woebin` function.
#' #'
#' #' @examples
#' #'
#' #' data(germancredit, package = "scorecard")
#' #'
#' #' bins <- scorecard::woebin(
#' #'  germancredit,
#' #'  y = "creditability",
#' #'  x = c("credit.amount", "housing", "duration.in.month"),
#' #'  method = "tree"
#' #' )
#' #'
#' #' scorecard::woebin_plot(bins)
#' #' woebin_plot2(bins)
#' #
#' # library(patchwork)
#' #
#' # purrr::reduce(woebin_plot2(bins), `+`) +
#' #   plot_layout(guides = "collect")
#' #'
#' #' @importFrom dplyr bind_rows group_by mutate summarize n
#' #' @importFrom purrr map
#' #'
#' #' @export
#' woebin_plot2 <- function(bins) {
#'
#'   purrr::map(bins, bin_plot)
#'
#' }
#'
