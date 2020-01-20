#'
#' Plot one bin from
#'
#' @param bin A item from `scorecard::woebin` function.
#' @param texts Logical, show text of values (percentages, bad rates).
#'
#' @examples
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
#' gg <- bin_plot(bins[[2]])
#'
#' gg
#'
#'
#' require(ggplot2)
#' ggplot2::update_geom_defaults("point", list(colour = "black", fill = "white", size = 2, stroke = 2))
#' ggplot2::update_geom_defaults("point", list(colour = "black", fill = "white", size = 5, stroke = 5))
#'
#'
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 ggplot aes
#' @importFrom scales comma percent
#' @importFrom stringr  str_c
#' @importFrom utils head
#'
#' @export
bin_plot <- function(bin = bins[[3]], texts = TRUE){

  # library(extrafont)
  # ggplot2::theme_set(ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom"))

  # values
  iv   <- unique(dplyr::pull(bin, total_iv))
  var  <- unique(dplyr::pull(bin, variable))
  lvls <- dplyr::pull(bin, bin)

  # dfs
  bin <- dplyr::mutate(
    bin,
    bin = factor(bin, levels = lvls),
    total_badprob = sum(count_distr*badprob)
    )

  bing <- tidyr::gather(dplyr::select(bin, bin, good, bad), label, value, -bin)
  bing <- dplyr::mutate(bing, count_distr = value/sum(value))

  df <- bind_rows(
    bing %>% dplyr::select(bin, count_distr, label)      %>% mutate(type = "count_distr"),
    bin  %>% dplyr::select(bin, badprob)                 %>% mutate(type = "badprob", type2 = "bin"),
    bin  %>% dplyr::select(bin, badprob = total_badprob) %>% mutate(type = "badprob", type2 = "total")
  )

  df <- dplyr::mutate(df, bin = factor(bin, levels = lvls))

  df

  gg <- ggplot2::ggplot() +

    # ggplot2::geom_line(
    #   ggplot2::aes(bin, badprob, group = type2, color = type2),
    #   data = dplyr::filter(df, type == "badprob", type2 == "total")
    #   ) +

    ggplot2::geom_hline(
      ggplot2::aes(yintercept = badprob, group = type2, color = type2),
      data = dplyr::filter(df, type == "badprob", type2 == "total")
      ) +

    ggplot2::geom_col(
      ggplot2::aes(bin,  count_distr, fill = label),
      data = df %>% dplyr::filter(type == "count_distr")
      ) +

    ggplot2::geom_line(
      ggplot2::aes(bin, badprob, group = type2, color = type2),
      data = dplyr::filter(df, type == "badprob", type2 == "bin")
      ) +

    ggplot2::geom_point(
      ggplot2::aes(bin, badprob, group = type2, color = type2),
      data = dplyr::filter(df, type == "badprob", type2 == "bin"),
      shape = 21
      ) +

    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, NA), breaks = 0:10/10) +

    ggplot2::theme()

  gg <- gg +
    ggplot2::labs(
      title = paste(var),
      y = NULL,
      x = NULL,
      fill = NULL,
      colour = NULL,
      subtitle = stringr::str_c("IV: ", round(iv, 2))
    )

  gg

  ggplot2::last_plot() + ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_y")

  gg +
    ggplot2::scale_color_manual(values = c(bin = "darkred", total = "darkcyan")) +
    ggplot2::scale_fill_manual(values = c(good = "gray80", bad = "darkorange4")) +
    ggplot2::theme()

  ggplot2::last_plot() + ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_y")

  gg +
    ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_y") +
    modrpley::scale_fill_ripley_d() +
    modrpley::scale_color_ripley_d(direction = -1) +
    ggplot2::theme()


  if(text) {

  }

  gg <- ggplot2::ggplot(bin) +
    # tasa
    ggplot2::geom_hline(ggplot2::aes(yintercept = br), color = line.col2) +
    # columns
    ggplot2::geom_col(ggplot2::aes(x = bin, y = value, fill = label), data = bing, width = 0.5) +
    # line
    ggplot2::geom_line(ggplot2::aes(x = bin, y = badprob, group = 1), size = 1.2, color = line.col2) +
    # point
    ggplot2::geom_point(ggplot2::aes(x = bin, y = badprob), size = 3, fill = "white", shape = 21, color = line.col) +
    # extras
    ggplot2::theme() +

  # ggplot2::theme_minimal(base_size = 15, base_family = "Segoe UI")



  gg

  gg <- gg +
    ggplot2::geom_text(
      ggplot2::aes(x = bin, y = count_distr, label = scales::comma(count)),
      vjust = 1.5, size = text.lbl.size, color = "gray70",
      family = ggplot2::theme_get()$text$family
    ) +
    # lines
    ggplot2::geom_text(
      aes(x = bin, y = badprob, label = scales::percent(badprob)),
      size = text.lbl.size, color = "gray70",
      family = ggplot2::theme_get()$text$family,
      vjust = 1.5, hjust = -.5
    ) +
    # tasa
    ggplot2::geom_text(
      ggplot2::aes(x = bin, y =  total_badprob, label = scales::percent( total_badprob)),
      vjust = -1, hjust = 2.5, color = line.col2, family = "Segoe UI", size = text.lbl.size,
      data = head(bin, 1)
    )

  # gg + modrpley::scale_fill_ripley_d()

  gg

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

