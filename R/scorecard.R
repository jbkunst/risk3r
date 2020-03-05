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
#'  x = c("credit.amount", "housing", "duration.in.month", "purpose"),
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

  stopifnot(is.data.frame(bin))

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

  woebinsum   <- woebin_summary(bins)
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


#'
#' Create a breaklist argument unsing ctree method
#'
#' @param dt A data frame using to create the bins
#' @param y Name of y variable.
#' @param ctrl A partykit::ctree_control instance
#' @param save_trees A logical indicating to return the trees objects or not
#'
#' @examples
#'
#' \dontrun{
#'
#' data(germancredit, package = "scorecard")
#'
#' brks <- ctree_break_list(germancredit, "creditability", ctrl = partykit::ctree_control())
#'
#' bins2 <- scorecard::woebin(germancredit, "creditability", breaks_list = brks)
#'
#' bins2$duration.in.month
#'
#' woebin_summary(bins2)
#'
#' bins1 <- scorecard::woebin(germancredit, "creditability")
#'
#' bins1$duration.in.month
#'
#' woebin_summary(bins1)
#'
#' library(dplyr)
#'
#' full_join(
#'   woebin_summary(bins1) %>% select(variable, iv),
#'   woebin_summary(bins2) %>% select(variable, iv),
#'   by = "variable"
#'   ) %>%
#'   mutate( iv.x  > iv.y)
#'
#' }
#'
#' @importFrom stats as.formula
#'
#' @export
ctree_break_list <- function(dt, y, ctrl = partykit::ctree_control(), save_trees = FALSE) {

  # dt <- tibble::as_tibble(germancredit)
  # y  <- "creditability"
  # ctrl = partykit::ctree_control()
  # save_trees <- FALSE

  vars <- setdiff(names(dt), y)

  predparty <- utils::getFromNamespace("predict.party", "partykit")

  brks <- purrr::map(vars, function(var = "duration.in.month"){

    message(var)

    d <- dplyr::select(dt, var, y)
    d <- dplyr::mutate_if(d, is.character, as.factor)
    d <- dplyr::filter(d, stats::complete.cases(d))

    tri <- partykit::ctree(
      as.formula(paste(y, var, sep = " ~ ")),
      data = d,
      control = ctrl
    )

    d <- dplyr::mutate(d, node = predparty(object = tri, newdata = d, type = "node"))

    if(is.factor(dplyr::pull(d, var))) {

      out <- dplyr::select(d, -y)
      out <- dplyr::distinct(out)
      out <- dplyr::group_by(out, node)
      out <- dplyr::summarise_all(out, paste0, collapse = "%,%")
      out <- dplyr::pull(out, var)

    } else {

      out <- dplyr::select(d, -y)
      out <- dplyr::group_by(out, node)
      out <- dplyr::summarise_all(out, max)
      out <- dplyr::pull(out, var)
      out <- out[-length(out)]
      out <- c(-Inf, out, Inf)
      out <- unique(out)

    }

    if(save_trees) {
      outf <- list("breaks" = out, "tree" = tri)
    } else {
      outf <- list("breaks" = out)
    }

    outf

  })

  output <- purrr::map(brks, "breaks")
  output <- purrr::set_names(output, vars)

  if(save_trees) {

    trees <- purrr::map(brks, "tree")
    trees <- purrr::set_names(trees, vars)

    attr(output, "trees") <- trees

  }

  output

}

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
