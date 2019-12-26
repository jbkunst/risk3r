#'
#' Get scorecard's woe_bin summary
#'
#' @param woebin_obj An output from `scorecard::woebin` function.
#'
#' @examples
#'
#' data(germancredit, package = "scorecard")
#'
#' woebin_obj <- scorecard::woebin(
#'  germancredit,
#'  y = "creditability",
#'  x = c("credit.amount", "housing", "duration.in.month"),
#'  method = "tree"
#' )
#'
#' woebin_summary(woebin_obj)
#'
#' @importFrom dplyr bind_rows group_by mutate summarize n
#' @importFrom purrr reduce
#' @importFrom rlang enquo enexpr
#'
#' @export
woebin_summary <- function(woebin_obj) {

  # variable    <- rlang::enquo(variable)
  # total_iv    <- rlang::enquo(total_iv)
  # count_distr <- rlang::enquo(count_distr)

  dbiv <- reduce(woebin_obj, bind_rows)

  dbiv <- dplyr::group_by(dbiv, variable)

  dbiv <- dplyr::summarize(
    dbiv,
    n_cat = dplyr::n(),
    iv = unique(total_iv),
    hhi = hhi(count_distr),
    max_concentration = max(count_distr),
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


