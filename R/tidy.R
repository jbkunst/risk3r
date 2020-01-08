#'
#' Create a tidy data frame from a correlation (cor) output
#'
#' @param datcor A matrix from cor function
#' @param upper Ligical. Remove repeated correlations
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

  daux <- dplyr::mutate_if(daux, is.character, factor, levels = rownames(datcor))

  if(upper) return(dplyr::filter(daux, as.numeric(variable_1) > as.numeric(variable_2)))

  daux
}
