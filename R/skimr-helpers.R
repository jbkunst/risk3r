#' As list method for skim_df object
#'
#' @param x A skim_df object
#' @param ... Optional arguments (non used yet).
#' @export
as.list.skim_df <- function(x, ...) {
  tsummary <- as.data.frame(summary(x))
  tsummary <- tibble::as_tibble(tsummary)
  tsummary <- dplyr::select(tsummary, 1, 3)
  tsummary <- setNames(tsummary, c("", ""))

  tdetails <- skimr::partition(x)
  tdetails <- purrr::map(tdetails, tibble::as_tibble)

  out <- c(list(summary = tsummary), tdetails)

  out
}
