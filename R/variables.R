#'
#' Relative strength index
#'
#' @param x A numeric vector to calculate the RSI
#'
#' @examples
#'
#' set.seed(123)
#'
#' x <- round(cumsum(rnorm(24))*100, 0)
#' x <- abs(x)
#' x
#'
#' plot(x, type = "l")
#'
#' rsi(x)
#'
#' # just test
#' rsi(rev(x))
#'
#'
#' @export
rsi <- function(x) {

  stopifnot(is.numeric(x), all(!is.na(x)))

  d <- diff(x)

  up <- ifelse(d < 0, 0, d)
  dn <- ifelse(d > 0, 0, -d)

  100 * mean(up, na.rm = TRUE) / ( mean(up, na.rm = TRUE) + mean(dn, na.rm = TRUE))

}

#'
#' Number of up/growths
#'
#' @param x A numeric vector to calculate the number of growths
#'
#' @examples
#'
#' set.seed(123)
#'
#' x <- round(cumsum(rnorm(24))*100, 0)
#' x <- abs(x)
#' x
#'
#' plot(x, type = "l")
#'
#' n_growths(x)
#'
#' n_growths(c(1, 2, 0))
#'
#' n_growths(c(0))
#'
#'
#' @export
n_growths <- function(x) {

  stopifnot(is.numeric(x), all(!is.na(x)))

  sum(diff(x)>0)

}
