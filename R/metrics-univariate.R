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
#' rsi(x) + rsi(rev(x))
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

#'
#' Get the max proportion
#'
#' @param x The value to get the maximun proportion.
#'
#' @examples
#'
#' x <- sample(letters[1:3], prob = 1:3, size = 10000, replace = TRUE)
#'
#' count_distr_max(x)
#'
#' @export
count_distr_max  <- function(x) {

  sort(prop.table(table(addNA(x))), decreasing = TRUE)[[1]]

}

#'
#' Herfindahl-Hirschman Index
#'
#' @param x A vector to obtain the HHI.
#' @examples
#'
#' x <- sample(LETTERS[1:10], size = 1000, replace = TRUE, prob = log(1:10))
#'
#' hhi(x)
#'
#' plot(table(x), main = hhi_label(hhi(x)))
#'
#' x <- sample(LETTERS[1:5], size = 1000, replace = TRUE, prob = exp(1:5))
#'
#' hhi(x)
#'
#' plot(table(x), main = hhi_label(hhi(x)))
#'
#' @export
hhi <- function(x) {

  xout <- prop.table(table(x, useNA = "always"))

  xout <- sum(xout*xout)

  xout

}

#'
#' Get labels for HHI. indexes
#'
#' @param x A numeric vector
#'
#' @examples
#'
#' hhi_label(c(0.2, 2))
#'
#' @export
hhi_label <- function(x) {
  cut(
    x,
    breaks = c(0, 0.01, 0.15, 0.25, Inf),
    include.lowest = TRUE,
    # c("Altamente no concentrado", "No concentrado", "muy concentrado", "Altamente concentrado"),
    labels = c("highly diverse","unconcentrated","moderate concentration", "high concentration"))
}
