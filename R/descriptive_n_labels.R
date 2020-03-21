#'
#' Get the max proportion
#'
#' @param x The value to get the maximun proportion.
#'
#' @examples
#'
#' x <- sample(letters[1:3], prob = 1:3, size = 10000, replace = TRUE)
#'
#' concentration_max(x)
#'
#' @export
concentration_max  <- function(x) {

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


#'
#' Get labels for Information Values
#'
#' @param x A numeric vector
#'
#' @examples
#'
#' iv_label(c(0.2, 2))
#'
#' @export
iv_label <- function(x) {
  cut(
    x,
    include.lowest = TRUE,
    breaks = c(0, 0.02, 0.1, 0.3, 0.5, Inf),
    labels = c("unpredictive", "weak", "medium", "strong", "suspicious")
  )
}



#'
#' Get labels for PSIs
#'
#' @param x A numeric vector
#'
#' @examples
#'
#' psi_label(c(0.2, 2))
#'
#' @export
psi_label <- function(x) {
  cut(
    x,
    c(0, 0.1, 0.25, Inf),
    include.lowest = TRUE,
    # c("Cambio insignificante", "Algún cambio menor", "Gran cambio en población")
    labels = c("insignificant change", "some minor change", "major shift in population")
  )
}
