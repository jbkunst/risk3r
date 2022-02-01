#'
#' Get labels for PSIs
#'
#' @param x A numeric vector
#'
#' @examples
#'
#' psi_label(c(0.2, 2))
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

#'
#' Get labels for Information Values
#'
#' @param x A numeric vector
#'
#' @examples
#'
#' N <- 10000
#'
#' predicted <- runif(N)
#'
#' actual <- rbinom(N, size = 1, prob = predicted)
#'
#' iv_label(information_value(actual, predicted))
#'
#' predicted <- runif(N)
#'
#' iv_label(information_value(actual, predicted))
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
#' Get labels for VIF values
#'
#' @param x A numeric vector
#'
#' @examples
#'
#' set.seed(123)
#'
#' vifs <- sample(1:10)
#'
#' vif_label(vifs)
#'
#' @export
vif_label <- function(x) {
  cut(
    x,
    right = FALSE,
    breaks = c(-Inf, 5, 10, Inf),
    labels = c("low (<5)", "moderate (<10)","high (>= 10)")
  )
}
