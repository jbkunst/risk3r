validate_actual_predicted_num <- function(actual, predicted){

  stopifnot(
    length(unique(actual)) == 2,
    length(predicted) == length(actual),
    is.numeric(predicted)
  )

}

#'
#' Calculate Kolmogorov-Smirnov statistic
#'
#' More metrics in the Metrics package https://github.com/mfrasco/Metrics
#'
#' @param actual A binary vector
#' @param predicted A numeric vector containing scores or probabilities
#' @return The KS statistic
#' @examples
#'
#' N <- 1000
#' actual <- rbinom(N, size = 1, prob = 0.3)
#' predicted <- runif(N)
#'
#' ks(actual, predicted)
#'
#' @importFrom stats ks.test
#' @export
ks <- function(actual, predicted){

  validate_actual_predicted_num(actual, predicted)

  actual_values <- unique(actual)

  dist1 <- predicted[actual == actual_values[1]]
  dist2 <- predicted[actual == actual_values[2]]

  value <- as.numeric(suppressWarnings(ks.test(dist1, dist2)[["statistic"]]))

  return(value)

}

#' Metrics
#'
#' Metrics from Metrics package https://github.com/mfrasco/Metrics
#'
#' @param actual A binary vector
#' @param predicted A numeric vector containing scores or probabilities
#'
#' @return A data frame with usual and opinionated metrics
#' @examples
#'
#' N <- 1000
#' actual <- rbinom(N, size = 1, prob = 0.3)
#' predicted <- runif(N)
#'
#' metrics(actual, predicted)
#'
#' @export
metrics <- function(actual, predicted){

  data.frame(
    ks = ks(actual, predicted),
    auc= Metrics::auc(actual, predicted)
  )

}

#' Gains
#'
#' @param actual A binary vector
#' @param predicted A numeric vector containing scores or probabilities
#' @param percents Values to calculate the gain
#'
#' @return The Gini Coefficient
#'
#' @examples
#'
#' N <- 1000
#' actual <- rbinom(N, size = 1, prob = 0.3)
#' predicted <- runif(N)
#'
#' gain(actual, predicted)
#'
#' @importFrom stats ecdf quantile
#' @export
gain <- function(actual, predicted, percents = c(0.10, 0.20, 0.30, 0.40, 0.50)){

  validate_actual_predicted_num(actual, predicted)

  g <- ecdf(predicted[actual == 0])(quantile(predicted, percents))

  g
}
