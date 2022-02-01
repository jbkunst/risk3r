validate_actual_predicted_num <- function(actual, predicted) {
  stopifnot(
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
#' N <- 10000
#'
#' predicted <- runif(N)
#'
#' actual <- rbinom(N, size = 1, prob = predicted)
#'
#' ks(actual, predicted)
#'
#' predicted[sample(c(TRUE, FALSE), size = N, prob = c(1, 99), replace = TRUE)] <- NA
#'
#' ks(actual, predicted)
#' @importFrom stats ks.test na.omit
#' @export
ks <- function(actual, predicted) {
  validate_actual_predicted_num(actual, predicted)

  if (length(unique(actual)) != 2) {
    warning("Not 2 distinct values in 'actual' vector, returning NA")
    return(NA)
  }

  if (any(is.na(predicted))) {
    message(sum(is.na(predicted)), " of ", length(predicted), " 'predicted' values are NAs, they will be ignorated")
  }

  actual_values <- unique(actual)

  dist1 <- na.omit(predicted[actual == actual_values[1]])
  dist2 <- na.omit(predicted[actual == actual_values[2]])

  # print(head(dist1))
  # print(head(dist2))

  if (length(dist1) == 0 | length(dist2) == 0) {
    message("Not enough data for one of the distributions")
    return(NA)
  }

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
#' N <- 10000
#'
#' predicted <- runif(N)
#'
#' actual <- rbinom(N, size = 1, prob = predicted)
#'
#' metrics(actual, predicted)
#'
#' predicted[sample(c(TRUE, FALSE), size = N, prob = c(1, 99), replace = TRUE)] <- NA
#'
#' metrics(actual, predicted)
#' @export
metrics <- function(actual, predicted) {
  if (is.factor(actual)) actual <- as.numeric(actual) - 1

  tibble::tibble(
    ks = ks(actual, predicted),
    auc = Metrics::auc(actual, predicted),
    iv = information_value(actual, predicted),
    gini = 2 * .data$auc - 1
  )
}

#'
#' Gains
#'
#' @param actual A binary vector
#' @param predicted A numeric vector containing scores or probabilities
#' @param percents Values to calculate the gain
#'
#' @examples
#'
#' N <- 10000
#'
#' predicted <- runif(N)
#'
#' actual <- rbinom(N, size = 1, prob = predicted)
#'
#' gain(actual, predicted)
#'
#' gain(actual, predicted, c(0.5, 1))
#' @importFrom stats ecdf quantile
#' @export
gain <- function(actual, predicted, percents = c(0.10, 0.20, 0.30, 0.40, 0.50)) {

  validate_actual_predicted_num(actual, predicted)

  g <- ecdf(predicted[actual == 0])(quantile(predicted, percents))

  g
}

#'
#' Calculate Information Value
#'
#' @param actual A binary vector
#' @param predicted A vector: character, numeric or containing scores or probabilities
#' @return The KS statistic
#' @examples
#'
#' N <- 10000
#'
#' predicted <- runif(N)
#'
#' actual <- rbinom(N, size = 1, prob = predicted)
#'
#' information_value(actual, predicted)
#'
#' predicted[sample(c(TRUE, FALSE), size = N, prob = c(1, 99), replace = TRUE)] <- NA
#'
#' information_value(actual, predicted)
#' @export
information_value <- function(actual, predicted) {
  validate_actual_predicted_num(actual, predicted)

  if (length(unique(actual)) == 1) {
    return(NA_real_)
  }

  df <- data.frame(y = actual, x = predicted)

  bin <- quiet(scorecard::woebin(df, "y", "x"))

  out <- bin[["x"]][["total_iv"]][[1]]

  out
}
