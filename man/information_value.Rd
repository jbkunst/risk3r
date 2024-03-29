% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-predictive.R
\name{information_value}
\alias{information_value}
\title{Calculate Information Value}
\usage{
information_value(actual, predicted)
}
\arguments{
\item{actual}{A binary vector}

\item{predicted}{A vector: character, numeric or containing scores or probabilities}
}
\value{
The KS statistic
}
\description{
Calculate Information Value
}
\examples{

N <- 10000

predicted <- runif(N)

actual <- rbinom(N, size = 1, prob = predicted)

information_value(actual, predicted)

predicted[sample(c(TRUE, FALSE), size = N, prob = c(1, 99), replace = TRUE)] <- NA

information_value(actual, predicted)
}
