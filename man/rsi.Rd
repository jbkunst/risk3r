% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variables.R
\name{rsi}
\alias{rsi}
\title{Relative strength index}
\usage{
rsi(x)
}
\arguments{
\item{x}{A numeric vector to calculate the RSI}
}
\description{
Relative strength index
}
\examples{

set.seed(123)

x <- round(cumsum(rnorm(24))*100, 0)
x <- abs(x)
x

plot(x, type = "l")

rsi(x)

# just test
rsi(rev(x))


}
