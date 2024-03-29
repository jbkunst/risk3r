% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-scorecard.R
\name{woebin_ctree}
\alias{woebin_ctree}
\title{Interface scorecard::woebin for partykiy::ctree}
\usage{
woebin_ctree(
  y,
  x,
  namevar = "variable",
  count_distr_limit = 0.05,
  control = partykit::ctree_control()
)
}
\arguments{
\item{y}{A vector of response. Usually 0-1}

\item{x}{A predictive variable}

\item{namevar}{a character element}

\item{count_distr_limit}{The minimum count distribution percentage. Accepted range: 0.01-0.2; Defaults to 0.05.}

\item{control}{a ctree::ctree_control list element}
}
\description{
Interface scorecard::woebin for partykiy::ctree
}
\examples{

if(FALSE){

data(germancredit, package = "scorecard")

y <- germancredit$creditability

x <- germancredit$duration.in.month

woebin_ctree(y, x, "duration", count_distr_limit = 0.05)

woebin_ctree(y, x, "duration", count_distr_limit = 0.2)

woebin_ctree(
  y,
  x,
  "duration",
  count_distr_limit = 0.05,
  control = partykit::ctree_control(alpha = 0.5)
)

x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA
woebin_ctree(y, x, "duration")

x <- germancredit$purpose
woebin_ctree(y, x, "purpose")

x[sample(c(TRUE, FALSE), size = length(x), prob = c(1, 99), replace = TRUE)] <- NA
woebin_ctree(y, x, "purpose_with_na")

}

}
