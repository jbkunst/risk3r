% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scorecard.R
\name{ctree_break_list}
\alias{ctree_break_list}
\title{Create a breaklist argument unsing ctree method}
\usage{
ctree_break_list(dt, y, ctrl = partykit::ctree_control(), save_trees = FALSE)
}
\arguments{
\item{dt}{A data frame using to create the bins}

\item{y}{Name of y variable.}

\item{ctrl}{A partykit::ctree_control instance}

\item{save_trees}{A logical indicating to return the trees objects or not}
}
\description{
Create a breaklist argument unsing ctree method
}
\examples{

\dontrun{

data(germancredit, package = "scorecard")

brks <- ctree_break_list(germancredit, "creditability", ctrl = partykit::ctree_control())

bins2 <- scorecard::woebin(germancredit, "creditability", breaks_list = brks)

bins2$duration.in.month

woebin_summary(bins2)

bins1 <- scorecard::woebin(germancredit, "creditability")

bins1$duration.in.month

woebin_summary(bins1)

library(dplyr)

full_join(
  woebin_summary(bins1) \%>\% select(variable, iv),
  woebin_summary(bins2) \%>\% select(variable, iv),
  by = "variable"
  ) \%>\%
  mutate( iv.x  > iv.y)

}

}
