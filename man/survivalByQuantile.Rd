% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survivalByQuantile.R
\name{survivalByQuantile}
\alias{survivalByQuantile}
\title{Compute cox model p-value under various threshold}
\usage{
survivalByQuantile(
  input.var,
  input.tax,
  surv.dat,
  percentiles = seq(0.01, 0.99, 0.01)
)
}
\arguments{
\item{input.var}{a character object specifying which subject survival analysis
is applying to.}

\item{input.tax}{data frame containing information about an object to be tested.
The sample order must agree with surv.dat's sample order}

\item{surv.dat}{a data frame containing all survival information. The sample
order must agree with input.dat's sample order}

\item{percentiles}{a list of numeric values specifying all of the percentiles
to be tested on}
}
\value{
The function will generate a data frame containing hazard ratio,
lower bound, upper bound, percentile, cutoff threshold, and p-value. This data
frame can then be manipulated for various plots
}
\description{
Compute cox model p-value under various threshold
}
\details{
The threshold used in this function is to categorize data into two groups
}
