% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive.R
\name{confidence_interval}
\alias{confidence_interval}
\title{Calculate confidence interval for the mean}
\usage{
confidence_interval(x, conf.level = 0.95)
}
\arguments{
\item{x}{A numeric vector of values.}

\item{conf.level}{The confidence level (default is 0.95).}
}
\value{
A numeric vector with the lower and upper bounds of the confidence interval.
}
\description{
This function calculates a confidence interval for the mean of a numeric vector.
}
\examples{
confidence_interval(rnorm(100))
confidence_interval(1:10, conf.level = 0.99)
}
