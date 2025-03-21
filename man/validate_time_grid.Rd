% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{validate_time_grid}
\alias{validate_time_grid}
\title{\code{validate_time_grid}}
\usage{
validate_time_grid(time_grid)
}
\arguments{
\item{time_grid}{(\code{numeric})\cr A vector of times which quantities will be
evaluated at.}
}
\description{
Validate that the provided time grid is:
\itemize{
\item finite
\item numeric
\item non-missing
\item sorted
\item unique
}
}
\keyword{internal}
