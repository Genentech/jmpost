% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SurvivalLoglogistic.R
\docType{class}
\name{SurvivalLogLogistic-class}
\alias{SurvivalLogLogistic-class}
\alias{.SurvivalLogLogistic}
\alias{SurvivalLogLogistic}
\title{\code{SurvivalLogLogistic}}
\usage{
SurvivalLogLogistic(
  a = prior_lognormal(log(0.1), 5),
  b = prior_gamma(2, 5),
  beta = prior_normal(0, 5)
)
}
\arguments{
\item{a}{(\code{Prior})\cr Prior distribution for the scale parameter \code{a}.}

\item{b}{(\code{Prior})\cr Prior distribution for the shape parameter \code{b}.}

\item{beta}{(\code{Prior})\cr Prior distribution for covariates coefficients \code{beta}.}
}
\description{
This class extends the general \code{\link{SurvivalModel}} class for using the
log-logistic survival model.
}
