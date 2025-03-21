% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimLongitudinalSteinFojo.R
\docType{class}
\name{SimLongitudinalSteinFojo-class}
\alias{SimLongitudinalSteinFojo-class}
\alias{.SimLongitudinalSteinFojo}
\alias{SimLongitudinalSteinFojo}
\title{Simulate Longitudinal Data from a Stein-Fojo Model}
\usage{
SimLongitudinalSteinFojo(
  times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550)/365,
  sigma = 0.01,
  mu_s = log(c(0.6, 0.4)),
  mu_g = log(c(0.25, 0.35)),
  mu_b = log(60),
  omega_b = 0.2,
  omega_s = 0.2,
  omega_g = 0.2,
  link_dsld = 0,
  link_ttg = 0,
  link_identity = 0,
  link_growth = 0,
  link_shrinkage = 0,
  scaled_variance = TRUE
)
}
\arguments{
\item{times}{(\code{numeric})\cr the times to generate observations at.}

\item{sigma}{(\code{number})\cr the variance of the longitudinal values.}

\item{mu_s}{(\code{numeric})\cr the mean shrinkage rates for the two treatment arms.}

\item{mu_g}{(\code{numeric})\cr the mean growth rates for the two treatment arms.}

\item{mu_b}{(\code{numeric})\cr the mean baseline values for the two treatment arms.}

\item{omega_b}{(\code{number})\cr the baseline value standard deviation.}

\item{omega_s}{(\code{number})\cr the shrinkage rate standard deviation.}

\item{omega_g}{(\code{number})\cr the growth rate standard deviation.}

\item{link_dsld}{(\code{number})\cr the link coefficient for the derivative contribution.}

\item{link_ttg}{(\code{number})\cr the link coefficient for the time-to-growth contribution.}

\item{link_identity}{(\code{number})\cr the link coefficient for the SLD Identity contribution.}

\item{link_growth}{(\code{number})\cr the link coefficient for the log-growth parameter contribution.}

\item{link_shrinkage}{(\code{number})\cr the link coefficient for the log-shrinkage parameter contribution.}

\item{scaled_variance}{(\code{logical})\cr whether the variance should be scaled by the expected value
(see the "Statistical Specifications" vignette for more details)}
}
\description{
Simulate Longitudinal Data from a Stein-Fojo Model
}
\section{Slots}{

\describe{
\item{\code{sigma}}{(\code{numeric})\cr See arguments.}

\item{\code{mu_s}}{(\code{numeric})\cr See arguments.}

\item{\code{mu_g}}{(\code{numeric})\cr See arguments.}

\item{\code{mu_b}}{(\code{numeric})\cr See arguments.}

\item{\code{omega_b}}{(\code{numeric})\cr See arguments.}

\item{\code{omega_s}}{(\code{numeric})\cr See arguments.}

\item{\code{omega_g}}{(\code{numeric})\cr See arguments.}

\item{\code{link_dsld}}{(\code{numeric})\cr See arguments.}

\item{\code{link_ttg}}{(\code{numeric})\cr See arguments.}

\item{\code{link_identity}}{(\code{numeric})\cr See arguments.}

\item{\code{link_growth}}{(\code{numeric})\cr See arguments.}

\item{\code{link_shrinkage}}{(\code{numeric})\cr See arguments.}

\item{\code{scaled_variance}}{(\code{logical})\cr See arguments.}
}}

\seealso{
Other SimLongitudinal: 
\code{\link{SimLongitudinal-class}},
\code{\link{SimLongitudinalClaretBruno-class}},
\code{\link{SimLongitudinalGSF-class}},
\code{\link{SimLongitudinalRandomSlope-class}}
}
\concept{SimLongitudinal}
